## FOR TESTING ONLY -- if called outside of PrepData.R
if (FALSE) {
  setwd("StrategyPaperAnalysis")
  source("PrepData.R")
}
###

#
# PrepSeatScores
#
# Uses election return info to calculate expected seat scores to plans
#
# Depends on: PrepData.R  -- This must be run first
# Returns: planscores.df

source("plandiff.R")


# use a local blocks to keep environment neat, only the objects defined outside will remain after run
## BEGIN LOCAL BLOCK
planProcessing.df <- local({ 
  planProcessing.df<-NULL
## 
## Data cleaning functions 
##

# function to check and merge plans
standardizePlan <- function(plan,edon,votedf) {
  #ow <- options("warn") 
  #oe <- options("error")
  #on.exit({options(ow); options(oe)})
  #options("warn"=2)
  #options("error"=recover)
  if(is.null(plan)) { return (NULL)}
  plan %<>% filter(!is.na(district) & district>0)
  if (!all((plan %>% group_by(seccion) %>% summarise(n()) %>% pull()) == 1)) {
    warning("duplicate seccions in plan")
  }
  # address issue related to edon 21, clv2 PRI submission which is incorrectly entered
  plan %<>% group_by(seccion) %>% slice_min(district,with_ties=FALSE) %>% ungroup()
  # filter unnecessary edon
  edont <- edon
  votedf %<>% filter(edon==edont) 
  plan %<>% left_join(votedf, by = c("seccion"), relationship="one-to-many")
  #plan %<>% mutate(edon=edon)
  #plan %<>% left_join(votedf, by = c("seccion", "edon"), relationship="one-to-many")
  return (plan)
}

#debug(standardizePlan)

###
### Create a tibble of plans with integrated , data which can then be used for various evaluations
###

# generate table of planids and plans -- planids are repeated across actors, so select one, 
# but make sure it has a plan attached -- some of the entries repeatedly proposed by multiple parties 
# were not recorded each time in the INE systems
planProcessing.df <- propfull.df %>% filter(!is.null(plan)) %>% group_by(planid) %>% summarize(plan=head(plan,1), edon=head(edon,1))


###
### Extract 2004 status quo plan from raw- file and 
###

fed2004.tb <- read_csv("mxDistritos-data/raw-seccion-2015.csv",
                       col_types=cols( .default = col_double()))
fed2004.tb %<>% 
  select(edon,disn,seccion) %>% 
  rename(district=disn) %>%
  group_by(edon) %>%
  summarise(plan = list(tibble(seccion,district))) %>%
  mutate(planid=paste("base",edon,"2004",sep="-"))

planProcessing.df %<>% bind_rows(fed2004.tb)

pri_paths <- dir("mxDistritos-data/academic/",pattern="\\.csv$",full.names=TRUE)

# Load Magar Gerrymanders
pri_gerry.ls <-map(pri_paths,  read_csv, col_names=c("seccion_char","district","seats"), col_types=cols_only(seccion_char="c",district="i"))
pri_gerry.tbl <- tibble(plan=pri_gerry.ls,
                        planid=paste(sep="-","academic","pri",
                                     str_match(sapply(str_split(pri_paths,"/"),tail,n=1),"(.*?)\\.csv")[,2]),
                        edon=15)

pri_gerry.tbl %<>% unnest(plan) %>% 
    mutate(seccion= as.integer(str_sub(seccion_char,start=-4))) %>%
    select(-seccion_char) %>% nest(plan=c(seccion,district))

planProcessing.df %<>% bind_rows(pri_gerry.tbl,.)


planProcessing.df %>% rowwise() %>% ## RETURN VALUE FROM LOCAL BLOCK
    mutate( plan_2015 = list(standardizePlan(plan,edon,votes.2015.df)),
            plan_2018 = list(standardizePlan(plan,edon,votes.2018.df)),
            plan_2012 = list(standardizePlan(plan,edon,votes.2012.df)),
            
    ) %>% ungroup() 
}) 

vars.srclist <- c("plan_2015","plan_2018","plan_2012")

## BEGIN LOCAL BLOCK
planProcessing.df<- local({ 
  
## Calculate functions -- used to generate district summary tables based on standardized plans
## these summary tables are then used to calculate any scores for plans
##
## USAGE NOTE: in mutate, use rowwise first
##             and  if the result is a data frame, wrap it in list()
##
##  mydata %>% rowwise() %>% mutate( answer = list(calcXXXX(plan_2015))  


# calcDistrictTotals 
#      INPUT: splan - plan produced by standardizedPlan
#      VALUE: tibble of district-level totals
calcDistrictTotals <- function (splan) {
  if(is.null(splan)) { return (NULL)}
  
  votes_total <- splan %>%   group_by(district) %>% summarize(votes_total=sum(votes,na.rm=TRUE))

 splan %<>% 
    group_by(seccion) %>% slice_head(n=1) %>% # lisnom,efec are duplicated across rows per actor
    group_by(district) %>%
    summarise(across(c(efec,lisnom), list(total = ~sum(.x,na.rm=TRUE))))
    splan %>% left_join(votes_total,  by = "district")
}

# calcDistrictTotals 
#      INPUT: splan - plan produced by standardizedPlan
#      VALUE: tibble of district-level totals
calcActorDistrictTotals<- function (splan) {
  if(is.null(splan)) { return (NULL)}
  
  splan %<>%   group_by(district,actor) %>% summarize(votes_total=sum(votes,na.rm=TRUE))
  splan %<>% group_by(district) %>% mutate(votes_share=votes_total/sum(votes_total,na.rm=TRUE))
  ungroup(splan)
}

# use the calc functions to extend planProcessing.df
planProcessing.df %>% # RETURN Value for local block
  rowwise() %>%
  mutate( across(
    c({{ vars.srclist }}) , 
    list( districts = ~list(calcDistrictTotals(.x)),
          actors = ~list(calcActorDistrictTotals(.x)))
  ))
}) ## END LOCAL BLOCK


###
### Add plan scores
###
## BEGIN LOCAL BLOCK
planProcessing.df<- local({ 
  
## scoring functions -- used to generate district plan scores based on district and/ districtActor summary tables

# scoreMaxPop 
#      INPUT: district level totals
#      VALUE: score representing maximum relative deviation from population equality
scoreMaxPopDev <- function(district) {
  if(is.null(district)) { return (NA)}
  district %>% 
    ungroup() %>%
    summarise(ideal = mean(lisnom_total),
              score = max(abs(lisnom_total-ideal)/ideal)
    ) %>% 
    pull(score) 
}

#scoreSeats 
#      INPUT: actor-district level totals
#      VALUE: tibble of scoring statistics

scoreSeats <- function(actordist) {
  if(is.null(actordist)) { return (NULL)}
  
  actordist %>% 
    group_by(district) %>% 
    slice_max(votes_share,n=2) %>% 
    summarize(votes_share=list(votes_share),actor=list(actor)) %>%
    rowwise() %>% 
    mutate(win_actor=actor[1], win_margin=votes_share[1]-votes_share[2] )
}

scoreComp <- function(x,threshold=.02){
  if(is.null(x)) { return (NA)}
  
  x %>% ungroup() %>%
    summarize(score=sum(win_margin<threshold)) %>%
    pull(score)
}

scoreMuniSplits <-function(x) {
  if (is.null(x)) {return(NA)}
  if (length(x)==1)  {x <- pull(x)}
  
  counts<- x %>% 
    group_by(district,inegi) %>%
    filter(!is.na(inegi)) %>%
    slice_head(n=1) %>% 
    ungroup() %>% 
    select(district,inegi) %>% 
    count(inegi) %>%
    pull(n)
  
  sum(counts-1)
}

scoreWins <- function(x){
  if(is.null(x)) { return (NULL)}
  
  x %>% ungroup() %>%
    count(win_actor) 
}

vars.districts <- vars.srclist %>% paste("_districts",sep="")
vars.actors <- vars.srclist %>% paste("_actors",sep="")


planProcessing.df  %<>% rowwise() %>% mutate( across( 
  c({{ vars.srclist }}), 
  list(
    muniSplits = ~scoreMuniSplits(.x)
  ))) 


planProcessing.df  %<>% rowwise() %>% mutate( across( 
  c({{ vars.districts }}), 
  list(
            maxPopDev = ~scoreMaxPopDev(.x)
      ))) 

planProcessing.df  %<>% rowwise() %>% mutate( across( 
  c({{ vars.actors }}), 
  list(
    winMargins = ~list(scoreSeats(.x))
  ))) 

vars.winmargins <- (vars.srclist %>% paste("_actors_winMargins",sep=""))
planProcessing.df  %<>% rowwise() %>% mutate( across( 
  c({{ vars.winmargins }}), 
  list(
    compCount2 = ~scoreComp(.x,.02),
    compCount4 = ~scoreComp(.x,.04),
    compCount8 = ~scoreComp(.x,.08),
    
    actorWins = ~list(scoreWins(.x))
   # muniDominate = ~list(scoreMuniDominate(.x))
  ))) 

  planProcessing.df  %<>% rowwise() %>% mutate( across( 
  c({{ vars.srclist }}),
  list(
    muniDominate = ~list(scoreMuniDominate(.x))
  ))) 

 planProcessing.df
}) ## END LOCAL BLOCK


###
### Plan difference analysis
###


### diffBaseline
###
### takes a plan,edon from \ and returns differences from baseline
### 2004 plan
local ({
  baseline.tib <- planProcessing.df %>% filter(str_starts(planid,"base")) %>% select(plan,edon) 
  diffBaseline_<-function(plan,i) {
    bplan <- baseline.tib %>% filter(edon==i) %>% pull(plan)
    planDiff(plan,bplan[[1]])
  }
  diffBaseline_}) -> diffBaseline

planProcessing.df %<>% 
  rowwise() %>%
  mutate(base_diffs=list(diffBaseline(plan,edon))) %>%
  ungroup()

planProcessing.df %<>%  rowwise() %>% mutate(
  plansize=nrow(plan),
  base_diff_sum=sum(base_diffs$size),
  base_diff_delta = head(base_diffs$deltadist,n=1),
  base_diff_percent = base_diff_sum/plansize,
  ndists = length(na.omit(unique(plan$district)))
  )




rm(el,vars.srclist)



#Done:
# Ghostplans -- INE system provides plan even if it was not submitted. Plan delivered is often the
#    prior winning plan -- which is presumably the default for counterproposals.

#TODO: 
# - academic- districts and 13.74065-2-2013	have wrong number of dists
# - missing seccions in maps
# DONE: plan hash matches on different plans- mark missing
# - replace missing plans from separate source
# DONE: revealed prefs
# DONE: municipality splits
# DONE: municipality homongeneity
