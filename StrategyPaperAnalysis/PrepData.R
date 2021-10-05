### PrepData
###
### Ingest and structure the core data for both Strategy, Transparency, and Algorithmic Analyses
###

## Libraries 
require(tidyverse)
require(magrittr)
require(tidylog)

## Party Type Codes

#Major - PRD, PAN, PRI / Minor - PT, PVEM, ES, MORENA, MC, PNA / Admin - CLV, Algorithm, Junta, INE, derfe / Other - PRD51
actors.df <- structure(list(actor = structure(c(2L, 7L, 9L, 10L, 11L, 13L, 
                                                 14L, 15L, 1L, 6L, 12L, 5L, 4L, 8L, 3L), .Label = c("Algorithm", 
                                                                                                    "CLV", "derfe", "ES", "INE", "Junta", "MC", "MORENA", "PAN", 
                                                                                                    "PNA", "PRD", "PRD51", "PRI", "PT", "PVEM"), class = "factor"), 
                             actortype = structure(c(1L, 3L, 2L, 3L, 2L, 2L, 3L, 3L, 1L, 
                                                     1L, 4L, 1L, 3L, 3L, 1L), .Label = c("admin", "major", "minor", 
                                                                                         "other"), class = "factor")), class = "data.frame", row.names = c(NA, 
                                                                                                                                                           -15L))                                                                                                                                                     

## Proposal events database: proposal.df
#setwd("StrategyPaperAnalysis")
source("Normalize.R") # reads in Pre-Normalized IFE tables 
## Ruling party by year and state: grule.df , controlByWindow() helper function
source("RulingParty.R") # Bring in ruling party db
## Integrate proposed plans: profull.df
source("integrateMxdistritos.R")
rm(proposals.df)
propfull.df %<>% left_join(actors.df)
propfull.df %<>% rowwise() %>% mutate(rscore=(round(SCORE,digits=5))) # prevent plans not matching across stages because of rounding error
propfull.df %<>% rowwise() %>% mutate(planid=ifelse(is.na(rscore),NA,paste(rscore,edon,year,sep="-"))) # for network analysis


## Supports cleaning, merging w/ raw-seccion-* files
# Make function public because we'll use it in the municipality analysis

clean_vraw<-function(x){
  
  # create missing coalition columns ,
  for (cmiss in setdiff(c("morenac","pric","panc","prdc","pan", "pri", "prd", "pvem", "pt", "mc", "pna", "morena", "pes"), x %>% names())) {
    x %<>% mutate({{ cmiss }} := 0)
  }
  
  # strip subtotals
  x %<>% filter(!is.na(disn))
  
  #Adds votes received by coalition groups to the major party with which the coalition is associated.
  x %<>%
    mutate(pan = pan + panc) %>%
    mutate(prd = prd + prdc) %>%
    mutate(pri = pri + pric) %>%
    mutate(morena = morena + morenac) %>%
    rename(es = pes) %>% pivot_longer(
      .,
      cols = c("pan", "pri", "prd", "pvem", "pt", "mc", "pna", "morena", "es"),
      names_to = "actor",
      values_to = "votes"
    ) %>% #Data is in wide format, converts the data to long format
    mutate(actor = toupper(actor)) %>%  #Converts the actor names to uppercase to match with actor names in subsequent dataframes
    select(edon, seccion, actor, votes, efec, lisnom, inegi,disn) %>%
    rename(disn_adopt = disn)  
}

# read  plan, and merge into a new plan column
votes.2012.df <- read_csv("mxDistritos-data/raw-seccion-2012.csv", 
                          col_types=cols( .default = col_double()))
votes.2015.df <- read_csv("mxDistritos-data/raw-seccion-2015.csv", 
                          col_types=cols( .default = col_double()))
votes.2018.df <- read_csv("mxDistritos-data/raw-seccion-2018.csv",
                          col_types=cols( .default = col_double()))
votes.2018.df %<>%  clean_vraw()
votes.2015.df %<>%  clean_vraw()
votes.2012.df %<>%  clean_vraw()


planHash <- function(x) {
  if (is.null(x)) return("")
  if(length(x)==1) {
    x==x[[1]]
  }
  x %>% 
    group_by(district) %>% 
    summarize(g=list(sort(seccion)),h=head(unlist(g),n=1)) %>%
    ungroup() %>%
    arrange(h) %>%
    select(g) %>% 
    rlang::hash()
}


propfull.df %<>%  rowwise() %>% mutate(
  planhash=planHash(plan)
)

# INE did not keep its electronic system up to date  -- especially as it modified plans
# We can tell there is a mismatch when a new proposal receives a different score, but the
# plan-file in the system is identical to a previous plan (which is impossible). We mark the
# later plans that don't validate with the earlier plans as as missing. 

propfull.df %>%
  ungroup() %>%
  mutate(row=row_number()) %>%
  select(planid, planhash, edon, actor, stage, year,row) %>%
  filter(!is.na(planid), planhash !="") %>%
  arrange(planhash,year,edon,stage,actor) %>%
  group_by(planhash) %>% 
  filter(n()>1) %>% 
  filter(length(unique(planid))>1) %>% # this portion identified groups with mismatches
  mutate(matched = (planid == planid[1])) %>% 
  filter(!matched) %>%
  select(-matched) %>%
  ungroup() -> missing_by_match_plans.tib

reprows <- missing_by_match_plans.tib %>% filter(stage==5,year==2017) %>% pull(row)
badrows <-  setdiff(missing_by_match_plans.tib  %>% pull(row),reprows)

# replace bad plans from final stage in 2017 with known accepted plans
reprows <- missing_by_match_plans.tib %>% filter(stage==5,year==2017) %>% pull(row)
for (i in reprows) {
  e <- propfull.df[i,"edon"] %>% pull()
  p <- votes.2018.df %>% 
    filter(edon==e) %>% 
    group_by(seccion) %>%
    slice_head(n=1) %>% 
    select(seccion,disn_adopt) %>% 
    rename(district=disn_adopt)%>%
    ungroup
  
  propfull.df[[i,"plan"]] <- list(p)
}
rm(i,e,p)
propfull.df[badrows,"plan"] <- NA
