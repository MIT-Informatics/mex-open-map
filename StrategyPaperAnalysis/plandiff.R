### planDiff 
###
### takes two plan tibbles defining block equivalencies
### returns a tibble of district difference

planDiff <- function(plan1,plan2) {
  
  # NOTE: Filtering unassigned blocks 
  planDis_1.tbl <- plan1 %>% 
    group_by(district) %>% 
    summarize(seclist=list(seccion)) %>%
    filter (!is.na(district))
  
  planDis_2.tbl <- plan2 %>%
    group_by(district) %>% 
    summarize(seclist=list(seccion)) %>%
    filter (!is.na(district))
  
  
  distDiff <- function(i,j) {
    setdiff(planDis_1.tbl[i,"seclist"][[1]][[1]],planDis_2.tbl[j,"seclist"][[1]][[1]])
  }
  
  # compute all differences between districts for potential matches
  allPlanDiffs.tbl <- expand.grid(i=1:nrow(planDis_1.tbl),j=1:nrow(planDis_2.tbl)) %>% 
    rowwise() %>%
    mutate(diff=list(distDiff(i,j)), size=length(diff)) %>% 
    ungroup() %>%
    arrange(size)
  
  planMatch.tbl <- tibble()
  # iteratively select best matches from the top, eliminate matched district ids 
  
  for (i in 1:(nrow(planDis_1.tbl))) {
    if (nrow(allPlanDiffs.tbl)==0) {
      # if plan 1 has more districts than plan2
      break
    }
    planMatch.tbl %<>% bind_rows(allPlanDiffs.tbl[1,])
    matched_i <- allPlanDiffs.tbl[[1,"i"]]
    matched_j <- allPlanDiffs.tbl[[1,"j"]]
    allPlanDiffs.tbl %<>% 
      filter(i != matched_i, j != matched_j)
  }
  # if plan 1 has more districts than plan2
  d_unmatched <- setdiff(unique(planDis_1.tbl %>% pull("district")),
                         planMatch.tbl %>% pull("i"))
  
  
  unmatched.tbl <- planDis_1.tbl %>% filter(district %in% d_unmatched) %>%
    rowwise() %>%
    mutate(j=NA, size=length(seclist)) %>% 
    rename(i=district,diff=seclist)
  
  planMatch.tbl %<>% bind_rows(unmatched.tbl)
  planMatch.tbl %<>% 
    mutate(deltadist = nrow(planDis_1.tbl) - nrow(planDis_2.tbl)) %>%
    arrange(i)
  planMatch.tbl
}

scoreMuniDominate <-function(x) {
  if (is.null(x)) {return(NA)}
  if (length(x)==1)  {x <- pull(x)}
  x %<>% rowwise() %>% filter(!is.na(actor)) %>% ungroup()
  
  splitinegi <- x %>% 
    group_by(edon,district,inegi) %>%
    filter(!is.na(inegi)) %>%
    slice_head(n=1) %>% 
    ungroup() %>% 
    select(edon,district,inegi) %>% 
    count(edon,inegi) %>%
    filter(n>1) %>%
    select(-n)
  
  distotals <- x %>%
    group_by(edon,district) %>%
    summarize(totalvote=sum(votes,na.rm=TRUE)) %>%
    ungroup()
  
  munidominate <- x %>%
    anti_join(splitinegi,by=c("edon","inegi")) %>%
    group_by(edon,district,inegi,actor) %>%
    summarize(totalactor=sum(votes,na.rm=TRUE)) %>%
    group_by(edon,district,inegi) %>%
    summarize(maxtotalactor=max(totalactor,na.rm=TRUE),topactor=actor[which(totalactor==maxtotalactor)]) %>%
    group_by(edon,district) %>%
    summarize(maxinfluence=max(maxtotalactor,na.rm=TRUE),maxactor=topactor[which(maxtotalactor==maxinfluence)]) %>%
    ungroup()
  
  munidominate %<>% right_join(by=c("edon","district"),distotals) %>% mutate(prop_influence=maxinfluence/totalvote, prop_influence=replace_na(prop_influence,0) )
  munidominate
  
}