# descriptives for imputed data
library(dplyr)
imp_descr <- function(imp){
  tab <- imp %>% mice::complete("all") %>% 
    purrr::map_df(., ~{psych::describe(.)[,c("n", "mean", "sd", "min", "max", "median")] %>% 
    cbind(., variable = rownames(.))}) %>% 
    aggregate(.~variable, data = ., FUN = "mean") %>% 
    cbind(., n_imputed = imp$nmis)
  return(tab)
}