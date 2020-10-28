# descriptive statistics for incomplete data
library(dplyr)
descr <- function(d){
  tab <- psych::describe(d)[,c("n", "mean", "sd", "min", "max", "median")] %>% 
    cbind(., missing = nrow(d) - .$n)
  return(tab)
}