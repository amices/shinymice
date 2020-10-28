# descriptive statistics for incomplete data
library(dplyr)
mis_descr <- function(d){
  tab <- psych::describe(d)[,c("n", "mean", "sd", "min", "max", "median")] %>% 
    cbind(variable = rownames(.), ., n_missing = nrow(d) - .$n)
  return(tab)
}