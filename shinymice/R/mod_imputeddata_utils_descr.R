# descriptives for imputed data
imp_descr <- function(imp){
  tab <- imp %>% mice::complete("all") %>% 
    purrr::map_df(., ~{psych::describe(.)[,c("n", "mean", "sd", "min", "max", "median")] %>% 
    cbind(., variable = rownames(.))}) %>% 
    aggregate(.~variable, data = ., FUN = "mean") %>% 
    cbind(., n_imputed = as.integer(imp$nmis))
  tab[,2] <- as.integer(tab[,2])
  return(tab)
}