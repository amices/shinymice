# Rhat function supplement
# requires library(purrr) and dplyr/magrittr

# function for Rhat for two or more imputation chains
rhat_adapted <- function(x) {
  # input: object with theta values (rows are iterations, columns are imputations)
  # output: Rhat across imputations
  
  # make input ready for mapping
  if (is.data.frame(x)) {
    x <- as.matrix(x)
  }
  
  # quit function if there are not enough observations
  if (length(x) < 2){
    return(NA)
  }
  
  # compute r hat in all ways described by Vehtari et al. (2019)
  rhat_original <- x %>% get.rhat()
  rhat_bulk <- x %>% split_chains() %>% z_scale() %>% get.rhat()
  rhat_tail <- x %>% fold_sims() %>% split_chains() %>% z_scale() %>% get.rhat()
  max.rhat <- max(rhat_original, rhat_bulk, rhat_tail) %>% set_names()
  
  # output
  return(data.frame(max.rhat))
}