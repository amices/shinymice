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
  
  # quit function if there are not enough iterations
  t <- dim(x)[1]
  if (is.null(t)) {
    r.hat <- data.frame(iteration = 1, r.hat = NA)
  }
  else {
  r.hat <- map_dfr(2:t, function(it){
  # compute r hat in all ways described by Vehtari et al. (2019)
  rhat_original <- x[1:it,] %>% get.rhat()
  rhat_bulk <- x[1:it,] %>% split_chains() %>% z_scale() %>% get.rhat()
  rhat_tail <- x[1:it,] %>% fold_sims() %>% split_chains() %>% z_scale() %>% get.rhat()
  max.rhat <- max(rhat_original, rhat_bulk, rhat_tail) %>% data.frame() %>% set_names("r.hat") 
  }) %>% rbind(NA,.) %>% cbind(iteration = 1:t, .)
  }
  # output
  return(r.hat)
}

