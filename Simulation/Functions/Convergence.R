# Extract convergence diagnostic Rhat from mids object
# Used within imputation function
# Requires package 'magrittr'/'dplyr', and the functions in Convergence_supplement.R

rhat_function <- function(imp, maxit, n.var = 4) {
  # extract convergence diagnostic Rhat from mids object
  
  # object to store output for loop
  rhat <- matrix(NA, nrow = 1, ncol = n.var)
  
  # compute converegnce diagnostic per variable v
  for (v in 1:n.var) {
    rhat_bulk <-
      imp$chainMean[v, , ] %>% split_chains() %>% z_scale() %>% get.rhat(maxit = maxit)
    rhat_tail <-
      imp$chainMean[v, , ] %>% fold_sims() %>% split_chains() %>% z_scale() %>% get.rhat(maxit = maxit)
    rhat[v] <- max(rhat_bulk, rhat_tail)
  }
  
  # output
  max(rhat)
}
