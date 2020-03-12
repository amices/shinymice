# Extract convergence diagnostic Rhat from mids object
# Used within imputation function
# Requires package 'magrittr'/'dplyr', and the functions in Convergence_supplement.R

# Creating a conflict because I want to keep the master files of this folder, does this work?

rhat_function <- function(imp, maxit, n.var = 4, moment = "mean") {
  # extract convergence diagnostic Rhat from mids object
  
  # object to store output for loop
  rhat <- numeric(length = n.var)
  names(rhat) <- attr(imp$chainMean, "dimnames")[[1]]
  
  # make suitable for convergence of mean and variance
  if (moment == "mean") {
    sims <- imp$chainMean
  } else if (moment == "variance") {
    sims <- imp$chainVar
  }
  
  # compute converegnce diagnostic per variable v
  for (v in 1:n.var) {
    rhat_bulk <-
      sims[v, , ] %>% split_chains() %>% z_scale() %>% get.rhat(maxit = maxit)
    rhat_tail <-
      sims[v, , ] %>% fold_sims() %>% split_chains() %>% z_scale() %>% get.rhat(maxit = maxit)
    rhat[v] <- max(rhat_bulk, rhat_tail)
  }
  
  # output
  return(rhat)
}
