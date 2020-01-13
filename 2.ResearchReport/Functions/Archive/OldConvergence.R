# Extract convergence diagnostic Rhat from mids object
# Used within imputation function
# Requires package 'magrittr', and the functions in Convergence_supplement.R

rhat_function <- function(imp, maxit, n.var = 4) {
  
  # objects with variables in columns
  rhat <- matrix(NA, nrow = 1, ncol = n.var)
  
  # compute converegnce diagnostic per variable
  for (v in 1:n.var) {
    split_rank <- imp$chainMean[v, , ] %>% split_chains() %>% z_scale()
    folded_split_rank <- imp$chainMean[v, , ] %>% fold_sims() %>% split_chains() %>% z_scale()
      # z_scale(split_chains(abs(imp$chainMean[v, , ] - median(imp$chainMean[v, , ]))))
    rhat_bulk <- get.rhat(split_rank, maxit = maxit)
    rhat_tail <- get.rhat(folded_split_rank, maxit = maxit)
    rhat[v] <- max(rhat_bulk, rhat_tail)
  }
  
  # output
  max(rhat)
}
