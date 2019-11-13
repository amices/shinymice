# Extract convergence diagnostic Rhat from mids object
# Used within imputation function

get.rhat <- function(imp, maxit, n.var = 4) {
  
  # objects with variables in columns
  var_between <- var_within <- rhat <- matrix(NA, nrow = 1, ncol = n.var)
  
  # compute converegnce diagnostic per variable
  for (v in 1:n.var) {
    var_between[v] <-
      maxit * var(apply(imp$chainMean[v, , ], 2, mean))
    var_within[v] <- mean(apply(imp$chainMean[v, , ], 2, var))
    rhat[v] <-
      sqrt((var_between[v] / var_within[v] + maxit - 1) / maxit)
  }
  
  # # possibly integrate rstan functions for folded, spplit-half rank-normalized Rhat
  # z_scale <- function(x) {
  #   S <- length(x)
  #   r <- rank(x, ties.method = 'average')
  #   z <- qnorm((r - 1 / 2) / S)
  #   if (!is.null(dim(x))) {
  #     # output should have the input dimension
  #     z <- array(z, dim = dim(x), dimnames = dimnames(x))
  #   }
  #   z
  # }
  
  # split_chains <- function(sims) {
  #   # split Markov chains
  #   # Args:
  #   #   sims: a 2D array of samples (# iter * # chains)
  #   if (is.vector(sims)) {
  #     dim(sims) <- c(length(sims), 1)
  #   }
  #   niter <- dim(sims)[1]
  #   if (niter == 1L) return(sims)
  #   half <- niter / 2
  #   cbind(sims[1:floor(half), ], sims[ceiling(half + 1):niter, ])
  # }
  
  # rhat <- function(sims) {
  #   bulk_rhat <- rhat_rfun(z_scale(split_chains(sims)))
  #   sims_folded <- abs(sims - median(sims))
  #   tail_rhat <- rhat_rfun(z_scale(split_chains(sims_folded)))
  #   max(bulk_rhat, tail_rhat)
  # }
  
  # output
  max(rhat)
}