# Extract convergence diagnostic from mids object
# Used within imputation function
# Requires package 'magrittr'/'dplyr', and the functions in Convergence_supplement.R

within_function <- function(imp, maxit, n.var = 4, moment = "mean") {
  # extract convergence diagnostic from mids object
  
  # object to store output for loop
  within <- numeric(length = n.var)
  names(within) <- attr(imp$chainMean, "dimnames")[[1]]
  
  # make suitable for convergence of mean and variance
  if (moment == "mean") {
    sims <- imp$chainMean
  } else if (moment == "variance") {
    sims <- imp$chainVar
  }
  
  # compute converegnce diagnostic per variable v
  for (v in 1:n.var) {
    within[v] <- mean(apply(sims[v, , ], 2, var))
  }
  
  # output
  return(within)
}
