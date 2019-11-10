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
  
  # output
  max(rhat)
}