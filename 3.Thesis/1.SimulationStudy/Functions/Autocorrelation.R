# Compute the maximum auto-correlation at lag 1
# Used within imputation function

autocorr_function <- function(imp, maxit, m = 5, n.var = 4, moment = "mean"){
  
  # object for for loop
  ac_per_chain <- numeric(length = m)
  ac_per_var <- numeric(length = n.var)
  names(ac_per_var) <- attr(imp$chainMean, "dimnames")[[1]]
  
  # make suitable for convergence of mean and variance
  if (moment == "mean") {
    sims <- imp$chainMean
  } else if (moment == "variance") {
    sims <- imp$chainVar
  }
  
  # compute convergence diagnostic per variable
  for (v in 1:n.var) {
    for (chain in 1:m) {
      # ac[chain, v] <-
      ac_per_chain[chain] <-
        acf(sims[v, , chain],
          lag.max = 1,
          plot = F)$acf[2] * -2 
    }
    ac_per_var[v] <- mean(ac_per_chain)
}
# output
  return(ac_per_var)
}

# NB. This function can be adjusted to show the nr. of chains with significant auto-correlations,
# output will then be the fraction of imputation chains with significant autocorrelation at lag 1.
# To get this, add:
# # critical value significance 
# AC_lim <- qnorm((1 + .95) / 2) / sqrt(maxit)