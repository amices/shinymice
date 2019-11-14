# compute the autocorrelation at lag 1
# Used within imputation function

autocorr_function <- function(imp, maxit, m = 5, n.var = 4){
  
  # objects with variables in columns
  ac <- matrix(NA, nrow = m, ncol = n.var)
  #ac.prop <- matrix(NA, nrow = 1, ncol = n.var)
  
  # critical value significance 
  AC_lim <- qnorm((1 + .95) / 2) / sqrt(maxit)
  
  # compute converegnce diagnostic per variable
  for (v in 1:n.var) {
    for (chain in 1:m) {
      ac[chain, v] <-
        acf(imp$chainMean[v, , chain],
          lag.max = 1,
          plot = F)$acf[-1] #> AC_lim
    #ac.prop <- colMeans(ac)
  }
}
# output
  ac[which.max(abs(ac))] # fraction of imputation chains with significant autocorrelation at lag 1
}
