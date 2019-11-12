# Extract aggregated simulation results by averaging over runs

evaluate.sim <- function(sims, true = 2) {
  
  # compute average over simulations
  Bias <- rowMeans(res[, , 1]) - true
  # Perc.bias <- 100 * abs((rowMeans(res[, , 1]) - true) / true)
  Emp.SE <- rowMeans(res[, , 2])
  CI.width <- rowMeans(res[, , 4] - res[, , 3])
  Cov.rate <- rowMeans(res[, , 3] <= true & true <= res[, , 4])
  # MCSE <- sqrt((Cov.rate*(1 - Cov.rate))/rowMeans(row(res[,,1])))
  R.hat <- rowMeans(res[, , 5])
  Autocorr <- rowMeans(res[, , 6])

  # output
  return(data.frame(Bias, Emp.SE, CI.width, Cov.rate, R.hat, Autocorr))
}