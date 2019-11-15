# Extract aggregated simulation results by averaging over runs

evaluate.sim <- function(sims, true_effect = 2) {
  
  # compute average over simulations
  Bias <- rowMeans(res[, , 1]) - true_effect #average bias of estimated regression coefficient
  Emp.SE <- apply(res[, , 1], 1, sd) #empirical standard error of estimated regression coefficient
  CI.width <- rowMeans(res[, , 4] - res[, , 3]) #width of 95% confidence interval
  Cov.rate <- rowMeans(res[, , 3] <= true_effect & true_effect <= res[, , 4]) #coverage probability
  R.hat <- rowMeans(res[, , 5]) #maximum potential scale reduction factor
  Autocorr <- rowMeans(res[, , 6]) #maximum auto-correlation at lag 1

  # output
  return(data.frame(Bias, Emp.SE, CI.width, Cov.rate, R.hat, Autocorr))
}

# This function can be adjusted to also compute:
  # Bias_MCSE <- apply((res[ , , 1] - true), 2, sd)
  # Perc.bias <- 100 * abs((rowMeans(res[, , 1]) - true) / true)
  # Pooled.SE <- rowMeans(res[, , 2])
  # Cov_MCSE <- sqrt((Cov.rate*(1 - Cov.rate))/rowMeans(row(res[,,1])))
