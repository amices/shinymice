# Extract aggregated simulation results by averaging over runs

evaluate.sim <- function(sims, true_effect) {
  
  # compute average over simulations
  # out %>% .[[1]] %>% .[[1]] %>% .[["est"]] # where the first 1 is nsim, the second 1 is niter
  # out %>% map("1") %>% .[[1]] %>% .[["est"]]
  Bias <- rowMeans(res[, , 1]) - true_effect #average bias of estimated regression coefficient
  Emp.SE <- sapply(out[["est"]], sapply, mean) #empirical standard error of estimated regression coefficient
  CI.width <- rowMeans(res[, , 4] - res[, , 3]) #width of 95% confidence interval
  Cov.rate <- rowMeans(res[, , 3] <= true_effect & true_effect <= res[, , 4]) #coverage probability
  R.hat <- rowMeans(res[, , 5]) #maximum potential scale reduction factor
  R.var <- rowMeans(res[, , 6])
  Autocorr <- rowMeans(res[, , 7]) #maximum auto-correlation at lag 1
  AC.var <- rowMeans(res[, , 8])

  # output
  return(data.frame(Bias, Emp.SE, CI.width, Cov.rate, R.hat, R.var, Autocorr, AC.var))
}

# This function can be adjusted to also compute:
  # Bias_MCSE <- apply((res[ , , 1] - true), 2, sd)
  # Perc.bias <- 100 * abs((rowMeans(res[, , 1]) - true) / true)
  # Pooled.SE <- rowMeans(res[, , 2])
  # Cov_MCSE <- sqrt((Cov.rate*(1 - Cov.rate))/rowMeans(row(res[,,1])))
