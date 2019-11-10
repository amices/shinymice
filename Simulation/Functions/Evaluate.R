# Extract aggregated simulation results by averaging over runs

evaluate.sim <- function(sims, true = 2) {
  
  # compute average over simulations
  RB <- rowMeans(res[, , 1]) - true
  PB <- 100 * abs((rowMeans(res[, , 1]) - true) / true)
  CR <- rowMeans(res[, , 2] < true & true < res[, , 3])
  AW <- rowMeans(res[, , 3] - res[, , 2])
  Rh <- rowMeans(res[, , 4])
  AC <- rowMeans(res[, , 5])

  # output
  return(data.frame(RB, PB, CR, AW, Rh, AC))
}