# simulate imputations with varying niter
# requires the package 'mice'

# simulation function
simulate.function <- function(data, n.iter, n.sim) {
  # set.seed(123)
  pb <- txtProgressBar(min = 0, max = n.iter, style = 3)
  #objects to store sims
  OUT <- out <- state <- list()
  # loop over all possible nrs of iterations
  for (i in 1:n.iter) {
    # repeat each analysis nsim times
    for (j in 1:n.sim) {
      state[i,j] <- .Random.seed
      # for each iterarion value, store each nsim 'repeatings' in lower case out
      out[[j]] <-
        mice(
          data = data,
          m = 3,
          method = "norm",
          maxit = i,
          print = F,
          seed = .Random.seed
        )
    }
    # for each iteration value, store the combined nsim simulations together
    OUT[[i]] <- out
    setTxtProgressBar(pb, i)
  }
  close(pb)
  return(OUT)
}
