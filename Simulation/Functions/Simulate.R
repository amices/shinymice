# simulate imputations with varying niter
# requires the package 'mice'

# simulation function
simulate.function <- function(data, n.iter, n.sim) {
  # set.seed(123)
  pb <- txtProgressBar(min = 0, max = n.iter, style = 3)
  z_value <- qnorm(.025,lower.tail = F)
  m <- 5
  #objects to store sims
  temp <- out <- OUT <- list()
  # objects to store converegnce diagnostics
  r.hat <- matrix(NA, nrow = n.iter, ncol = n.sim)
  convergence <- matrix(NA, n.iter, ncol = 4)
  # loop over all possible nrs of iterations
  for (i in 1:n.iter) {
    # repeat each analysis nsim times
    for (j in 1:n.sim) {
      # for each iterarion value, store each nsim 'repeatings' in lower case out
      temp <-
        mice(
          data = data,
          method = "norm",
          maxit = i,
          print = F
        )
      # extract r hat
      if (i < 2) {r.hat[i, j] <- NA}
      else {r.hat[i, j] <-
        max(as.numeric(Rhat.mice(temp)$Rhat.M.imp))}
      
      # apply analysis and extract est.
      temp <- pool(lm.mids(Y ~ X + Z1 + Z2, temp)) 
      
      # extract diagnostics
      out[[i]] <- diagnostics.function(mipo_object = temp, true_effect = true_effect, z_value = z_value)
    }
    # for each iteration value, store the combined nsim simulations together
    setTxtProgressBar(pb, i)
    OUT[[j]] <- out
  }
  
  # prepare for output
  convergence[, 1] <- rowMeans(r.hat, na.rm = T)
  # convergence[, 2] <- rowMeans(bias)
  # convergence[, 2] <- rowMeans(r.hat < 1.2, na.rm = T)
  # convergence[, 3] <- rowMeans(r.hat < 1.1, na.rm = T)
  # convergence[, 4] <- rowMeans(r.hat < 1.01, na.rm = T)
  # colnames(convergence) <- c("Rhat", "Rhat_lenient", "Rhat_mid", "Rhat_stringent")
  
  # end function
  close(pb)
  return(OUT)
}

# # simulation function
# simulate.function <- function(data, n.iter, n.sim) {
#   # set.seed(123)
#   pb <- txtProgressBar(min = 0, max = n.iter, style = 3)
#   #objects to store sims
#   OUT <- out <- state <- STATE <- list()
#   # loop over all possible nrs of iterations
#   for (i in 1:n.iter) {
#     # repeat each analysis nsim times
#     for (j in 1:n.sim) {
#       # save current state of the randomness
#       state[[j]] <- .Random.seed[1]
#       # for each iterarion value, store each nsim 'repeatings' in lower case out
#       out[[j]] <-
#         mice(
#           data = data,
#           method = "norm",
#           maxit = i,
#           print = F
#         )
#     }
#     # for each iteration value, store the combined nsim simulations together
#     OUT[[i]] <- out
#     STATE[[i]] <- state
#     setTxtProgressBar(pb, i)
#   }
#   close(pb)
#   return(OUT)
# }