# simulation version 2
simulate.fun <- function(x, n.iter, n.sim) {
  # fixed parameters
  m <- 5 # chains <- dim(chain_mean)[3] # == m
  n.var <- 4 #length(mids$imp) # == Y, X, Z1, Z2
  lag.max <- 3
  AC_lim <- qnorm((1 + .95) / 2) / sqrt(n.iter)
  
  # object to store mids in
  mids <- out <- list()
  
  # apply mice to amputed data, with varying maxit values
  for (i in 1:n.iter) {
    mids <- mice(x,
                 method = "norm",
                 maxit = i,
                 print = F)
    # mids <- mice(data, method = "norm", maxit = 100, print = F)
    
    ########## R hat ###########
    # compute Rhat
    if (i < 2) {
      hat_R <- NA
    }
    else if (i > 1) {
      n_samples <- i # == maxit value
      # n.var <- length(mids$imp) # == Y, X, Z1, Z2
      var_between <-
        var_within <-
        var_plus <- hat_R <- matrix(NA, nrow = 1, ncol = n.var)
      AC <- matrix(NA, nrow = m, ncol = lag.max)
      AC_sig <- matrix(NA, nrow = lag.max, ncol = n.var)
      
      # loop over imputed variables
      for (v in 1:n.var) {
        # var_between[v] <- n_samples * var(mids$chainMean[v,n_samples, ])
        # var_within[v] <- mean(mids$chainVar[v,n_samples, ])
        var_between[v] <-
          n_samples * var(apply(mids$chainMean[v, ,], 2, mean))
        var_within[v] <- mean(apply(mids$chainMean[v, ,], 2, var))
        
        # # from rstan::Rhat
        # hat_R[v] <-
        #  sqrt((var_between[v] / var_within[v] + n_samples - 1) / n_samples)
        # is the same as:
        var_plus[v] <-
          (((n_samples - 1) / n_samples) * var_within[v]) + (var_between[v] / n_samples)
        
        hat_R[v] <- sqrt(var_plus[v] / var_within[v])
        # this one is similar but outdated:
        # hat_R[v] <- (m + 1)/m * (var_plus[v] / var_within[v]) - ((n_samples-1)/(m*n_samples))
        # # this one is incorrect but just to check
        # var_plus[v] <-
        #   var_within[v] + (var_between[v] / n_samples)
      
        # ######### ACF ##########
        # # calculate autocorrelation upto lag 10
        # for (imps in 1:m) {
        #   AC[imps, ] <-
        #     acf(mids$chainMean[v, , imps],
        #         lag.max = 3,
        #         plot = F)$acf[-1] > AC_lim
        #   AC_sig[ , v] <- colMeans(AC)
        # }
        
      }
    }
    out[[i]] <- c(max(hat_R)) #, max(AC_sig[1,]), max(AC_sig[2,]), max(AC_sig[1,]))
  }
  return(out)
}
