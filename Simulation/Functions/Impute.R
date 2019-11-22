# Perform mi on mids object with varying maxit values
# requires the package 'mice' and the function 'get.rhat'

test.impute <- function(data,
                        m = 5,
                        method = "norm",
                        maxit,
                        ...) {
  # object for output
  tab <- matrix(nrow = 1, ncol = 8)
  
  if (maxit == 1) {
    impsim <<- mice(data,
        method = method,
        m = m,
        maxit = maxit,
        print = FALSE,
        ...)
  } else {
    impsim <<- mice.mids(impsim, maxit = 1, printFlag = FALSE) 
  }
  # impute missing values
  
  # compute convergence diagnostics
  if (maxit < 2) {
    tab[5] <- tab[6] <- tab[7] <- tab[8] <- NA
  }
  else if (maxit > 1) {
    tab[5] <- rhat_function(impsim, maxit) #maximum Rhat across variables
    tab[6] <- rhat_function(impsim, maxit, moment = "variance") #maximum Rhat across variables
    tab[7] <- autocorr_function(impsim, maxit) #auto-correlation at lag 1
    tab[8] <- autocorr_function(impsim, maxit, moment = "variance") #auto-correlation at lag 1
  }
  
  # extract estimates
  mip <- unlist(pool(with(impsim, lm(Y ~ X + Z1 + Z2))))
  tab[1] <- mip$pooled.estimate2 #estimated regression coefficient
  tab[2] <- sqrt((m + 1) * mip$pooled.b2 / m) #pooled SE
  tab[3] <- tab[1] - qt(.975, df = m - 1) * tab[2] #lower bound CI
  tab[4] <- tab[1] + qt(.975, df = m - 1) * tab[2] #upper bound CI
  
  # output
  as.numeric(tab)
}
