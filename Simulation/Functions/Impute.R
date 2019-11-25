# Perform mi on mids object with varying maxit values
# requires the package 'mice' and the function 'get.rhat'

test.impute <- function(data,
                        m = 5,
                        method = "norm",
                        maxit,
                        ...) {
  # object for output
  #tab <- list() # matrix(nrow = 1, ncol = 8)
  
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
    R.mean <- R.var <- AC.mean <- AC.var <- NA
  }
  else if (maxit > 1) {
    R.mean <- rhat_function(impsim, maxit) #maximum Rhat across variables
    R.var <- rhat_function(impsim, maxit, moment = "variance") #maximum Rhat across variables
    AC.mean <- autocorr_function(impsim, maxit) #auto-correlation at lag 1
    AC.var <- autocorr_function(impsim, maxit, moment = "variance") #auto-correlation at lag 1
  }
  
  # extract estimates
  mip <- unlist(pool(with(impsim, lm(Y ~ X + Z1 + Z2))))
  est <- mip$pooled.estimate2 #estimated regression coefficient
  SE <- sqrt(mip$pooled.b2 + mip$pooled.b2 / m) #pooled SE
  #tab[1:2] <- mip$pooled %>% select(estimate, b) %>% mutate(b = b + b/m)
  CI.low <- est - qt(.975, df = m - 1) * SE #lower bound CI
  CI.up <- est + qt(.975, df = m - 1) * SE #upper bound CI
  
  # output
  return(list(est = est, SE = SE, CI.low = CI.low, CI.up = CI.up, R.mean = R.mean, R.var = R.var, AC.mean = AC.mean, AC.var = AC.var))
 }
