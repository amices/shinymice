# Perform mi on mids object with varying maxit values
# requires the package 'mice' and the function 'get.rhat'

test.impute <- function(true_effect,
                        data,
                        m = 5,
                        method = "norm",
                        maxit,
                        ...) {
  # object for output
  #tab <- list() # matrix(nrow = 1, ncol = 8)
  
  if (maxit == 1) {
    impsim <<- mice(
      data,
      method = method,
      m = m,
      maxit = maxit,
      print = FALSE,
      ...
    )
  } else {
    impsim <<- mice.mids(impsim, maxit = 1, printFlag = FALSE)
  }
  # impute missing values
  
  # compute convergence diagnostics
  if (maxit < 2) {
    R_mean <- R_var <- NA
    AC_mean <- AC_var <- c(rep(NA, 4))
  }
  else if (maxit > 1) {
    R_mean <-
      rhat_function(impsim, maxit) #maximum Rhat across variables
    R_var <-
      rhat_function(impsim, maxit, moment = "variance") #maximum Rhat across variables
    AC_mean <-
      autocorr_function(impsim, maxit) #auto-correlation at lag 1
    AC_var <-
      autocorr_function(impsim, maxit, moment = "variance") #auto-correlation at lag 1
  }
  
  # AC_mean_X = AC_mean[1]
  # AC_mean_Z1 = AC_mean[2]
  # AC_mean_Z2 = AC_mean[3]
  # AC_mean_Y = AC_mean[4]
  # AC_var_X = AC_var[1]
  # AC_mean_Z1 = AC_var[2]
  # AC_mean_Z2 = AC_var[3]
  # AC_mean_Y  = AC_var[4]

  # perform analysis
  # mip <- unlist(pool(with(impsim, lm(Y ~ X + Z1 + Z2))))
  mip <- impsim %>% with(lm(Y ~ X + Z1 + Z2)) %>% pool %>% .$pooled
  
  # compute simulation diagnostics
  est <- mip$estimate[2] #estimated regression coefficient
  bias <- est - true_effect #bias
  SE <- sqrt(mip$b[2] + (mip$b[2] / m)) #pooled finite SE
  CI.low <- est - qt(.975, df = m - 1) * SE #lower bound CI
  CI.up <- est + qt(.975, df = m - 1) * SE #upper bound CI
  CIW <- CI.up - CI.low #confidence interval width
  cov <- CI.low < true_effect & true_effect < CI.up #coverage
  
  # output
  return(
    data.frame(
      bias = bias,
      CIW = CIW,
      cov = cov,
      R_mean = R_mean,
      R_var = R_var,
      AC_mean_X = AC_mean[1],
      AC_mean_Z1 = AC_mean[2],
      AC_mean_Z2 = AC_mean[3],
      AC_mean_Y = AC_mean[4],
      AC_var_X = AC_var[1],
      AC_var_Z1 = AC_var[2],
      AC_var_Z2 = AC_var[3],
      AC_var_Y  = AC_var[4]
    )
  )
}
