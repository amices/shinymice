# Perform mi on mids object with varying maxit values
# requires the package 'mice' and the function 'get.rhat'

test.impute <- function(true_effect,
                        data,
                        m = 5,
                        method = "norm",
                        maxit,
                        ...) {

  # impute missing values
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
  
  # compute convergence diagnostics
  if (maxit < 2) {
    R_mean <- R_var <- AC_mean <- AC_var <- c(rep(NA, 4))
  } else {
    R_mean <-
      rhat_function(impsim, maxit) #maximum Rhat across variables
    R_var <-
      rhat_function(impsim, maxit, moment = "variance") #maximum Rhat across variables
    AC_mean <-
      autocorr_function(impsim, maxit) #auto-correlation at lag 1
    AC_var <-
      autocorr_function(impsim, maxit, moment = "variance") #auto-correlation at lag 1
  }
  

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
      R_mean_X = R_mean["X"],
      R_mean_Z1 = R_mean["Z1"],
      R_mean_Z2 = R_mean["Z2"],
      R_mean_Y = R_mean["Y"],
      max_R_mean = max(R_mean),
      R_var_X = R_var["X"],
      R_var_Z1 = R_var["Z1"],
      R_var_Z2 = R_var["Z2"],
      R_var_Y  = R_var["Y"],
      max_R_var = max(R_var),
      AC_mean_X = AC_mean["X"],
      AC_mean_Z1 = AC_mean["Z1"],
      AC_mean_Z2 = AC_mean["Z2"],
      AC_mean_Y = AC_mean["Y"],
      max_AC_mean = min(AC_mean),
      AC_var_X = AC_var["X"],
      AC_var_Z1 = AC_var["Z1"],
      AC_var_Z2 = AC_var["Z2"],
      AC_var_Y  = AC_var["Y"],
      max_AC_var = min(AC_var)
    )
  )
}
