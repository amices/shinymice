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
    R_mean <- R_var <- AC_mean <- AC_var <- between_mean <- between_var <- within_mean <- within_var <- c(rep(NA, 4))
  } else {
    R_mean <-
      rhat_function(impsim, maxit) #maximum Rhat across variables
    R_var <-
      rhat_function(impsim, maxit, moment = "variance") #maximum Rhat across variables
    AC_mean <-
      autocorr_function(impsim, maxit) #auto-correlation at lag 1
    AC_var <-
      autocorr_function(impsim, maxit, moment = "variance") #auto-correlation at lag 1
    between_mean <-
      between_function(impsim, maxit) #between chain variance of the chain means 
    between_var <-
      between_function(impsim, maxit, moment = "variance") #between chain variance of the chain variances
    within_mean <-
      within_function(impsim, maxit) #between chain variance of the chain means 
    within_var <-
      within_function(impsim, maxit, moment = "variance") #between chain variance of the chain variances
  }
  

  # perform analysis
  # # mip <- unlist(pool(with(impsim, lm(Y ~ X + Z1 + Z2))))
  # mip <- impsim %>% with(lm(Y ~ X + Z1 + Z2)) %>% pool %>% .$pooled
  # mip <- impsim %>% mice::complete(., "long") %>% .[,"Y"] %>%  mean #mean(complete(mids)[,2])
  mip <- impsim %>% mice::complete(., "long") %>% .[,-c(1:2)] %>% colMeans()
  
  # compute simulation diagnostics
  est <-  mip # mip$estimate[2] #estimated regression coefficient
  bias <- est - true_effect #bias
  SE <- 1 # sqrt(mip$b[2] + (mip$b[2] / m)) #pooled finite SE
  CI.low <- 1 # est - qt(.975, df = m - 1) * SE #lower bound CI
  CI.up <- 1 # est + qt(.975, df = m - 1) * SE #upper bound CI
  CIW <- 1 # CI.up - CI.low #confidence interval width
  cov <- 1 # CI.low < true_effect & true_effect < CI.up #coverage
  
  # output
  return(
    data.frame(
      # bias = bias,
      # CIW = CIW,
      # cov = cov,
      bias_X = bias["X"],
      bias_Y = bias["Y"],
      bias_Z1 = bias["Z1"],
      bias_Z2 = bias["Z2"],
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
      max_AC_var = min(AC_var),
      between_mean_X = between_mean["X"],
      between_mean_Z1 = between_mean["Z1"],
      between_mean_Z2 = between_mean["Z2"],
      between_mean_Y  = between_mean["Y"],
      between_var_X = between_var["X"],
      between_var_Z1 = between_var["Z1"],
      between_var_Z2 = between_var["Z2"],
      between_var_Y  = between_var["Y"],
      within_mean_X = within_mean["X"],
      within_mean_Z1 = within_mean["Z1"],
      within_mean_Z2 = within_mean["Z2"],
      within_mean_Y  = within_mean["Y"],
      within_var_X = within_var["X"],
      within_var_Z1 = within_var["Z1"],
      within_var_Z2 = within_var["Z2"],
      within_var_Y  = within_var["Y"]
    )
  )
}
