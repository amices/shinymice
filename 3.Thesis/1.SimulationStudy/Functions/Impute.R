# Perform mi on mids object with varying maxit values
# requires the package 'mice' and the function 'get.rhat'

test.impute <- function(data = data,
                        true_effect,
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
    R_mean <- c(rep(NA, 4))
    names(R_mean) <- c("X1", "X2", "X3", "Y")
    R_var <-
      AC_mean <-
      AC_var <-
      ACF_mean <-
      ACF_var <-
      between_mean <-
      between_var <- within_mean <- within_var <- R_mean
  } else {
    R_mean <-
      rhat_function(impsim, maxit) #maximum Rhat across variables
    R_var <-
      rhat_function(impsim, maxit, moment = "variance") #maximum Rhat across variables
    AC_mean <-
      autocorr_function(impsim, maxit) #auto-correlation at lag 1
    AC_var <-
      autocorr_function(impsim, maxit, moment = "variance") #auto-correlation at lag 1
    ACF_mean <-
      autocorr_function(impsim, maxit, ac_function = "acf") #auto-correlation at lag 1
    ACF_var <-
      autocorr_function(impsim, maxit, moment = "variance", ac_function = "acf") #auto-correlation at lag 1
    between_mean <-
      between_function(impsim, maxit) #between chain variance of the chain means
    between_var <-
      between_function(impsim, maxit, moment = "variance") #between chain variance of the chain variances
    within_mean <-
      within_function(impsim, maxit) #between chain variance of the chain means
    within_var <-
      within_function(impsim, maxit, moment = "variance") #between chain variance of the chain variances
  }
  
  # combine y_obs and y_imp
  imputed <-
    mice::complete(impsim, "all") # equal to: map(1:5, ~{impsim %>% mice::complete(., .x)})
  
  # compute univariate diagnostics
  bias_mean <-
    true_mean - imputed %>% map_df(., ~ {
      apply(., 2, mean)
    }) %>% apply(., 1, mean) #bias in est mean per variable
  bias_sd <-
    true_sd - imputed %>% map_df(., ~ {
      apply(., 2, sd)
    }) %>% apply(., 1, mean) #bias in est var per variable
  
  # compute multivariate diagnostics
  est <-
    map(imputed, lm, formula = Y ~ X1 + X2 + X3) %>% pool() %>% .$pooled %>% .$estimate #regression coefficients
  bias_est <- true_effect - est #bias in reg. coeff.
  var_est <-
    map(imputed, lm, formula = Y ~ X1 + X2 + X3) %>% pool() %>% .$pooled %>% .$b #est finite pop variance
  SE <- sqrt(var_est + (var_est / m)) #pooled finite SE
  CI.low <- est - qt(.975, df = m - 1) * SE #lower bound CI
  CI.up <- est + qt(.975, df = m - 1) * SE #upper bound CI
  CIW <- CI.up - CI.low #confidence interval width
  cov_est <- CI.low < true_effect & true_effect < CI.up #coverage
  
  # compute predictive performance
  R_sq <-
    lm.mids(Y ~ X1 + X2 + X3, impsim) %>% pool.r.squared() %>% .[1] #coeff of determination
  bias_R_sq <- true_R_sq - R_sq #bias in R squared
  RMSE <-
    lm.mids(Y ~ X1 + X2 + X3, impsim) %>% .$analyses %>% map_dbl(., function(x) {
      x$residuals %>% . ^ 2 %>% mean() %>%  sqrt()
    }) #root mean squared error
  MAE <-
    lm.mids(Y ~ X1 + X2 + X3, impsim) %>% .$analyses %>% map_dbl(., function(x) {
      x$residuals %>% abs() %>% mean()
    }) #mean absolute error
  # bij (te) weinig iteraties zouen we hogere var van MAE verwachten
  error_var <-
    lm.mids(Y ~ X1 + X2 + X3, impsim) %>% .$analyses %>% map_dbl(., function(x) {
      x$residuals %>% var()
    }) %>% mean() #residual variance
  bias_error_var <-
    true_sigma - error_var #bias in residual variance
  
  # # get Mahalanobis distance as multivariate convergence measure
  # covs <- map(imputed, ~{cov(.)})
  # mah <- mahalanobis(imputed[[1]], colMeans(imputed[[1]]), covs[[1]])
  
  # compute PCA loading instead
  pca <-
    map_dbl(imputed, ~ {
      princomp(., cor = TRUE) %>% .$sdev %>% .[1] %>% .^2 #first eigenvalue of the varcovar matrix
    })
  
  # collect output
  return(
    data.frame(
      t = maxit,
      bias.mean = t(bias_mean),
      R.mean = t(R_mean),
      AC.mean = t(AC_mean),
      ACF.mean = t(ACF_mean),
      between.mean = t(between_mean),
      within.mean = t(within_mean),
      bias.sd = t(bias_sd),
      R.var = t(R_var),
      AC.var = t(AC_var),
      ACF.var = t(ACF_var),
      pca = t(pca),
      bias.est = t(bias_est),
      CIW.est = t(CIW),
      cov.est = t(cov_est),
      R.sq = R_sq,
      bias.R.s = bias_R_sq,
      RMSE = t(RMSE),
      MAE = t(MAE),
      error.var = t(error_var),
      bias.sigma = t(bias_error_var)
      
    )
  )
}
