# perform multiple imputation on incomplete data
# requires the packages 'mice' and 'dplyr'/'magrittr'
# and the complete data estimates for the quantities of scientific interest

test.impute <- function(amp_data,
                        it_nr,
                        final_it, ...) {
  # input: an incomplete dataset, the number of iterations in the simulation condition
  # and the maximum number of iterations across conditions
  # output: performance measures (bias, coverage and confidence interval width)
  
  # impute missing values
  if (it_nr == 1) {
    impsim <<- mice(
      data = amp_data,
      method = "norm",
      m = 5,
      maxit = 1,
      print = FALSE
    )
  } else {
    impsim <<- mice.mids(impsim, maxit = 1, printFlag = FALSE)
  }
  
  # save chain means and variances for convergence diagnostics
  if (it_nr == final_it) {
    chain_means <<-
      data.frame(
        t = 1:final_it,
        chain.mean.X1 = impsim$chainMean["X1", , ],
        chain.mean.X2 = impsim$chainMean["X2", , ],
        chain.mean.X3 = impsim$chainMean["X3", , ],
        chain.mean.Y = impsim$chainMean["Y", , ]
      )
    chain_vars <<-
      data.frame(
        t = 1:final_it,
        chain.var.X1 = impsim$chainVar["X1", , ],
        chain.var.X2 = impsim$chainVar["X2", , ],
        chain.var.X3 = impsim$chainVar["X3", , ],
        chain.var.Y = impsim$chainVar["Y", , ]
      )
  }
  
  # combine y_obs and y_imp
  imputed <-
    mice::complete(impsim, "all") # equal to: map(1:5, ~{impsim %>% mice::complete(., .x)})
  
  # compute univariate diagnostics
  m <- 5
  means <-
    imputed %>% map_df(., ~ {
      apply(., 2, mean)
    }) %>% apply(., 1, mean) #bias in est mean per variable
  sds <-
    imputed %>% map_df(., ~ {
      apply(., 2, sd)
    }) %>% apply(., 1, mean) #bias in est var per variable
  names(means) <- names(sds) <- names(imputed[[1]])
  
  # compute multivariate diagnostics
  mipo <-
    map(imputed, lm, formula = Y ~ X1 + X2 + X3) %>% pool() %>% .$pooled
  est <- mipo %>% .$estimate #regression coefficients
  names(est) <- mipo$term
  var_est <- mipo %>% .$b #est finite pop variance
  SE <- sqrt(var_est + (var_est / m)) #pooled finite SE
  CI.low <- est - qt(.975, df = m - 1) * SE #lower bound CI
  CI.up <- est + qt(.975, df = m - 1) * SE #upper bound CI
  CIW <- CI.up - CI.low #confidence interval width
  cov_est <- CI.low < true.effect & true.effect < CI.up #coverage
  
  # track worst performing regression coefficient as theta 
  beta <-
    map_dbl(imputed, ~ {
      lm(formula = Y ~ X1 + X2 + X3, data = .) %>% .$coefficients %>% .[2]
    })
  
  # compute predictive performance
  R_sq <-
    lm.mids(Y ~ X1 + X2 + X3, impsim) %>% pool.r.squared() %>% .[1] #coeff of determination
  # RMSE <- #root mean squared error
  #   lm.mids(Y ~ X1 + X2 + X3, impsim) %>% .$analyses %>% map_dbl(., function(x) {
  #     x$residuals %>% . ^ 2 %>% mean() %>%  sqrt()
  #   }) #root mean squared error
  # MAE <- #mean absolute error
  #   lm.mids(Y ~ X1 + X2 + X3, impsim) %>% .$analyses %>% map_dbl(., function(x) {
  #     x$residuals %>% abs() %>% mean()
  #   }) #mean absolute error
  error_var <-
    lm.mids(Y ~ X1 + X2 + X3, impsim) %>% .$analyses %>% map_dbl(., function(x) {
      x$residuals %>% var()
    }) %>% mean() #residual variance
  
  # compute lambda_1
  pca <-
    map_dbl(imputed, ~ {
      princomp(., cor = TRUE) %>% .$sdev %>% .[1] %>% . ^ 2 #first eigenvalue of the varcovar matrix
    })
  
  # collect output
  return(
    data.frame(
      t = it_nr,
      bias.mean = t(means - true.mean),
      bias.sd = t(sds - true.sd),
      bias.est = t(est - true.effect),
      CIW.est = t(CIW),
      cov.est = t(cov_est),
      bias.R.sq = R_sq - true.R.sq,
      #RMSE = t(RMSE),
      #MAE = t(MAE),
      bias.error = error_var - true.sigma,
      pca = t(pca),
      beta = t(beta)
      
    )
  )
}
