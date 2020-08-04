# Supplementary functions for AC function (see AC.R)
# To define lag 1 autocorrelations, computed two ways
# Requires packages 'purrr' and 'magrittr'/'dplyr'

# function for lag 1 AC for one or more imputation chains
ac_lag1 <- function(x) {
  # input: object with theta values (rows are iterations, columns are imputations)
  # output: one autocorrelation value per imputation (over the complete set of iterations in x)
  
  # make input ready for mapping
  if (is.numeric(x)) {
    x <- as.data.frame(x)
  }
  
  # compute ac per imputation (column)
  out <- map_dbl(1:dim(x)[2], function(m) {
    cor(x[-dim(x)[1], m], x[-1, m])
  }) %>% set_names(paste0("ac.", names(x)))
  
  # output
  return(out)
}


# adjusted stats::acf() function for lag 1 AC for one or more imputation chains
acf_lag1 <- function(x) {
  # input: object with theta values (rows are iterations, columns are imputations)
  # output: autocorrelation per imputation, with max and mean across imputations
  
  # make input ready for mapping
  if (is.numeric(x)) {
    x <- as.data.frame(x)
  }
  
  # compute ac
  out <-
    acf(x, lag.max = 1, plot = FALSE) %>% .$acf %>% .[2, , ] %>% as.matrix() %>% diag() %>% set_names(paste0("acf.", names(x)))
  
  
  # output
  return(out)
}
