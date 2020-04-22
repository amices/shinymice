# autocorrelation supplement
# requires library(purrr) and dplyr/magrittr

# function for lag 1 AC for one or more imputation chains
ac_lag1 <- function(x) {
  # input: object with theta values (rows are iterations, columns are imputations)
  # output: autocorrelation per imputation, with max and mean across imputations
  
  # make input ready for mapping
  if (is.numeric(x)) {
    x <- as.data.frame(x)
  }
  
  # compute ac
  ac <- map_dbl(1:dim(x)[2], function(m) {
    cor(x[-dim(x)[1], m], x[-1, m])
  }) %>% set_names(paste0("ac.", names(x)))
  
  # add mean and max for iterations > 2
  if (!is.na(ac)[1]) {
    ac <- c(ac, mean.ac = mean(ac), max.ac = max(ac))
  }
  
  # output
  return(data.frame(t(ac)))
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
  acf <-
    acf(x, lag.max = 1, plot = FALSE) %>% .$acf %>% .[2, , ] %>% diag() %>% set_names(paste0("acf.", names(x)))
  
  # add mean and max for iterations > 2
  if (!is.na(acf)[1]) {
    acf <- c(acf, mean.acf = mean(acf), max.acf = max(acf))
  }
  
  # output
  return(data.frame(t(acf)))
}

################

# chain <- 1:4
# Tt <- length(chain)
# theta <- chain - mean(chain)
#
# Tt/(Tt-1) * (theta[-1] %*% theta[-Tt])/(theta[-1]%*%theta[-1])

# for (t in 1:(Tt-1)){
#   a[t] <- theta[t]*theta[t+1]
# }
# sum(a)/Tt

#
#
# AC <- (theta[-length(theta)] %*% theta[-1]) / (theta[-length(theta)] %*% theta[-length(theta)])
