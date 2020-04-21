# autocorrelation supplement

# function for lag 1 AC for one or more imputation chains 
ac_lag1 <- function(x){
  # # make input the same object type
  # x <- as.matrix(x)
  # # create output object with length equal to the number of imputation chains of the input
  # ac <- matrix(NA, 1, dim(x)[2])
  # # for each imputation
  # for (m in 1:dim(x)[2]) {
  #   # compute the correlation between the chain and itself, one iteration delayed
  #   ac[m] <- cor(x[-dim(x)[1], m], x[-1, m])
  # }
  ac <- map_dbl(1:dim(x)[2], function(m){
    cor(x[-dim(x)[1], m], x[-1, m]) 
  })
  names(ac) <- paste0("ac.", names(x))
  #names(ac) <- names(x)
  if(!is.na(ac)[1]){
    ac <- c(ac, mean.ac = mean(ac), max.ac = max(ac))
    }
  return(data.frame(t(ac)))
}

acf_lag1 <- function(x){acf(x, lag.max = 1, plot = FALSE) %>% .$acf %>% .[2, , ] %>% diag() %>% set_names(paste0("acf.", names(x)))}

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


