# autocorrelation supplement

# function for lag 1 AC for a single chain
ac_lag1 <- function(x){
  x <- as.matrix(x)
  ac <- matrix(NA, 1, dim(x)[2])
  for (m in 1:dim(x)[2]) {
    ac[m] <- cor(x[-dim(x)[1], m], x[-1, m])
  }
  return(max(ac))
}

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


