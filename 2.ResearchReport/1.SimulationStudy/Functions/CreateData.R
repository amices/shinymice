# function to create data and ampute it
# requires the packages 'mice' and 'mvtnorm'

data.simulation <- function(n = populationsize, true_effect = 2) {
  
  # simulate multivariate normal predictors
  means <- c(12, 3, 0.5) #predictor means
  vars  <- c(4, 16, 9) #predictor variances  
  R <- matrix(numeric(3 * 3), nrow = 3) #correlation matrix
  diag(R) <- 1 #set diagonal to 1
  R[upper.tri(R)] <- R[lower.tri(R)] <- c(.5, .3, .4) #set bivariate correlations
  sigma <- diag(sqrt(vars)) %*% R %*% diag(sqrt(vars)) #variance-covariance matrix
  simdata <-
    as.data.frame(rmvnorm(n = populationsize, mean = means, sigma = sigma)) #create data
  colnames(simdata) <- c("X1", "X2", "X3") #set predictors names
  
  # compute  outcome variable from predictors
  simdata$Y <-
    true_effect * simdata$X1 + 0.5 * simdata$X2 - 1 * simdata$X3 + rnorm(n = populationsize, sd = 10)
  
  # estimate comlpete data parameter
  true_effect <<- lm(Y ~ X1 + X2 + X3, data = simdata)[["coefficients"]]
  true_mean <<- apply(simdata, 2, mean)#colMeans(simdata)
  true_sd <<- apply(simdata, 2, sd)
  
  # output
  return(simdata)
  
}

###
# ampute
# 50% univar miss
# sapply(A, function(x) sample(c(0, NA), length(x))
# 