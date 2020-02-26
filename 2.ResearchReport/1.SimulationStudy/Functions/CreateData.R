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
  colnames(simdata) <- c("X", "Z1", "Z2") #set predictors names
  
  # compute  outcome variable from predictors
  simdata$Y <-
    true_effect * simdata$X + 0.5 * simdata$Z1 - 1 * simdata$Z2 + rnorm(n = populationsize, sd = 10)
  
  # estimate comlpete data parameter
  true_effect <<- mean(simdata$Y)# unname(lm(Y ~ X + Z1 + Z2, data = simdata)[["coefficients"]]["X"])

  # output
  return(simdata)
  
}
