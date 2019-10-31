# function to create data and ampute it
# requires the packages 'mice' and 'mvtnorm'

data.simulation <- function(n = populationsize, true_effect = 2) {
  # create variance covariance matrix of the predictors from bivariate correlations
  R <-
    matrix(numeric(3 * 3), nrow = 3) # will be the correlation matrix
  diag(R) <- 1 # set diagonal to 1
  xy <- 0.5 # bivar correlations
  xz <- 0.3
  yz <- 0.4
  R[upper.tri(R)] <-
    c(xy, xz, yz) # fill in x, y, z to upper right
  R[lower.tri(R)] <-
    c(xy, xz, yz) # fill in x, y, z to lower left
  vars  <- c(4, 16, 9)              # arbitrary variances
  sigma <- diag(sqrt(vars)) %*% R %*% diag(sqrt(vars))
  # simulate multivariate normal predictors
  simdata <-
    as.data.frame(rmvnorm(n = populationsize, sigma = sigma))
  colnames(simdata) <- c("X", "Z1", "Z2")
  # compute  outcome var
  simdata$Y <-
    1 + true_effect * simdata$X + 0.5 * simdata$Z1 - 1 * simdata$Z2 + rnorm(1)
  # # perform complete data analysis
  # compl.lm <- lm(Y ~ X + Z1 + Z2, simdata)
  
  # remove values at random with 50 percent probability to be missing
  ampdata <- ampute(simdata, prop = 0.5, mech = "MCAR")$amp
  # add pattern, y obs and x not & x obs and y not
  # output
  return(ampdata)
}
