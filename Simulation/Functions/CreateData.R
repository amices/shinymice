# function to create data and ampute it
# requires the packages 'mice' and 'mvtnorm'

data.simulation <- function(n = populationsize, bivar.corr = .5){
  # define variance covariance matrix
  sigma <- matrix(c(1, bivar.corr, bivar.corr, 1), 2, 2)
  # simulate multivariate normal
  simdata <- rmvnorm(n = populationsize, sigma = sigma)
  # remove values at random with 50 percent probability to be missing
  ampdata <- ampute(simdata, mech = "MCAR")$amp
}

