# function to create data and ampute it
# requires the packages 'mice' and 'mvtnorm'

data.simulation <- function(n = populationsize, bivar.corr = .5) {
  # define variance covariance matrix
  sigma <- matrix(c(1, bivar.corr, bivar.corr, 1), 2, 2)
  # simulate multivariate normal
  simdata <-
    as.data.frame(rmvnorm(n = populationsize, sigma = sigma))
  # perform complete data analysis
  compl.lm <- lm(V1 ~ V2, simdata)
  # remove values at random with 50 percent probability to be missing
  ampdata <- ampute(simdata, mech = "MCAR")$amp
  
  return(list(data = ampdata, compl.lm = compl.lm))
}
