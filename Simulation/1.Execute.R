# script to perform simulation study on convergence of mice
# author: Hanne Oberman

###

# load required packages
library(mice)
library(mvtnorm)
library(miceadds)
library(dplyr)
library(ggplot2)

# load simulation/evaluation functions
source("Functions/CreateData.R")
source("Functions/Convergence.R")
source("Functions/Convergence_supplement.R")
source("Functions/Autocorrelation.R")
source("Functions/Impute.R")
source("Functions/Evaluate.R")

# simulation parameters
populationsize <- 1000 #n of simulated dataset
n.iter <- 15 #nr of iterations (varying 1:n.iter)
n.sim <- 10 #nr of simulations per iteration value
true_effect <- 2 #regression coefficient to be estimated

# start simulation study
set.seed(1111)

###

# combine separate functions into wrapper
simulate <- function(runs = 10, n.iter = 5) {
  pb <- txtProgressBar(min = 0, max = runs, style = 3)
  # object for output
  res <- array(NA, dim = c(n.iter, runs, 6))
  # simulate data once
  data <- data.simulation(n = poulationsize)
  # repeat mi procedure 'runs'  times for each nr of iterations
  for (run in 1:runs) {
    for (i in 1:n.iter) {
      res[i, run, ] <- test.impute(data, method = "norm",
                                   maxit = i)
    }
    setTxtProgressBar(pb, run)
  }
  close(pb)
  res
}

# simulate
res <- simulate(runs = n.sim, n.iter = n.iter)

###

# evaluate
(out <- evaluate.sim(res))

###

# plot
# load("Results/results2.Rdata.Rdata")
# out <- dat
plot.ts(out, main = "", xlab = "Number of iterations")

###

# save for future reference
save.Rdata(out, name = "results6.Rdata", path = "Results")
# list <- as.list(res)
# saveRDS(list, file = "allsims2.Rdata")
