# script to perform simulation study on convergence of mice
# author: Hanne Oberman

###

# load required packages
library(mice)
library(mvtnorm)
library(miceadds)
library(data.table)
library(tidyverse)
# or if you don't want the entire tidyverse, just load these
# library(dplyr)
# library(ggplot2)
# library(purrr)
# library(magrittr)
# library(tibble)

# load simulation/evaluation functions
source("Simulation/Functions/CreateData.R")
source("Simulation/Functions/Convergence.R")
source("Simulation/Functions/Convergence_supplement.R")
source("Simulation/Functions/Autocorrelation.R")
source("Simulation/Functions/Impute.R")
source("Simulation/Functions/Evaluate.R")

# simulation parameters
populationsize <- 1000 #n of simulated dataset
n.iter <- 100 #nr of iterations (varying 1:n.iter)
n.sim <- 1000 #nr of simulations per iteration value
true_effect <- 2 #regression coefficient to be estimated

# start simulation study
set.seed(1111)

###

# simulate data once
data <- data.simulation(n = populationsize, true_effect)

# combine separate functions into wrapper
simulate <- function(data, n.iter, true_effect) {
  pb <- txtProgressBar(min = 0, max = n.iter, style = 3)
  
  # remove values at random with 20 percent probability to be missing
  ampdata <- ampute(data, prop = 0.8, mech = "MCAR")$amp
  
  # object for output
  res <- list()
  # repeat mi procedure 'runs'  times for each nr of iterations
  #for (run in 1:runs) {
    for (i in 1:n.iter) {
      res[[i]] <- test.impute(true_effect, data = ampdata, maxit = i)
      setTxtProgressBar(pb, i)
    }
  #}
  close(pb)
  # output
  names(res) <- 1:n.iter
  res
}

# simulate
out <- replicate(n.sim, simulate(data = data, n.iter = n.iter, true_effect = true_effect), simplify = FALSE)

###

# evaluate
results <- evaluate.sim(sims = out)
names(results) <- c("It.", "Bias", "CI width", "Cov. rate", "R chain mean", "R chain var", "AC chain mean", "AC chain var")

# plot
plot.ts(results[c(5:8, 2:4)], main = "Convergence and Simulation Diagnostics", xlab = "Number of iterations")
plot.ts(results[2:4], main = "Simulation Diagnostics", xlab = "Number of iterations")
plot.ts(results[5:8], main = "Convergence Diagnostics", xlab = "Number of iterations")

###

# save for future reference
save.Rdata(results, name = "results14.Rdata", path = "Simulation/Results")
# save.image("environment.Rdata")
