# script to perform simulation study on convergence of mice
# author: Hanne Oberman

###

# load required packages
library(mice)
library(mvtnorm)
library(miceadds)
library(data.table)
library(tidyverse)
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
n.sim <- 100 #nr of simulations per iteration value
true_effect <- 2 #regression coefficient to be estimated

# start simulation study
set.seed(11)

###

# simulate data once
data <- data.simulation(n = populationsize, true_effect)

# combine separate functions into wrapper
simulate <- function(data, n.iter, true_effect) {
  pb <- txtProgressBar(min = 0, max = n.iter, style = 3)
  # object for output
  res <- list()
  # repeat mi procedure 'runs'  times for each nr of iterations
  #for (run in 1:runs) {
    for (i in 1:n.iter) {
      res[[i]] <- test.impute(true_effect, data, maxit = i)
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

# plot
plot.ts(results, main = "", xlab = "Number of iterations")

###

# save for future reference
save.Rdata(results, name = "results12.Rdata", path = "Simulation/Results")
# save.image("environment.Rdata")
