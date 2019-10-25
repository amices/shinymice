# script to perform simulation study on convergence of mice
# author: Hanne Oberman

# load required packages
library(mice)
library(mvtnorm)
library(miceadds)
library(dplyr)

# load simulation/evaluation functions
source("Functions/CreateData.R")
source("Functions/Simulate.R")
source("Functions/Evaluate.R")
# source("Functions/xyz.R")

# simulation parameters
set.seed(321)
populationsize <- 100
n.iter <- 10
n.sim <- 10
# bivar.corr <- 0.5

# create data, perform lm(), and ampute data to impute
data <- data.simulation(n = populationsize, bivar.corr = .5)

# run simulation
sims <-
  simulate.function(data = data[[1]],
                    n.iter = n.iter,
                    n.sim = n.sim)

# compute R hat
conv <- invisible(convergence.diag(sims = sims))
##### UNDER CONSTRUCTION #####

# Evaluate
# Displayed are the bias, coverage rate (cov) of the 95% confidence interval and the confidence interval width (ciw).
