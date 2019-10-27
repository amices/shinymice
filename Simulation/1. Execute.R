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
populationsize <- 1000
n.iter <- 100
n.sim <- 100
# bivar.corr <- 0.5

# create data, perform lm(), and ampute data to impute
data <- data.simulation(n = populationsize, bivar.corr = .5)

# run simulation
sims <-
  simulate.function(data = data[[1]],
                    n.iter = n.iter,
                    n.sim = n.sim)

# create test objects 
# test <- sims[[2]][[1]]
# test2 <- list(sims[[2]][[1]], sims[[2]][[2]])
# test3 <- my.lm(test)

# compute R hat
conv <- convergence.diag(sims = sims)

# create analyzed object
mi.lm <- lapply(sims, lapply, my.lm)

# eval
evals <- lapply(mi.lm, sapply, evaluate.function)

# extract
result <- cbind(t(sapply(evals, rowMeans)), conv)


##### UNDER CONSTRUCTION #####

# Evaluate
# Displayed are the bias, coverage rate (cov) of the 95% confidence interval and the confidence interval width (ciw).
