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
#source("Functions/Evaluate.R")
source("Functions/Diagnostics.R")

# simulation parameters
populationsize <- 1000
n.iter <- 5
n.sim <- 10
true_effect <- 2

# start simulation study
set.seed(128)

# create data and ampute it
data <- data.simulation(n = populationsize, true_effect = true_effect)

# run simulation
sims <-
  simulate.function(data = data,
                    n.iter = n.iter,
                    n.sim = n.sim)
## q: should I save everything of all simulations? the incomplete data is the same each it, each sim. should I just extract the imps?

# compute R hat
conv <- convergence.diag(sims = sims)
## q: should I include this (and the next function) in the simulate step to save on memory use? I don't actually needs the imp objects

# create analyzed object
mi.lm <- lapply(sims, lapply, my.lm)

# eval
evals <- lapply(mi.lm, sapply, evaluate.function) # a list where every element is ...
## q: do I need to save this in an object or just the averages? ses are nice for plots! create error bars

# extract
result <- cbind(t(sapply(evals, rowMeans)), conv)
## q: include this in function? pipe sapply rowmeans, dan result t evals conv

# plot (not fancy, just to check)
colnames(result) <- c("bias", "ciw", "cov", "Rhat", "Rhat_lenient", "Rhat_mid", "Rhat_stringent")
for (i in 1:7) {
  plot(result[,i], main = colnames(result)[i])
}

# save for future reference
save(result, filename = "results.Rdata")
