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
#source("simulation100it100sim.Rdata")
# source("Functions/xyz.R")

# simulation parameters
set.seed(128)
populationsize <- 1000
n.iter <- 100
n.sim <- 10
# bivar.corr <- 0.5

# create data, perform lm(), and ampute data to impute
data <- data.simulation(n = populationsize, bivar.corr = .5)
## question: do I need a completely obs. covariate X?
## q: which vars then go into lm()?
## q: is missingness proportion of .5 ok?
## q: do I need to set.seed() within this function or is outside enough?

# run simulation
sims <-
  simulate.function(data = data[[1]],
                    n.iter = n.iter,
                    n.sim = n.sim)
## q: how do I make sure that mice doesn't start from the same seed value every simulation?
## q: how do I speed this up? should I use parlmice()?
## q: should I save everything of all simulations? the incomplete data is the same each it, each sim. should I just extract the imps?

# compute R hat
conv <- convergence.diag(sims = sims)
## q: should I include this (and the next function) in the simulate step to save on memory use? I don't actually needs the imp objects

# create analyzed object
mi.lm <- lapply(sims, lapply, my.lm)
## q: should I use the finite pool function to get nominal coverage?
## q: how?

# eval
evals <- lapply(mi.lm, sapply, evaluate.function)
## q: do I need to save this in an object or just the averages?
## q: do I need to compute var/se of the diagnostics?

# extract
result <- cbind(t(sapply(evals, rowMeans)), conv)
## q: include this in function?

# plot (not fancy, just to check)
colnames(result) <- c("bias", "ciw", "cov", "Rhat", "Rhat_lenient", "Rhat_mid", "Rhat_stringent")
for (i in 1:7) {
  plot(result[,i], main = colnames(result)[i])
}

# save for future reference
save(result, filename = "results.Rdata")
