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
source("Functions/NewSim.R")
source("Functions/Diagnostics.R")

# simulation parameters
populationsize <- 1000
n.iter <- 10
n.sim <- 10
true_effect <- 2

# start simulation study
set.seed(128)

# create data

# perfrom mi
impute.data <- function(data, m = 5, method = "norm", maxit = maxit, ...) {
  imp <- mice(data, m = m, maxit = maxit, print = FALSE, ...)
  fit <- with(imp, lm(Y ~ X + Z1 + Z2))
  tab <- summary(pool(fit), "all", conf.int = TRUE)
  as.numeric(tab["x", c("estimate", "2.5 %", "97.5 %")])
}


# combine individual functions into simulation function
simulate <- function(runs = 10, n.iter = 10) {
  res <- array(NA, dim = c(n.iter, runs, 3))
  dimnames(res) <- list(c(1:n.iter),
                        as.character(1:runs),
                        c("estimate", "2.5 %","97.5 %"))
  for(run in 1:runs) {
    data <- data.simulation(populationsize, true_effect)
    for (i in 1:n.iter) {
    res[i, run, ] <- impute.data(data, maxit = i)}
  }
  res
}

# perform simulation
res <- simulate(2)

# evaluate simulations
evaluate.sim <- function(sims, true_effect = true_effect){
  RB <- rowMeans(res[,, "estimate"]) - true_effect
  CR <- rowMeans(res[,, "2.5 %"] < true_effect & true_effect < res[,, "97.5 %"])
  AW <- rowMeans(res[,, "97.5 %"] - res[,, "2.5 %"])
  return(data.frame(RB, CR, AW))
}

evaluate.sim(res)
  

# create data and ampute it
data <- data.simulation(n = populationsize, true_effect = true_effect)

# new sim
test.sim <- simulate.fun(data, n.iter, n.sim)

# run simulation
sims <-
  simulate.function(data = data,
                    n.iter = n.iter,
                    n.sim = n.sim)

# compute R hat
conv <- convergence.diag(sims = sims)

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
