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
source("3.Thesis/1.SimulationStudy/Functions/CreateData.R")
source("3.Thesis/1.SimulationStudy/Functions/Convergence.R")
source("3.Thesis/1.SimulationStudy/Functions/Convergence_supplement.R")
source("3.Thesis/1.SimulationStudy/Functions/Between.R")
source("3.Thesis/1.SimulationStudy/Functions/Within.R")
source("3.Thesis/1.SimulationStudy/Functions/Autocorrelation.R")
source("3.Thesis/1.SimulationStudy/Functions/Impute.R")
source("3.Thesis/1.SimulationStudy/Functions/Evaluate.R")

# simulation parameters
populationsize <- 10000 #n of simulated dataset
n.iter <- 30 #nr of iterations (varying 1:n.iter)
n.sim <- 20 #nr of simulations per iteration value
true_effect <- 2 #regression coefficient to be estimated
true_mean <- true_sd <- NA

# start simulation study
set.seed(1111)

###

# simulate data once
data <- data.simulation(n = populationsize, true_effect)
# run ampute once to get patterns object (and )to be able to adjust it for multivariate missingness)
old_amp_patterns <- ampute(data)$patterns
# for multivariable missingness, uncomment this
# amp_patterns[1:4, 1] <- 0
amp_patterns <- expand.grid(c(0,1),c(0,1), c(0,1), c(0,1)) %>% .[c(-1, -16),]# %>% unname()
names(amp_patterns) <- names(old_amp_patterns)

# combine separate functions into wrapper
simulate <- function(data, n.iter, true_effect, patterns) {
  pb <- txtProgressBar(min = 0, max = n.iter, style = 3)
  
  # remove values at random with 20 percent probability to be missing
  ampdata <- ampute(data, patterns = amp_patterns, prop = 0.95, mech = "MCAR")$amp
  
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
out <- replicate(n.sim, simulate(data = data, n.iter = n.iter, true_effect = true_effect, patterns = amp_patterns), simplify = FALSE)

###

# evaluate
results <- evaluate.sim(sims = out, n.iter = n.iter)
# uncomment for MCMC SEs
MCMCSE <- evaluate.sim(sims = out, n.iter = n.iter, mean_or_SE = "se")
# combine into one dataframe
results_with_SE <- left_join(results, MCMCSE, by = "T", suffix = c("", ".SE"))
# with empirical CI
CI_lower <- evaluate.sim(sims = out, n.iter = n.iter, mean_or_SE = "lower")
CI_upper <- evaluate.sim(sims = out, n.iter = n.iter, mean_or_SE = "upper")
results_with_CI <- results %>% left_join(CI_lower, by = "T", suffix = c("", ".LL")) %>% left_join(CI_upper, by = "T", suffix = c("", ".UL"))

# plot
#plot.ts(results[c(5:8, 2:4)], main = "Convergence and Simulation Diagnostics", xlab = "Number of iterations")
#plot.ts(results[2:4], main = "Simulation Diagnostics", xlab = "Number of iterations")
#plot.ts(results[5:12], main = "Convergence Diagnostics", xlab = "Number of iterations")
#plot.ts(results[13:20], main = "Convergence Diagnostics", xlab = "Number of iterations")
#plot.ts(results[21:28], main = "Convergence Diagnostics", xlab = "Number of iterations")
#plot.ts(results[29:36], main = "Convergence Diagnostics", xlab = "Number of iterations")

###

# save for future reference
#save.Rdata(results_with_CI, name = "full_results", path = "2.ResearchReport/1.SimulationStudy/Results")
# save.image("environment_full_results.Rdata")
