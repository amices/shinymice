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
source("3.Thesis/1.SimulationStudy/Functions/Autocorrelation_supplement.R")
source("3.Thesis/1.SimulationStudy/Functions/Impute.R")
source("3.Thesis/1.SimulationStudy/Functions/Evaluate.R")
source("3.Thesis/1.SimulationStudy/Functions/PCA_convergence.R")

# simulation parameters
populationsize <- 1000 #n of simulated dataset
n.iter <- 50 #nr of iterations (varying 1:n.iter)
n.sim <- 100 #nr of simulations per iteration value
true_effect <- 2 #regression coefficient to be estimated
true_mean <- true_sd <- NA
miss_prop <- .05

# start simulation study
set.seed(1111)

###

# simulate data once
data <- data.simulation(n = populationsize, true_effect)

# create patterns object with multivariate missingness
amp_patterns <-
  expand.grid(c(0, 1), c(0, 1), c(0, 1), c(0, 1)) %>% .[c(-1,-16), ]
names(amp_patterns) <- ampute(data)$patterns %>% names()

# combine separate functions into wrapper
simulate <- function(data, n.iter, true_effect, patterns, prop) {
  
  # remove values at random with 20 percent probability to be missing
  ampdata <-
    ampute(data,
           patterns = amp_patterns,
           prop = miss_prop,
           mech = "MCAR")$amp
  
  # object for output
  res <- list()
  # repeat mi procedure 'runs'  times for each nr of iterations
  #for (run in 1:runs) {
  for (i in 1:n.iter) {
    res[[i]] <- test.impute(true_effect, data = ampdata, maxit = i)
  }
  #}

    # output
  names(res) <- 1:n.iter
  res
}

#######################
####### Start #########
#######################

for (miss_prop in c(.05, .25, .5, .75, .95)) {


# simulate
out <-
  replicate(
    n.sim,
    simulate(
      data = data,
      n.iter = n.iter,
      true_effect = true_effect,
      patterns = amp_patterns,
      prop = miss_prop
    ),
    simplify = FALSE
  )

###

# evaluate
results_without_CI <- evaluate.sim(sims = out, n.iter = n.iter)

# # uncomment for MCMC SEs
# MCMCSE <- evaluate.sim(sims = out, n.iter = n.iter, mean_or_SE = "se")
# # combine into one dataframe
# results_with_SE <- left_join(results, MCMCSE, by = "T", suffix = c("", ".SE"))

# results with empirical CI
CI_lower <-
  evaluate.sim(sims = out,
               n.iter = n.iter,
               mean_or_SE = "lower")
CI_upper <-
  evaluate.sim(sims = out,
               n.iter = n.iter,
               mean_or_SE = "upper")
results <-
  results_without_CI %>% left_join(CI_lower, by = "T", suffix = c("", ".LL")) %>% left_join(CI_upper, by = "T", suffix = c("", ".UL"))

# add convergence diagnostics for PCA
PCA_results <- PCA_convergence(out)
results <- cbind(results, PCA_results) %>% mutate(miss = miss_prop*100)

### 

# save with other missingness proportions
if (miss_prop == .05){
  dat <- results
  } else {
  load("3.Thesis/1.SimulationStudy/Results/test.Rdata")
  dat <- rbind(dat, results)}

save(dat, file = "3.Thesis/1.SimulationStudy/Results/test.Rdata")
}
