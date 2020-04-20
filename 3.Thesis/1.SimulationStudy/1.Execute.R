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
n.iter <- 4 #nr of iterations (varying 1:n.iter)
n.sim <- 2 #nr of simulations per iteration value
p.miss <- c(.05, .25, .5, .75, .95)
true_effect <- 2 #regression coefficient to be estimated
true_mean <- true_sd <- NA

# start simulation study
set.seed(1111)

###

# simulate data once
data <- data.simulation(n = populationsize, true_effect)

# create patterns object with multivariate missingness
amp_patterns <-
  expand.grid(c(0, 1), c(0, 1), c(0, 1), c(0, 1)) %>% .[c(-1, -16),]
names(amp_patterns) <- ampute(data)$patterns %>% names()

# combine separate functions into wrapper
simulate <- function(data,
                     n.iter,
                     true_effect,
                     patterns,
                     p.miss) {
  
  # ampute the complete data with each missingness proportion
  amps <- map(p.miss, function(x) {
    ampute(data,
           patterns = amp_patterns,
           prop = x,
           mech = "MCAR")$amp
  })
  
  # with amputed datasets (as many as there are missingess proportions), impute missingness and compute diagnostics for every nr. of iterations
  imps <-
    map_df(p.miss, function(p) {
      map_df(1:n.iter, function(t) {
        test.impute(true_effect, data = amps[[p]], maxit = t, n.iter = n.iter) %>% cbind(p)
      }) %>% cbind(chain_means, chain_vars)
    })
  
  return(imps)
}

#######################
####### Start #########
#######################

# simulate
out <-
  replicate(
    n.sim,
    simulate(
      data = data,
      n.iter = n.iter,
      true_effect = true_effect,
      patterns = amp_patterns,
      p.miss = p.miss
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
  results_without_CI %>% left_join(CI_lower, by = c("t", "p"), suffix = c("", ".LL")) %>% left_join(CI_upper, by = c("t", "p"), suffix = c("", ".UL"))

#####################################
############# make this work again!!
#####################################

# # add convergence diagnostics for PCA
# PCA_results <- PCA_convergence(out)
# dat <-
#   cbind(results, PCA_results)

###

save(dat, file = "3.Thesis/1.SimulationStudy/Results/test.Rdata")
