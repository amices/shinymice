# script to perform simulation study of the MSc thesis 'Missing the Point: 
  # Non-Convergence in Iterative Imputation Algorithms'
# results in the thesis were obtained on 03-05-2010, using R version 3.6.3, 
  # on a Windows 10 personal computer (package versions and random seed can 
  # be found in the script)
# pseudo-code describing the simulation can be found in the thesis manuscript
# author: Hanne Oberman

########################
# set-up environment ###
########################

# load required packages
library(mice) #version 3.8.0
library(miceadds) #version 3.8-9
library(mvtnorm) #version 1.1-0
library(data.table) #version 1.12.8
library(tidyverse) #version 1.3.0
# # if required, just load the necessary tidyverse packages, instead of all
# library(dplyr)
# library(ggplot2)
# library(purrr)
# library(magrittr)

# load simulation/evaluation functions
source("Thesis/Functions/CreateData.R")
source("Thesis/Functions/RunSimulationOnce.R")
source("Thesis/Functions/EvaluateResults.R")

# simulation parameters
populationsize <- 1000 #nr of observations in simulated dataset
n.iter <- 100 #nr of iterations (varying 1:n.iter)
n.sim <- 1000 #nr of simulations per iteration value
p.miss <- c(.05, .25, .5, .75, .95) #proportion of incomplete cases
true.effect <- 2 #regression coefficient to be estimated


########################
# start simulation study
########################

# set random seed
set.seed(1111)

# simulate data once
data <- data.simulation(n = populationsize, true.effect)

# create patterns to ampute the data with multivariate missingness
amp.pat <-
  expand.grid(c(0, 1), c(0, 1), c(0, 1), c(0, 1)) %>% #define all possible combinations of univariate and multivariate missingness
  .[c(-1,-16), ] #remove the completely (un)observed cases  
names(amp.pat) <- ampute(data)$patterns %>% names() #obtain correct names of patterns

# run simulation n.sim times
out <-
  replicate(n = n.sim,
            expr = simulate(complete_data = data),
            simplify = FALSE)


########################
# obtain and save result
########################

# summarize raw results
results <- evaluate.sim(sims = out)

# # calculate empirical CI
# CI_lower <-
#   evaluate.sim(sims = out, mean_or_SE = "lower")
# CI_upper <-
#   evaluate.sim(sims = out, mean_or_SE = "upper")
# 
# # combine with other results
# results <-
#   results %>%
#   left_join(CI_lower,
#             by = c("t", "p"),
#             suffix = c("", ".LL")) %>%
#   left_join(CI_upper,
#             by = c("t", "p"),
#             suffix = c("", ".UL"))

# # if necessary, calculate MCMC SEs
# MCMCSE <- evaluate.sim(sims = out, mean_or_SE = "se")
# # combine with other results
# results <- left_join(results, MCMCSE, by = c("t", "p"), suffix = c("", ".SE"))

# save results
save(results, file = "Results/complete.Rdata")

# # if necessary, save raw results (too large for Github)
# save(out, file = "Results/raw.Rdata")
