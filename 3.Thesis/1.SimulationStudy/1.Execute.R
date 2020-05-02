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

# load simulation/evaluation functions
source("3.Thesis/1.SimulationStudy/Functions/CreateData.R")
source("3.Thesis/1.SimulationStudy/Functions/Impute.R")
source("3.Thesis/1.SimulationStudy/Functions/Evaluate.R")
source("3.Thesis/1.SimulationStudy/Functions/Convergence.R")
source("3.Thesis/1.SimulationStudy/Functions/Rhat.R")
source("3.Thesis/1.SimulationStudy/Functions/Rhat_supplement.R")
source("3.Thesis/1.SimulationStudy/Functions/AC.R")
source("3.Thesis/1.SimulationStudy/Functions/AC_supplement.R")

# simulation parameters
populationsize <- 1000 #n of simulated dataset
n.iter <- 100 #nr of iterations (varying 1:n.iter)
n.sim <- 1000 #nr of simulations per iteration value
p.miss <- c(.05, .25, .5, .75, .95)
true.effect <- 2 #regression coefficient to be estimated

# start simulation study
set.seed(1111)

###

# simulate data once
data <- data.simulation(n = populationsize, true.effect)

# create patterns object with multivariate missingness
amp.pat <-
  expand.grid(c(0, 1), c(0, 1), c(0, 1), c(0, 1)) %>% .[c(-1, -16),]
names(amp.pat) <- ampute(data)$patterns %>% names()

# combine separate functions into wrapper
simulate <- function(complete_data = data,
                     amp_patterns = amp.pat,
                     mis_prop = p.miss,
                     final_it = n.iter,
                     ...) {
  # ampute the complete data with each missingness proportion
  amps <- map(mis_prop, function(mis) {
    ampute(
      data = complete_data,
      prop = mis,
      patterns = amp_patterns,
      mech = "MCAR"
    )$amp
  }) %>% set_names(as.character(mis_prop))
  
  # with amputed datasets (as many as there are missingess proportions), impute missingness and compute diagnostics for every nr. of iterations
  imps <-
    map_df(mis_prop, function(mis) {
      map_df(1:n.iter, function(it) {
        test.impute(amp_data = amps[[as.character(mis)]],
                    it_nr = it,
                    final_it = n.iter) %>% cbind(p = mis, .)
      }) %>% cbind(chain_means, chain_vars)
    })
  # save seed
  seed<<-.Random.seed
  
  return(imps)
}

#######################
####### Start #########
#######################

# simulate
out <-
  replicate(n.sim,
            simulate(complete_data = data),
            simplify = FALSE)


###

# evaluate
results_without_CI <- evaluate.sim(sims = out)

# # uncomment for MCMC SEs
# MCMCSE <- evaluate.sim(sims = out, n.iter = n.iter, mean_or_SE = "se")
# # combine into one dataframe
# results_with_SE <- left_join(results, MCMCSE, by = "T", suffix = c("", ".SE"))

# results with empirical CI
CI_lower <-
  evaluate.sim(sims = out, mean_or_SE = "lower")
CI_upper <-
  evaluate.sim(sims = out, mean_or_SE = "upper")
results <-
  results_without_CI %>% left_join(CI_lower,
                                   by = c("t", "p"),
                                   suffix = c("", ".LL")) %>% left_join(CI_upper,
                                                                        by = c("t", "p"),
                                                                        suffix = c("", ".UL"))

# save results
save(out, file = "3.Thesis/1.SimulationStudy/Results/raw.Rdata")
save(results, file = "3.Thesis/1.SimulationStudy/Results/complete.Rdata")
