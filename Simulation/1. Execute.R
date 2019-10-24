# load required packages
library(mice)
library(mvtnorm)

# load simulation/evaluation functions
source("Functions/CreateData.R")
source("Functions/Simulate.R")
# source("Functions/xyz.R")

# simulation parameters
set.seed(321)
populationsize <- 100
n.iter <- 10
n.sim <- 10
# bivar.corr <- 0.5

# create and ampute data
data <- data.simulation(n = populationsize, bivar.corr = .5)

# run simulation
sims <- simulate.function(data = data, n.iter = n.iter, n.sim = n.sim)

##### UNDER CONSTRUCTION #####

# Evaluate
evaluate.function <- function(OUT){
  eval.out <- list()
  for (j in OUT[[i]]){
    #calculate your evaluations vector - so Rhat, bias, cov and ciw
    eval.out[[i]] <- yourvector
    # 2. unlist(eval.out)
    # 3. Reduce("+", eval.out) / length(eval.out)
  }
}
#sapply is 'sneller?' en geeft je direct matrix
#sapply(out, evaluate.function) en dan colMeans
  
# evaluate for each simulated iteration in n.iter
# so 100 averaged (in evaluate.function()) simulations
# or use purrr::map()
for (i in OUT){
 evaluate.function
}