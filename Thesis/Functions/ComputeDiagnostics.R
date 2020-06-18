# function to compute Rhat conform Vehtari et al (2019) and calculate AC manually
# requires 'dplyr', and the functions in 'ComputeAC.R', and 'ComputeRhat.R'

# load functions
source('Thesis/Functions/ComputeRhat.R')
source('Thesis/Functions/ComputeAC.R')

# combine the two diagnostics
convergence <- function(x, include_acf = FALSE) {
  # input: object with imputation chain(s), e.g. chain mean, choice of calculating default AC
  # output: AC values at each iteration, computed manually, if necessary also using stats:acf() 
  out <-  left_join(rhat_adapted(x), ac_adapted(x), by = "iteration") 
  if(include_acf){ out <- left_join(out, ac_adapted(x, "acf"), by = "iteration")
  }
  return(out)
}
