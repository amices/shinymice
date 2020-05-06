# Convergence function to combine Rhat and AC
# Requires 'dplyr', and the functions in 'AC.R', 'AC_supplement.R', 'Rhat.R', and 'Rhat_supplement.R'

convergence <- function(x, include_acf = FALSE) {
  out <-  left_join(rhat_adapted(x), ac_adapted(x), by = "iteration") 
  if(include_acf){ out <- left_join(out, ac_adapted(x, "acf"), by = "iteration")
  }
  return(out)
}
