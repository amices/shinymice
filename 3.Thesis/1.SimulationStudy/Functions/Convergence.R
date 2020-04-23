# Convergence function to combine Rhat and AC
# Requires 'dplyr', and the functions in 'AC.R', 'AC_supplement.R', 'Rhat.R', and 'Rhat_supplement.R'

convergence <- function(x, ac_function = "ac") {
  left_join(rhat_adapted(x), ac_adapted(x, ac_function), by = "iteration")
}
