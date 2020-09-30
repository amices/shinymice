# plot convergence diagnostics

gg.convergence <- function(x, include_acf = FALSE) {
  # input: object with imputation chain(s), e.g. chain mean, choice of calculating default AC
  # output: AC values at each iteration, computed manually, if necessary also using stats:acf() 
  out <-  dplyr::left_join(rhat_adapted(x), ac_adapted(x), by = "iteration") 
  if(include_acf){ out <- dplyr::left_join(out, ac_adapted(x, "acf"), by = "iteration")
  }
  return(out)
}
