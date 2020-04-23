# Extract convergence diagnostic autocorrelation from iteration object (e.g., chain means)
# Requires packages 'purrr' and 'magrittr'/'dplyr', and the functions in AC_supplement.R

# NB. This function can be adjusted to show the nr. of chains with significant auto-correlations,
# output will then be the fraction of imputation chains with significant autocorrelation at lag 1.
# To get this, add:
# # critical value significance 
# AC_lim <- qnorm((1 + .95) / 2) / sqrt(maxit)

# function for lag 1 AC for one or more imputation chains
ac_adapted <- function(x, ac_function = "ac") {
  # input: object with theta values (rows are iterations, columns are imputations),
  # and preferred way of calculating ac values
  # output: autocorrelations at each iteration in x, computed per imputation chain
  # and with max and mean across imputations
  
  # set function to apply
  if (ac_function == "acf") {
    function_to_apply <- acf_lag1
  } else {
    function_to_apply <- ac_lag1
  }
  
  # make input ready for mapping
  if (is.numeric(x)) {
    x <- as.data.frame(x)
  }
  t <- dim(x)[1] #nr of iterations
  M <- dim(x)[2] #nr of imputations
  
  # compute ac for each iteration (by definition only possible for t > 2)
  if (t < 3) {
    ac <-
      matrix(NA, t, M + 2) %>% as.data.frame %>% set_names(paste0(ac_function, ".", c("mean", "max", names(x))))
  } else {
    ac <- map_dfr(3:t, function(it) {
      # compute ac
      x[1:it, ] %>% function_to_apply() %>% t() %>% data.frame(mean(.), max(.), .) %>% set_names(c(
        paste0(ac_function, ".mean"),
        paste0(ac_function, ".max"),
        names(.)[-(1:2)]
      ))
    }) %>% rbind(NA, NA, .) %>% cbind(iteration = 1:t, .)
  }
  
  # output
return(ac)
}

