# extract aggregated simulation results by averaging over runs
# requires the packages 'purrr', 'data.table' and 'magrittr'/'dplyr',
# and the non-convergence diagnostics function 'ComputeDiagnostics.R'

# load function
source("Functions/ComputeDiagnostics.R")

evaluate.sim <-
  # input: the 'out' object obtained by running '1.Execute.R'
  # output: convergence diagnostics and performance measures across repetitions
  
  function(sims,
           mean_or_SE = "mean") {
    # determine whether we want averages per simulation condition, or MCMC SEs, or MCMC CIs
    if (mean_or_SE == "se") {
      function_to_apply <-
        function(x)
          sd(x, na.rm = TRUE) # or use: function(x) sqrt((var(x)))
    } else if (mean_or_SE == "lower") {
      function_to_apply <- function(x)
        quantile(x, .025, na.rm = TRUE)
    } else if (mean_or_SE == "upper") {
      function_to_apply <- function(x)
        quantile(x, .975, na.rm = TRUE)
    } else {
      function_to_apply <- function(x)
        mean(x, na.rm = TRUE)
    }
    
    # for all variables, average over repetitions with the same missingness proportion and number of iterations
    without_conv <-
      map_df(sims, ~ {
        as.data.frame(.)
      }) %>% aggregate(. ~ t + p, data = ., function_to_apply)
    
    # define which variables do need convergence diagnostics
    thetas <-
      c(
        "chain.mean.X1",
        "chain.mean.X2",
        "chain.mean.X3",
        "chain.mean.Y",
        "chain.var.X1",
        "chain.var.X2",
        "chain.var.X3",
        "chain.var.Y",
        "pca",
        "beta"
      )
    
    # for the defined thetas, compute Rhat and AC at each iteration,
    # then average over repetitions with the same missingness proportion and number of iterations
    conv <-
      map_df(sims, function(rep) {
        # per repetition
        # # to test use this: map(1:2, function(rep){sims[[rep]] %>% ...})
        rep %>% base::split(as.factor(.$p)) %>%
          map_dfr(., function(mis) {
            # per missingness proportion
            map_dfc(thetas, function(vrb) {
              # per variable
              mis %>% .[, grep(vrb, names(.))] %>% convergence() %>% .[,-1] %>% set_names(paste0(names(.), ".", vrb))
            })
          }) %>% cbind(rep[, 1:2], .)
      }) %>% aggregate(. ~ t + p,
                       data = .,
                       function_to_apply,
                       na.action = na.pass) %>% dplyr::na_if(., "NaN")
    
    
    #output
    return(left_join(without_conv, conv, by = c("t", "p")))
  }
