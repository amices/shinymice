# Extract aggregated simulation results by averaging over runs
# requires the packages "purrr" and "data.table"

evaluate.sim <-
  function(sims,
           n.iter,
           mean_or_SE = "mean"
           ) {
    # organize output for evaluation
    dt <- map(sims, as.data.table) %>% rbindlist(fill = TRUE)
    # determine whether we want averages per simulation condition or MCMC SEs
    if (mean_or_SE == "se") {
      function_to_apply <- sd # or use: function(x) sqrt((var(x)))
    } else if (mean_or_SE == "lower") {
      function_to_apply <- function(x) quantile(x, .025, na.rm = TRUE)
    } else if (mean_or_SE == "upper") {
      function_to_apply <- function(x) quantile(x, .975, na.rm = TRUE)
    } else {function_to_apply <- mean}

    
    # apply function to aggregate simulation runs per simulation condition
    d <- dt[, lapply(.SD, function_to_apply)] %>% as.data.frame
    # create object for output
    output <- 1:n.iter %>% as.data.frame()
    # add names
    vars <- names(sims[[1]][[1]])
    for (var in vars) {
      output <- cbind(output, d[, grep(var, names(d))] %>% t)
    }
    rownames(output) <- NULL
    colnames(output) <- c("T", vars)
    
    #output
    return(output)
  }
