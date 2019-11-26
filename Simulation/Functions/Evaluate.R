# Extract aggregated simulation results by averaging over runs

evaluate.sim <- function(sims) {
  # organize output for evaluation
  dt <- map(sims, as.data.table) %>% rbindlist(fill = TRUE)
  d <- dt[, lapply(.SD, mean)] %>% as.data.frame
  tab <- 1:n.iter %>% as.data.frame()
  vars <- c("bias", "CIW", "cov", "R_mean", "R_var", "AC_mean", "AC_var")
  for (var in vars) {
    tab <- cbind(tab, d[,grep(var, names(d))] %>% t) 
  }
  rownames(tab) <- NULL
  colnames(tab) <- c("It.", vars)
  
  #output
  return(tab)
}

