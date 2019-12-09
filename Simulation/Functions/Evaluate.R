# Extract aggregated simulation results by averaging over runs

evaluate.sim <- function(sims) {
  # organize output for evaluation
  dt <- map(sims, as.data.table) %>% rbindlist(fill = TRUE)
  d <- dt[, lapply(.SD, mean)] %>% as.data.frame
  tab <- 1:n.iter %>% as.data.frame()
  vars <- c("bias", "CIW", "cov", "R_mean_X", "R_mean_Z1", "R_mean_Z2", "R_mean_Y", "R_var_X", "R_var_Z1", "R_var_Z2", "R_var_Y", "AC_mean_X", "AC_mean_Z1", "AC_mean_Z2", "AC_mean_Y", "AC_var_X", "AC_var_Z1", "AC_var_Z2", "AC_var_Y")
  for (var in vars) {
    tab <- cbind(tab, d[,grep(var, names(d))] %>% t) 
  }
  rownames(tab) <- NULL
  colnames(tab) <- c("It.", vars)
  
  #output
  return(tab)
}

