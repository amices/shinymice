# Track convergence of PCA component across iterations
# requires the package 'dplyr'/'margrittr'
# input is the 'out' object obtained by running '1.Execute.R'

PCA_convergence <- function(sims, maxit = n.iter, reps = n.sim, m = 5) { 
  PCAs <-
    map(sims, as.data.table) %>% rbindlist(fill = TRUE) %>% .[, grep("pca", names(.)), with = FALSE] %>% t() %>% as.data.frame()
  sim1 <- map(PCAs, ~{
    matrix(., nrow = m, ncol = maxit) %>% t() %>% as.data.frame()
    }) 
  
  rh <- ac <- matrix(NA, maxit, reps)
  R.PCA <- AC.PCA <- matrix(NA, maxit, 1)
  
  # loop over nr of simulations
  for (k in 1:reps) {
    
  # loop over nr of iterations
  for (it in 2:maxit){
  # auto-correlation
  ac[it, k] <-
   sim1[[k]][1:it,] %>% acf(lag.max = 1, plot = FALSE) %>% .$acf %>% .[2, , ] %>% diag() %>% mean()
  # r hat
  rhat_bulk <-
    sim1[[k]][1:it,] %>% as.matrix() %>% split_chains() %>% z_scale() %>% get.rhat(maxit = it)
  rhat_tail <-
    sim1[[k]][1:it,] %>% as.matrix() %>% fold_sims() %>% split_chains() %>% z_scale() %>% get.rhat(maxit = it)
  rh[it, k] <- max(rhat_bulk, rhat_tail)
  
  # average of reps
  R.PCA[it] <- mean(rh[it,])
  AC.PCA[it] <- mean(ac[it,])
  }
    }
  return(data.frame(R.PCA = R.PCA, AC.PCA = AC.PCA))
}

