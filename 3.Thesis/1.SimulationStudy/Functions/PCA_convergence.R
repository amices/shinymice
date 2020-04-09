# Track convergence of PCA component across iterations
# requires the package 'dplyr'/'margrittr'
# input is the 'out' object obtained by running '1.Execute.R'

PCA_convergence <- function(sims, maxit = n.iter, reps = n.sim, m = 5) { 
  PCAs <-
    map(sims, as.data.table) %>% rbindlist(fill = TRUE) %>% .[, grep("pca", names(.)), with = FALSE]
  # sim1 <- PCAs[1,] %>% matrix(nrow = 5, ncol = 50) %>% t() %>% as.data.frame()
  #sim1 %>% split_chains() %>% z_scale() %>% get.rhat(maxit = maxit)
  #ac <- sim1[,1] %>%  as.numeric() %>% acf(lag.max = 1, plot = F) %>% .$acf %>% .[2]
  # ac <- sim1 %>% acf(lag.max = 1, plot = FALSE) %>% .$acf %>% .[2,,] %>% diag()
  ac <- list()
  for (it in 1:maxit){
  ac[[it]] <-
    PCAs[1, ] %>% matrix(nrow = m, ncol = maxit) %>% t() %>% as.data.frame() %>% acf(lag.max = 1, plot = FALSE) %>% .$acf %>% .[2, , ] %>% diag()
  }
  return(ac)
}

PCA_convergence(out)
