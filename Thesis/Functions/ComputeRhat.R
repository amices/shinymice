# extract convergence diagnostic Rhat from iteration object (e.g., chain means)
# requires packages 'purrr' and 'magrittr'/'dplyr', and the functions in SupplementRhat.R

# load function
source('Thesis/Functions/SupplementRhat.R')

# function for Rhat for two or more imputation chains
rhat_adapted <- function(x) {
  # input: object with theta values (rows are iterations, columns are imputations)
  # output: convergence diagnostic Rhat across imputations
  
  # make input ready for mapping
  if (is.data.frame(x)) {
    x <- as.matrix(x)
  }
  
  # quit function if there are not enough iterations
  t <- dim(x)[1]
  if (is.null(t)) {
    r.hat <- data.frame(iteration = 1,
                        max.r.hat = NA,
                        r.hat = NA)
  }
  else {
    r.hat <- map_dfr(2:t, function(it) {
      # compute original r hat conform Gelman and Rubin (1992) 
      rhat_original <- x[1:it,] %>% get.rhat()
      # compute r hat in all ways described by Vehtari et al. (2019)
      rhat_bulk <-
        x[1:it,] %>% split_chains() %>% z_scale() %>% get.rhat()
      rhat_tail <-
        x[1:it,] %>% fold_sims() %>% split_chains() %>% z_scale() %>% get.rhat()
      max(rhat_bulk, rhat_tail) %>% data.frame(r.hat.max = ., r.hat = rhat_original) #%>% set_names("r.hat")
    }) %>% rbind(NA, .) %>% cbind(iteration = 1:t, .)
  }
  # output
  return(r.hat)
}
