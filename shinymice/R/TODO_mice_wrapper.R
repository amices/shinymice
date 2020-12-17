# mice wrapper function to compute pca convergence
mice_wrapper <- function(dat, ...){
imp <- mice::mice(dat, maxit = 1)
lambda <- imp %>% 
  mice::complete("all") %>% 
  purrr::map_dbl(., ~ {
    data.matrix(.) %>% 
      princomp(., cor = TRUE) %>%
      .$sdev %>%
      .[1] %>%
      . ^ 2
  })
imp <- mice::mice.mids(imp)
}