# create descriptive stats table

dat <- mice::boys
dat %>% purrr::map(~{mean(.x, na.rm = TRUE)})

num_desc <- function(v){
  m <- mean(v, na.rm = TRUE)
  s <- sd(v, na.rm = TRUE)
  na <- sum(is.na(v))
  return(data.frame(mean = m, sd = s, miss = na))
  }

dat %>% 
  #setNames(., names(.)) %>% 
  select(where(is.numeric)) %>% 
  purrr::map_dfr(~{num_desc(.x)})

cat_desc <- function(v){
  #m <- mean(v, na.rm = TRUE)
  #s <- sd(v, na.rm = TRUE)
  na <- sum(is.na(v))
  return(data.frame(miss = na))
  }

dat %>% 
  setNames(., names(.)) %>% 
  select(!where(is.numeric)) %>% 
  purrr::map_dfr(~{cat_desc(.x) %>% cbind(var = .x, .)})
