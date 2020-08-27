# create descriptive stats table

num_desc <- function(d, v){
  m <- mean(d[[v]], na.rm = TRUE) 
  s <- sd(d[[v]], na.rm = TRUE)
  na <- sum(is.na(d[[v]]))
  return(data.frame(mean = round(m, 2), sd = round(s,2), mode = "", perc = "", nmiss = na)) #%>% round(.,2))
}

cat_desc <- function(d, v){
  t <- table(d[[v]])
  m <- names(which.max(t))
  p <- t[[m]]/sum(t)*100
  na <- sum(is.na(d[[v]]))
  
  return(data.frame(mean = "", sd = "", mode = m, perc = round(p, 2), nmiss = na))
}

descriptives <- function(data){
  nums <- data %>% 
    select(where(is.numeric)) %>% 
    names(.) %>% 
    purrr::map_dfr(~{ 
      num_desc(d = data, v = .x) %>% 
        cbind(var = .x, .)})
  cats <- data %>% 
    select(!(where(is.numeric))) %>% 
    names(.) %>% 
    purrr::map_dfr(~{ 
      cat_desc(d = data, v = .x) %>% 
        cbind(var = .x, .)})
  return(rbind(nums, cats))
}

# dat %>% descriptives()

# a <- table(dat$reg, useNA = "always") %>% 
#   t() %>% 
#   as.data.frame() %>% 
#   .[,-1] %>% 
#   setNames(., c("level", "freq"))
