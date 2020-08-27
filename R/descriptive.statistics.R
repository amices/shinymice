# create descriptive stats table

num_desc <- function(d, v) {
  m <- mean(d[[v]], na.rm = TRUE)
  s <- sd(d[[v]], na.rm = TRUE)
  na <- sum(is.na(d[[v]]))
  return(data.frame(
    mean = round(m, 2),
    sd = round(s, 2),
    mode = "",
    freq = "",
    nmiss = na
  )) #%>% round(.,2))
}

cat_desc <- function(d, v) {
  t <- table(d[[v]])
  m <- names(which.max(t))
  p <- t[[m]]#/sum(t)*100
  na <- sum(is.na(d[[v]]))
  
  return(data.frame(
    mean = "",
    sd = "",
    mode = m,
    freq = round(p, 2),
    nmiss = na
  ))
}

descriptives <- function(data) {
  nums <- data %>%
    select(where(is.numeric)) %>%
    names(.) %>%
    purrr::map_dfr( ~ {
      num_desc(d = data, v = .x) %>%
        cbind(column = .x, .)
    })
  cats <- data %>%
    select(!(where(is.numeric))) %>%
    names(.) %>%
    purrr::map_dfr( ~ {
      cat_desc(d = data, v = .x) %>%
        cbind(column = .x, .)
    })
  return(rbind(nums, cats))
}


descr <- function(dat) {
  if (is.data.frame(dat)) {
    tab <- dat %>% psych::describe() %>%
      .[, -1] %>%
      cbind(vars = rownames(.), .)
  }
  if (mice::is.mids(dat)) {
    compl <- mice::complete(imp, "all")
    
    tab <- purrr::map_df(compl, ~ {
      psych::describe(.x) %>%
        mutate(vars = factor(rownames(.), levels = rownames(.)))
    }) %>%
      aggregate(. ~ vars, data = ., mean)
  }
  return(tab)
}

# test
dat <- mice::boys
imp <- mice::mice(dat)

# compare
a <- descr(dat)
b <- descr(imp)
b[, -1] - a[, -1]

# add extra info about categories
v = "phb"

forcats::fct_lump(dat[[v]], n = 3) %>%
  table() %>%
  t() %>%
  as.data.frame() %>%
  .[, -1] %>%
  setNames(., c("level", "freq")) %>%
  cbind(column = v, .)

least <- min(table(dat[[v]]))
nmis <- sum(is.na(dat[[v]]))
p <- least / (sum(most$freq) + nmis) * 100

# table(mice::boys$reg) %>% #, useNA = "always") %>%
#   t() %>%
#   as.data.frame() %>%
#   .[,-1] %>%
#   setNames(., c("level", "freq")) %>%
#   mutate(freq = sort(freq, decreasing = TRUE))


# top 4 and NA + perc cases in least populated cat
# and use psych::describe()
