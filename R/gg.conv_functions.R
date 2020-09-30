# convergence functions

########################### 
# rhat
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
    r.hat <- purrr::map_dfr(2:t, function(it) {
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

# fold chains for rhat of tails
fold_sims <- function(sims) {
  # fold Markov chain for rhat of tails, adapted from rstan
  abs(sims - median(sims))
}

# split chains with maxit > 4 to detect trending
split_chains <- function(sims) {
  # split Markov chains, adapted from rstan
  
  # number of iterations
  t <- dim(sims)[1]
  
  # do not split if result will be chains of length 1
  if (t < 4)
    return(sims)
  else {
    # split each chain to get 2m chains
    lower <- 1:floor(t / 2)
    upper <- ceiling((t / 2) + 1):t
    splits <- base::cbind(sims[lower, ], sims[upper, ])
    return(splits)
  }
}

# rank-normalize chains because Gelman says so
z_scale <- function(x) {
  # rank-normalize Markov chain, copied from rstan
  t <- length(x)
  r <- rank(x, ties.method = 'average')
  z <- qnorm((r - 1 / 2) / t)
  
  # output
  if (!is.null(dim(x))) {
    # output should have the input dimensions
    z <- array(z, dim = dim(x), dimnames = dimnames(x))
  }
  return(z)
}

# compute rhat
get.rhat <- function(sims) {
  # compute potential scale reduction factor (rhat) for each variable in mids object
  # equations adapted from Vehtari et al. (2019)
  
  # number of iterations
  t <- length(sims)
  
  # between chain variance
  var_between <-
    t * var(apply(sims, 2, mean))
  
  # within chain variance
  var_within <- mean(apply(sims, 2, var))
  
  # rhat
  rhat <-
    sqrt((var_between / var_within + t - 1) / t)
  
  # output
  return(rhat)
}

########################### 
# function for lag 1 AC for one or more imputation chains
ac_adapted <- function(x, ac_function = "ac") {
  # input: object with theta values (rows are iterations, columns are imputations),
  # and preferred way of calculating ac values
  # output: autocorrelations at each iteration in x, computed per imputation chain
  # and with max and mean across imputations
  
  # set function to apply
  if (ac_function == "acf") {
    function_to_apply <- acf_lag1
    min_it <- 2
  } else {
    function_to_apply <- ac_lag1
    min_it <- 3
  }
  
  # make input ready for mapping
  if (is.numeric(x)) {
    x <- as.data.frame(x)
  }
  it <- dim(x)[1] #nr of iterations
  M <- dim(x)[2] #nr of imputations
  
  # compute ac for each iteration (by definition only possible for t > 2)
  if (it < min_it) {
    ac <-
      matrix(NA, it, M + 2) %>% as.data.frame %>% setNames(paste0(ac_function, ".", c("mean", "max", names(x))))
  } else {
    ac <- purrr::map_dfr(min_it:it, function(i) {
      # compute ac
      x[1:i, ] %>% function_to_apply() %>% t() %>% data.frame(mean(.), max(.), .) %>% setNames(paste0(ac_function, ".", c("mean", "max", 1:M)))
    }) %>% base::rbind(matrix(NA, min_it-1, M+2, dimnames = list(NULL, names(.))), .) %>% base::cbind(iteration = 1:it, .)
  }
  
  # output
  return(ac)
}

# function for lag 1 AC for one or more imputation chains
ac_lag1 <- function(x) {
  # input: object with theta values (rows are iterations, columns are imputations)
  # output: one autocorrelation value per imputation (over the complete set of iterations in x)
  
  # make input ready for mapping
  if (is.numeric(x)) {
    x <- as.data.frame(x)
  }
  
  # compute ac per imputation (column)
  out <- purrr::map_dbl(1:dim(x)[2], function(m) {
    cor(x[-dim(x)[1], m], x[-1, m])
  }) %>% setNames(paste0("ac.", names(x)))
  
  # output
  return(out)
}


# adjusted stats::acf() function for lag 1 AC for one or more imputation chains
acf_lag1 <- function(x) {
  # input: object with theta values (rows are iterations, columns are imputations)
  # output: autocorrelation per imputation, with max and mean across imputations
  
  # make input ready for mapping
  if (is.numeric(x)) {
    x <- as.data.frame(x)
  }
  
  # compute ac
  out <-
    acf(x, lag.max = 1, plot = FALSE) %>% .$acf %>% .[2, , ] %>% as.matrix() %>% diag() %>% setNames(paste0("acf.", names(x)))
  
  
  # output
  return(out)
}
