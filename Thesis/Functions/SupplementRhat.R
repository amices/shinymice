# Supplementary functions for Rhat function (see Rhat.R)
# To get folded, split-half, rank-normalized Rhat

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