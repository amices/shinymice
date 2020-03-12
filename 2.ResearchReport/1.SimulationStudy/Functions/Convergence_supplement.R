# Supplementary functions for Rhat function (see Convergence.R)
# To get folded, split-half, rank-normalized Rhat

# Creating a conflict because I want to keep the master files of this folder, does this work?

# fold chains for rhat of tails
fold_sims <- function(sims) {
  # fold Markov chain for rhat of tails, adapted from rstan
  abs(sims - median(sims))
}

# split chains with maxit > 4 to detect trending
split_chains <- function(sims) {
  # split Markov chains, adapted from rstan
  niter <- dim(sims)[1]
  # do not split if result will be chains of length 1
  if (niter < 4)
    return(sims)
  # split each chain to get 2m chains
  half <- niter / 2
  cbind(sims[1:floor(half), ], sims[ceiling(half + 1):niter, ])
}

# rank-normalize chains because Gelman says so
z_scale <- function(x) {
  # rank-normalize Markov chain, copied from rstan
  S <- length(x)
  r <- rank(x, ties.method = 'average')
  z <- qnorm((r - 1 / 2) / S)
  if (!is.null(dim(x))) {
    # output should have the input dimension
    z <- array(z, dim = dim(x), dimnames = dimnames(x))
  }
  z
}

# compute rhat
get.rhat <- function(sims, maxit = maxit) {
  # compute potential scale reduction factor (rhat) for each variable in mids object
  # equations adapted from Vehtari et al. (2019)
  var_between <-
    maxit * var(apply(sims, 2, mean))
  var_within <- mean(apply(sims, 2, var))
  rhat <-
    sqrt((var_between / var_within + maxit - 1) / maxit)
  return(rhat)
}