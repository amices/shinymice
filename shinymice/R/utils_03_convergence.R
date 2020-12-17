
#' Title Preprocess imputation chains (i.e., chain means, chain variances) for traceplot
#'
#' @param imp A multiply imputed data set (mids) object
#'
#' @return A dataframe with tidy convergence parameters for all variables
preprocess_thetas <- function(imp) {
  if (!any(imp$nmis > 0)) {
    return(NULL)
  }
  # preprocess chain means
  long_trace <- imp$chainMean %>%
    dplyr::na_if(., "NaN") %>%
    as.data.frame(.) %>%
    dplyr::mutate(var = row.names(.), theta = "Chain means") %>%
    # preprocess chain variances
    rbind(
      .,
      imp$chainVar %>%
        sqrt(.) %>%
        as.data.frame(.) %>%
        dplyr::mutate(var = row.names(.), theta = "Chain standard deviations")
    ) %>%
    # convert to long format
    tidyr::pivot_longer(-c(var, theta)) %>%
    cbind(.it = as.integer(1:imp$iteration),
          .imp = as.factor(rep(1:imp$m, each = imp$iteration)))
  # output
  return(long_trace)
}

#' Title Plot convergence parameters for a single variable
#'
#' @param d A dataframe with preprocessed imputation chains generated via the function `preprocess_thetas()`
#' @param x A variable to plot
#'
#' @return A ggplot object visualizing a traceplot
#' @export
plot_trace <- function(d, x) {
  if (is.null(d)) {
    return(plot_a_mouse())
  }
  # select one variable and plot it
  p <- d[d$var == x, ] %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(
      x = as.integer(.it),
      y = value,
      color = .imp
    )) +
    ggplot2::geom_point(ggplot2::aes(
      x = as.integer(.it),
      y = value,
      color = .imp
    ), size = 0.5) +
    ggplot2::facet_wrap(~ theta, scales = "free", ncol = 1) +
    ggplot2::theme_classic() +
    ggplot2::theme(strip.background = ggplot2::element_rect(size = 0.5)) +
    ggplot2::labs(x = "Iteration",
                  y = paste0(x),
                  color = "Imputation")
  # show user if the variable is completely observed
  if (all(is.na(p$data$value))) {
    p <-
      p + ggplot2::geom_text(ggplot2::aes(
        x = mean(1:max(d$.it)),
        y = 0,
        label = "No \n imputations \n to show"
      ),
      color = "grey")
  }
  # output
  return(p)
}

# compute rhat
rhat_functions <- function(sims) {
  # compute potential scale reduction factor (rhat) for each variable in mids object
  # equations adapted from Vehtari et al. (2019)
  # helper functions
  n_it <- length(sims)
  # split chains with maxit > 4 to detect trending
  split_chains <- function(sims) {
    # split Markov chains, adapted from rstan
    n_it <- dim(sims)[1]
    # output
    if (n_it < 4)
      # do not split if result will be chains of length 1
      return(sims)
    else {
      # split each chain to get 2m chains
      lower <- 1:floor(n_it / 2)
      upper <- ceiling((n_it / 2) + 1):n_it
      splits <- base::cbind(sims[lower,], sims[upper,])
      return(splits)
    }
  }
  # rank-normalize chains because Gelman says so
  z_scale <- function(splits) {
    # if there are no imputations, exit function
    if (all(is.na(splits))) {
      return(splits)
    }
    # rank-normalize Markov chain, copied from rstan
    t <- length(splits)
    r <- rank(splits, ties.method = 'average')
    z <- qnorm((r - 1 / 2) / t)
    # output
    if (!is.null(dim(splits))) {
      # output should have the input dimensions
      z <- array(z, dim = dim(splits), dimnames = dimnames(splits))
    }
    return(z)
  }
  # preprocess chains
  sims <- sims %>% split_chains(.) %>% z_scale(.)
  # compute rhat
  var_between <-
    n_it * var(apply(sims, 2, mean))
  var_within <- mean(apply(sims, 2, var))
  rhat <-
    sqrt((var_between / var_within + n_it - 1) / n_it)
  # output
  return(rhat)
}

# function for Rhat for two or more imputation chains
compute_rhat <- function(thetas) {
  # input: object with theta values (rows are iterations, columns are imputations)
  # output: convergence diagnostic Rhat across imputations
  # parse inputs
  if (is.data.frame(thetas)) {
    thetas <- as.matrix(thetas)
  }
  # quit function if there are not enough iterations
  n_it <- dim(thetas)[1]
  if (is.null(n_it)) {
    return(data.frame(
      iteration = 1,
      max.r.hat = NA,
      r.hat = NA
    ))
  } else {
    r.hat <- purrr::map_dfr(2:n_it, function(it) {
      # compute r hat in all ways described by Vehtari et al. (2019)
      rhat_bulk <- thetas[1:it, ] %>%
        rhat_functions(.)
      # for rhat of the tails, fold the chains
      rhat_tail <- abs(thetas[1:it, ] - median(thetas[1:it, ])) %>%
        rhat_functions(.)
      max(rhat_bulk, rhat_tail) %>%
        data.frame(rhat = .)
    }) %>%
      rbind(NA, .) %>%
      cbind(iteration = 1:n_it, .)
  }
  # output
  return(r.hat)
}

#' Title Plot non-convergence diagnostic 'R hat'/potential scale reduction factor/Gelman-Rubin statistic
#'
#' @param imp A multiply imputed data set (mids) object
#' @param x A variable to plot
#' @param theta The parameter to plot (either "means" for chain means, or "vars" for chain variances)
#'
#' @return A ggplot object visualizing non-convergence
#' @export
plot_rhat <- function(imp, x, theta = "means") {
  #parse inputs
  if (theta == "means" | theta == "both") {
    thetas <- imp$chainMean[x, ,]
    thetas[is.nan(thetas)] <- NA
  }
  if (theta == "vars") {
    thetas <- imp$chainVar[x, ,]
  }
  # plot
  if (all(is.na(thetas))) {
    return(
      ggplot2::ggplot(data.frame(thetas)) + 
        ggplot2::ggtitle("Convergence diagnostics cannot be computed \n if the number of iterations is three or less \n (or for completely observed variables)") +
        ggplot2::theme_classic()
    )
  }
  p <- compute_rhat(thetas) %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x = iteration, y = rhat)) +
    ggplot2::geom_hline(yintercept = 1.2,
                        color = "grey",
                        size = 1) +
    ggplot2::theme_classic()
  # optional added second theta
  if (theta == "both") {
    p <-
      p + ggplot2::geom_line(
        ggplot2::aes(x = iteration, y = rhat),
        linetype = "dashed",
        data = compute_rhat(imp$chainVar[x, ,])
      )
  }
  # output
  return(p)
}
