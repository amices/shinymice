# plot md pattern of incomplete data
plot_md_pattern <- function(dat) {
  # get md pattern and store additional info
  pat <- mice::md.pattern(dat, plot = FALSE)
  vrb <- colnames(pat)[-ncol(pat)]
  colnames(pat) <- c(vrb, "n_vrb_inc")
  n_pat_obs <- as.numeric(rownames(pat))[-nrow(pat)]
  n_vrb_inc <- as.numeric(pat[, ncol(pat)])[-nrow(pat)]
  n_val_mis <- as.numeric(pat[nrow(pat), ])[-ncol(pat)]
  # make the pattern tidy
  long_pat <- pat[-nrow(pat), ] %>%
    cbind(., n_pat_obs, pat_nr = 1:nrow(.)) %>%
    as.data.frame() %>%
    tidyr::pivot_longer(cols = all_of(vrb),
                        names_to = "vrb",
                        values_to = "obs") %>%
    cbind(., n_val_mis)
  # plot the md pattern
  p <- long_pat %>%
    ggplot2::ggplot() +
    ggplot2::geom_tile(ggplot2::aes(
      x = vrb,
      y = pat_nr,
      fill = as.factor(obs),
      group = n_vrb_inc
    ),
    color = "black") +
    # set axes
    ggplot2::scale_x_discrete(limits = vrb,
                              position = "bottom",
                              labels = as.character(n_val_mis)) +
    ggplot2::scale_y_reverse(
      breaks = 1:max(long_pat$pat_nr),
      labels = as.character(n_pat_obs),
      expand = c(0.01, 0.01),
      sec.axis = ggplot2::dup_axis(labels = as.character(n_vrb_inc),
                                   name = "Incomplete variables (per pattern)")
    ) +
    # add labels
    ggplot2::labs(x = "Incomplete cases (per variable)",
                  y = "Pattern frequency") +
    ggplot2::geom_text(aes(x = vrb, y = -Inf, label = vrb),
                       data = long_pat,
                       vjust = -0.5) +
    ggplot2::coord_cartesian(clip = "off") +
    # add styling
    ggplot2::theme(
      legend.position = "none",
      plot.margin = ggplot2::margin(t = 25, l = 10, b = 10, r = 10, unit = "pt"),
      axis.title.y.right = ggplot2::element_text(margin = ggplot2::margin(l = 10)),
      panel.grid = ggplot2::element_blank()
    ) +
    ggplot2::scale_fill_manual(values = c("1" = mice:::mdc(1), "0" = mice:::mdc(2)))
  # output
  return(p)
}

# plot in- and outflux of incomplete data
plot_flux <- function(dat) {
  flx <- mice::flux(dat)
  p <- flx %>% ggplot2::ggplot() +
    ggplot2::geom_text(ggplot2::aes(
      x = influx,
      y = outflux,
      label = rownames(flx)
    )) +
    ggplot2::geom_abline(intercept = 1,
                         slope = -1,
                         linetype = "dashed") +
    ggplot2::lims(x = c(0, 1), y = c(0, 1)) +
    ggplot2::theme_classic()
  # output
  return(p)
}

# plot the predictor matrix for the imputation model
plot_pred_matrix <- function(dat) {
  # parse input
  if (mice::is.mids(dat)) {
    imp <- dat
  } else {
    imp <- mice::mice(dat, maxit = 0)
  }
  # get the predictor matrix and add the rownames as variable
  pred <- imp$predictorMatrix %>%
    as.data.frame() %>%
    cbind(vrb_to_imp = rownames(.), .)
  # make the data tidy to plot
  long_pred <-
    pivot_longer(
      pred,
      cols = names(pred)[-1],
      names_to = "vrb_as_pred",
      names_transform = list(vrb_as_pred = as.factor)
    )
  # plot the predictor matrix
  p <- long_pred %>%
    ggplot2::ggplot() +
    ggplot2::geom_tile(ggplot2::aes(
      x = vrb_as_pred,
      y = vrb_to_imp,
      fill = as.factor(value)
    ),
    color = "black") +
    ggplot2::theme(legend.position = "none",
                   panel.grid = ggplot2::element_blank()) +
    ggplot2::scale_y_discrete(limits = rev(names(imp$data))) +
    ggplot2::scale_x_discrete(limits = names(imp$data), position = "top") +
    ggplot2::scale_fill_manual(values = c("1" = mice:::mdc(1), "0" = mice:::mdc(2))) +
    ggplot2::labs(x = "Predictor", y = "Variable to impute")#, title = "Predictor matrix")
  # output
  return(p)
}

# traceplot: make chain means and variances tidy
preprocess_thetas <- function(imp) {
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

# traceplot: plot trace of one variable
trace_one_variable <- function(d, x) {
  # select one variable and plot it
  p <- d[d$var == x, ] %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x = .it, y = value, color = .imp)) +
    ggplot2::facet_wrap(~ theta, scales = "free", ncol = 1) +
    ggplot2::theme_classic() +
    ggplot2::theme(strip.background = element_rect(size = 0.5)) +
    ggplot2::labs(x = "Iteration",
                  y = paste0(x),
                  color = "Imputation")
  # show user if the variable is completely observed
  if (all(is.na(p$data$value))) {
    p <-
      p + ggplot2::geom_text(ggplot2::aes(
        x = median(as.numeric(d$.imp)),
        y = 0,
        label = "No \n imputations \n to show"
      ),
      color = "grey")
  }
  # output
  return(p)
}