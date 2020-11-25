# plot md pattern of incomplete data
#' Title
#'
#' @param dat An incomplete dataset of class dataframe
#'
#' @return
#' @export
plot_md_pattern <- function(dat) {
  # get md pattern and store additional info
  pat <- mice::md.pattern(dat, plot = FALSE)
  vrb <- colnames(pat)[-ncol(pat)]
  colnames(pat) <- c(vrb, "NA_per_pat")
  pat_freq <- as.numeric(rownames(pat))[-nrow(pat)]
  NA_per_pat <- as.numeric(pat[, ncol(pat)])[-nrow(pat)]
  NA_per_vrb <- as.numeric(pat[nrow(pat), ])[-ncol(pat)]
  NA_total <- pat[nrow(pat), ncol(pat)]
  # make the pattern tidy
  long_pat <- pat[-nrow(pat), ] %>%
    cbind(., pat_freq, pat_nr = 1:nrow(.)) %>%
    as.data.frame() %>%
    tidyr::pivot_longer(cols = all_of(vrb),
                        names_to = "vrb",
                        values_to = "obs") %>%
    cbind(., NA_per_vrb)
  # plot the md pattern
  p <- long_pat %>%
    ggplot2::ggplot() +
    ggplot2::geom_tile(
      ggplot2::aes(
        x = vrb,
        y = pat_nr,
        fill = as.factor(obs),
        group = NA_per_pat,
        text = paste('pat_freq: ', pat_freq,
                     '</br>NA_per_vrb: ', NA_per_vrb)
      ),
      color = "black"
    ) +
    # set axes
    ggplot2::scale_x_discrete(
      limits = vrb,
      position = "bottom",
      labels = as.character(NA_per_vrb),
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_reverse(
      breaks = 1:max(long_pat$pat_nr),
      labels = as.character(pat_freq),
      expand = c(0, 0),
      sec.axis = ggplot2::dup_axis(labels = as.character(NA_per_pat),
                                   name = "Number of missing entries per pattern")
    ) +
    # add labels
    ggplot2::labs(
      x = "Number of missing entries per variable",
      y = "Pattern frequency",
      title = paste0(
        "Missing data pattern (total number of missing cells = ",
        NA_total,
        ")\n"
      )
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = vrb,
        y = -Inf,
        label = abbreviate(.data[["vrb"]])
      ),
      data = long_pat[1:length(vrb),],
      vjust = -0.5
    ) +
    #ggrepel::geom_text_repel(ggplot2::aes(x = vrb, y = -Inf, label = vrb)) +
    ggplot2::coord_cartesian(clip = "off") +
    # add styling
    ggplot2::theme(
      legend.position = "none",
      plot.margin = ggplot2::margin(
        t = 20,
        l = 10,
        b = 10,
        r = 10,
        unit = "pt"
      ),
      axis.title.y.right = ggplot2::element_text(margin = ggplot2::margin(l = 10)),
      panel.grid = ggplot2::element_blank()
    ) +
    ggplot2::scale_fill_manual(values = c("1" = mice:::mdc(1), "0" = mice:::mdc(2)))
  # output
  return(p)
}

# plot in- and outflux of incomplete data
#' Title
#'
#' @param dat An incomplete dataset of class dataframe
#'
#' @return
#' @export
plot_flux <- function(dat) {
  flx <- mice::flux(dat) %>% cbind(variable = rownames(.))
  p <- flx %>% ggplot2::ggplot() +
    ggplot2::geom_text(ggplot2::aes(x = influx,
                                    y = outflux,
                                    label = variable)) +
    ggplot2::geom_abline(intercept = 1,
                         slope = -1,
                         linetype = "dashed") +
    ggplot2::lims(x = c(0, 1), y = c(0, 1)) +
    ggplot2::theme_classic()
  # output
  return(p)
}

# plot the predictor matrix for the imputation model
#' Title
#'
#' @param d An incomplete dataset of class dataframe, a multiply imputed data set of class mids, or a predictor matrix
#'
#' @return
#' @export
plot_pred_matrix <- function(d) {
  # parse input
  if (is(d, "mids")) {
    pred <- d$predictorMatrix
  } else if (is(d, "matrix")) {
    pred <- d
  } else {
    pred <- mice::mice(d, maxit = 0) %>% .$predictorMatrix
  }
  # make the data tidy to plot
  long_pred <- pred %>%
    as.data.frame() %>%
    cbind(vrb_to_imp = rownames(.), .) %>%
    tidyr::pivot_longer(
      cols = names(.)[-1],
      names_to = "predictor",
      names_transform = list(predictor = as.factor)
    )
  # plot the predictor matrix
  p <- long_pred %>%
    ggplot2::ggplot() +
    ggplot2::geom_tile(ggplot2::aes(
      x = predictor,
      y = vrb_to_imp,
      fill = as.factor(value)
    ),
    color = "black") +
    ggplot2::theme(legend.position = "none",
                   panel.grid = ggplot2::element_blank()) +
    ggplot2::scale_y_discrete(
      limits = rev(colnames(pred)),
      expand = c(0, 0),
      label = abbreviate(rev(colnames(pred)))
    ) +
    ggplot2::scale_x_discrete(
      limits = colnames(pred),
      position = "top",
      expand = c(0, 0),
      label = abbreviate(colnames(pred))
    ) +
    ggplot2::scale_fill_manual(values = c("1" = mice:::mdc(1), "0" = mice:::mdc(2))) +
    ggplot2::labs(x = "Predictor", y = "Variable to impute")#, title = "Predictor matrix")
  # output
  return(p)
}

# traceplot: make chain means and variances tidy
#' Title
#'
#' @param imp A multiply imputed data set (mids) object
#'
#' @return
#' @export
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
#' Title
#'
#' @param d A dataframe with preprocessed imputation chains generated via the function `preprocess_thetas()`
#' @param x A variable to plot
#'
#' @return
#' @export
trace_one_variable <- function(d, x) {
  # escape function if no variable is selected
  if (x == "Select a variable") {
    return(ggplot2::ggplot(d) + ggplot2::ggtitle("Please select variable(s)"))
  }
  
  # select one variable and plot it
  p <- d[d$var == x, ] %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x = .it, y = value, color = .imp)) +
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
        x = median(as.numeric(d$.imp)),
        y = 0,
        label = "No \n imputations \n to show"
      ),
      color = "grey")
  }
  # output
  return(p)
}