# descriptive statistics for incomplete data
#' Title
#'
#' @param d
#'
#' @return
#' @export
#'
#' @examples
descr_NA <- function(d) {
  tab <-
    psych::describe(d, skew = FALSE)[, c("n", "mean", "sd", "min", "max")] %>%
    cbind(variable = rownames(.), .) %>%
    dplyr::mutate(
      n = as.integer(n),
      mean = round(mean, 2),
      sd = round(sd, 2),
      n_missing = nrow(d) - n
    )
  dt <- DT::datatable(tab, rownames = FALSE) %>%
    DT::formatStyle(
      "n_missing",
      color = DT::styleInterval(cuts = 0, values = c("black", "#B61A51")),
      fontWeight = "bold"
    )
  return(dt)
}

# tabulate dataset and highlight NAs
#' Title
#'
#' @param d
#'
#' @return
#' @export
#'
#' @examples
tab_NA <- function(d) {
  dt <- d %>%
    dplyr::mutate(dplyr::across(where(is.numeric), round, 2)) %>%
    DT::datatable(d) %>%
    DT::formatStyle(
      names(d),
      target = "cell",
      color = DT::styleEqual("NA", "#B61A51"),
      fontWeight = DT::styleEqual("NA", "bold")
    )
  return(dt)
}

# histogram/bar plot conditional on missingness in another variable
#' Title
#'
#' @param dat
#' @param x
#' @param z
#'
#' @return
#' @export
#'
#' @examples
plot_NA_cond <- function(dat, x, z) {
  # escape function if no variable is selected
  if (x == "Select a variable" |
      z == "Select a variable") {
    return(ggplot2::ggplot(dat) + ggplot2::ggtitle("Please select variable(s)"))
  }
  # define graphing elements to add to plot
  if (is.numeric(dat[[x]])) {
    # for continuous variables
    geom <- ggplot2::geom_histogram()
  } else {
    # for categorical variables
    geom <- ggplot2::geom_bar()
  }
  # create facet labels
  facet_labs <- c(paste(z, "observed"), paste(z, "missing")) %>%
    setNames(c("observed", "missing"))
  # preprocess the data
  dat[!is.na(dat[[x]]), ] %>%
    dplyr::mutate(conditional = factor(
      is.na(.data[[z]]),
      levels = c(FALSE, TRUE),
      labels = c("observed", "missing")
    )) %>%
    # plot
    ggplot2::ggplot(ggplot2::aes(x = .data[[x]],
                                 color = conditional,
                                 fill = "white")) +
    geom +
    # split by conditional variable
    ggplot2::facet_wrap(
      ~ conditional,
      ncol = 1,
      scales = "free_y",
      labeller = ggplot2::labeller(conditional = facet_labs)
    ) +
    # style
    theme_mice() +
    ggplot2::theme(
      legend.position = "none",
      strip.background = ggplot2::element_rect(size = 0.5)
    )
}

# scatterplot with NAs
#' Title
#'
#' @param dat
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
plot_NA_scatter <- function(dat, x, y) {
  # escape function if no variable is selected
  if (x == "Select a variable" |
      y == "Select a variable") {
    return(ggplot2::ggplot(dat) + ggplot2::ggtitle("Please select variable(s)"))
  }
  # define graphing elements to add to plot later
  # set NA value and scale for variable x
  if (is.numeric(dat[[x]])) {
    NA_x <-
      min(dat[[x]], na.rm = TRUE) - .1 * diff(range(dat[[x]], na.rm = TRUE))
    shade_x <-
      min(dat[[x]], na.rm = TRUE) - .05 * diff(range(dat[[x]], na.rm = TRUE))
    scale_x <- NULL
  } else {
    NA_x <- "NA"
    shade_x <- 1.5
    scale_x <-
      ggplot2::scale_x_discrete(limits = c("NA", levels(dat[[x]])))
  }
  # set NA value and scale for variable y
  if (is.numeric(dat[[y]])) {
    NA_y <-
      min(dat[[y]], na.rm = TRUE) - .1 * diff(range(dat[[y]], na.rm = TRUE))
    shade_y <-
      min(dat[[y]], na.rm = TRUE) - .05 * diff(range(dat[[y]], na.rm = TRUE))
    scale_y <- NULL
  } else {
    NA_y <- "NA"
    shade_y <- 1.5
    scale_y <-
      ggplot2::scale_y_discrete(limits = c("NA", levels(dat[[y]])))
  }
  # add geom for NAs in the x space
  if (any(is.na(dat[[x]]))) {
    geom_x <- ggplot2::geom_point(
      position = ggplot2::position_jitter(width = 0.2, height = 0.2),
      color = mice:::mdc(2),
      shape = 4,
      mapping = ggplot2::aes(x = NA_x, y = .data[[y]]),
      data = dat[is.na(dat[[x]]), ]
    )
  } else {
    geom_x <- NULL
  }
  # add geom for NAs in the y space
  if (any(is.na(dat[[y]]))) {
    geom_y <- ggplot2::geom_point(
      position = ggplot2::position_jitter(width = 0.2, height = 0.2),
      color = mice:::mdc(2),
      shape = 4,
      mapping = ggplot2::aes(x = .data[[x]], y = NA_y),
      data = dat[is.na(dat[[y]]), ]
    )
  } else{
    geom_y <- NULL
  }
  # add geom for NAs in the x-y space
  if (any(is.na(dat[[x]]) & is.na(dat[[y]]))) {
    geom_xy <- ggplot2::geom_point(
      position = ggplot2::position_jitter(width = 0.2, height = 0.2),
      color = mice:::mdc(2),
      shape = 4,
      mapping = ggplot2::aes(x = NA_x, y = NA_y),
      data = dat[is.na(dat[[x]]) & is.na(dat[[y]]), ]
    )
  } else{
    geom_xy <- NULL
  }
  # plot
  p <- dat %>% ggplot2::ggplot() +
    # plot observed datapoints in x-y space
    ggplot2::geom_point(
      ggplot2::aes(x = .data[[x]], y = .data[[y]]),
      position = ggplot2::position_jitter(width = 0.2, height = 0.2),
      color = mice::mdc(1)
    ) +
    # add area for NAs in the x space
    ggplot2::annotate(
      "rect",
      xmin = -Inf,
      xmax = shade_x,
      ymin = -Inf,
      ymax = Inf,
      alpha = 0.1
    ) +
    # add area for NAs in the y space
    ggplot2::annotate(
      "rect",
      xmin = -Inf,
      xmax = Inf,
      ymin = -Inf,
      ymax = shade_y,
      alpha = 0.1
    ) +
    # add NAs
    geom_x +
    geom_y +
    geom_xy +
    # style graph
    theme_mice() +
    scale_x +
    scale_y
  # output
  return(p)
}