#' Title Upload a file with incomplete data, or don't (then use mice::boys)
#'
#' @param file An incomplete dataset with file extension '.csv', '.tsv', '.rda', or '.RData'
#'
#' @return The file loaded in the working directory as dataframe
read_data <- function(file) {
  if (is.null(file)) {
    set.seed(123)
    return(mice::boys[sample.int(748, 100),])
  } else {
    ext <- tools::file_ext(file$name)
    f <- file$datapath
    e <- new.env()
    d <- switch(
      ext,
      csv = vroom::vroom(f, delim = ","),
      tsv = vroom::vroom(f, delim = "\t"),
      Rdata = e[[load(f, envir = e)]],
      rda = e[[load(f, envir = e)]],
      validate("Invalid file; Please upload a .csv, .tsv, or .Rdata file")
    )
    return(d)
  }
}

#' Title Create descriptive statistics for incomplete data
#'
#' @param d An incomplete dataset of class dataframe
#'
#' @return A datatable object with descriptives
descr_NA <- function(d) {
  tab <-
    psych::describe(d)[, c("n", "mean", "sd", "min", "max", "median")] %>%
    cbind(variable = rownames(.), .) %>%
    dplyr::mutate(
      n = as.integer(n),
      mean = round(mean, 2),
      sd = round(sd, 2),
      n_missing = nrow(d) - n
    )
  dt <- DT::datatable(tab, rownames = FALSE, options = list(dom = 't'), selection = 'none') %>%
    DT::formatStyle(
      "n_missing",
      color = DT::styleInterval(cuts = 0, values = c("black", "#B61A51")),
      fontWeight = "bold"
    )
  return(dt)
}

#' Title Tabulate an incomplete dataset and highlight the NAs
#'
#' @param d An incomplete dataset of class dataframe
#'
#' @return A datatable object with the NA cells highlighted
tab_NA <- function(d) {
  dt <- d %>%
    dplyr::mutate(dplyr::across(where(is.numeric), round, 2)) %>%
    DT::datatable(., selection = "none", options = list(searching = FALSE)) %>% 
    DT::formatStyle(
      names(.$x$data),
      color = DT::styleEqual("NA", "#B61A51"),
      fontWeight = DT::styleEqual("NA", "bold")
    )
  return(dt)
}

#' Title Create a histogram/bar plot of a variable conditional on missingness in another variable
#'
#' @param dat An incomplete dataset of class dataframe
#' @param x A variable to plot
#' @param z A second variable to plot
#'
#' @return A ggplot object with two facets (variable z observed vs. missing)
#' @export
plot_NA_cond <- function(dat, x, z) {
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
  d <- dat[!is.na(dat[[x]]),] %>%
    dplyr::mutate(conditional = factor(
      is.na(.data[[z]]),
      levels = c(FALSE, TRUE),
      labels = c("observed", "missing")
    )) 
  
  # plot
  p <- d %>% ggplot2::ggplot(ggplot2::aes(x = .data[[x]],
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
  # output
  return(p)
}

#' Title Create a scatterplot with NAs plotted in the margins
#'
#' @param dat An incomplete dataset of class dataframe
#' @param x A variable to plot
#' @param y A second variable to plot
#'
#' @return A ggplot object with NAs plotted in the margins
#' @export
plot_NA_scatter <- function(dat, x, y) {
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
      data = dat[is.na(dat[[x]]),]
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
      data = dat[is.na(dat[[y]]),]
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
      data = dat[is.na(dat[[x]]) & is.na(dat[[y]]),]
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