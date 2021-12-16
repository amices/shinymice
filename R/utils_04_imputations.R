#' Title Compute descriptive statistics for imputed data
#'
#' @param imp A multiply imputed data set (`mids`) object
#'
#' @return A datatable with descriptives
imp_descr <- function(imp) {
  # compute descriptives
  tab <- imp %>% mice::complete("all") %>%
    purrr::map_df(., ~ {
      psych::describe(.)[, c("n", "mean", "sd", "min", "max", "median")] %>%
        cbind(., variable = rownames(.))
    }) %>%
    aggregate(. ~ variable, data = ., FUN = "mean") %>%
    dplyr::mutate(
      n = as.integer(n),
      mean = round(mean, 2),
      sd = round(sd, 2),
      n_imputed = as.integer(imp$nmis)
    )
  #tab[, 2] <- as.integer(tab[, 2])
  # create datatable
  dt <- DT::datatable(tab, rownames = FALSE, options = list(dom = 't'), selection = 'none') %>%
    DT::formatStyle(
      "n_imputed",
      color = DT::styleInterval(cuts = 0, values = c("black", "#B61A51")),
      fontWeight = "bold"
    )
  return(dt)
}

#' Title Preprocess imputations and initialize plot
#'
#' @param imp A multiply imputed data set (`mids`) object
#' @param x A variable to plot
#' @param y An optional second variable to plot
#'
#' @return A ggplot object with imputed data
plot_imps <- function(imp, x, y = NULL) {
  # parse inputs
  if (is.null(y)) {
    y <- x
  }
  # combine observed and imputed data
  xy_obs <- imp$data %>%
    cbind(datapoint = "observed",
          .imp = 0,
          .id = 1:nrow(.),
          .) %>%
    .[!is.na(imp$data[[x]]) &
        !is.na(imp$data[[y]]),]
  xy_imps <- imp %>%
    mice::complete("long") %>%
    cbind(datapoint = "imputed", .) %>%
    .[.$.id %nin% xy_obs$.id, ]
  xy_dat <- rbind(xy_obs, xy_imps) %>%
    dplyr::mutate(datapoint = factor(datapoint, levels = c("observed", "imputed")))
  # initialize plot
  p <- xy_dat %>%
    ggplot2::ggplot() +
    theme_mice()
  # output
  return(p)
}

#' Title Plot boxplot/faceted barplot of imputed data
#'
#' @param imp A multiply imputed data set (`mids`) object
#' @param x A variable to plot
#'
#' @return A ggplot object to replace mice::bwplot()
#' @export
plot_bw <- function(imp, x) {
  # plot
  if (is.numeric(imp$data[[x]])) {
    # plot box and whiskers for numeric variables
    p <- imp %>% plot_imps(x) +
      ggplot2::geom_boxplot(ggplot2::aes(
        x = as.factor(.imp),
        y = .data[[x]],
        color = datapoint
      ),
      width = 0.5) +
      ggplot2::xlab("Imputation (0 = observed data)")
  } else {
    # plot faceted barplot for categorical variables
    p <- imp %>% plot_imps(x) +
      ggplot2::geom_bar(ggplot2::aes(x = .data[[x]],
                                     color = datapoint),
                        width = 0.5,
                        fill = "white") +
      ggplot2::facet_wrap(
        ~ .imp,
        scales = "free_y",
        ncol = 2,
        labeller = ggplot2::labeller(.imp = c(
          "Observed data", paste("Imputation", 1:imp$m)
        ) %>% setNames(0:imp$m))
      )
  }
  # output
  return(p)
}

#' Title Plot stripplot of imputed data
#'
#' @param imp A multiply imputed data set (`mids`) object
#' @param x A variable to plot
#'
#' @return A ggplot object to replace mice::stripplot
#' @export
plot_strip <- function(imp, x) {
  # plot individual values (stripplot)
  p <- imp %>% plot_imps(x) +
    ggplot2::geom_jitter(
      ggplot2::aes(
        x = as.factor(.imp),
        y = .data[[x]],
        color = datapoint
      ),
      height = 0.25,
      width = 0.25
    ) +
    ggplot2::xlab("Imputation (0 = observed data)")
  # output
  return(p)
}

#' Title Plot density of imputed data
#'
#' @param imp A multiply imputed data set (`mids`) object
#' @param x A variable to plot
#'
#' @return A ggplot object to replace mice::densityplot
#' @export
plot_dens <- function(imp, x) {
  # plot density
  p <- imp %>% plot_imps(x) +
    ggplot2::geom_density(ggplot2::aes(
      x = .data[[x]],
      color = datapoint,
      size = datapoint,
      group = .imp
    ))
  # output
  return(p)
}

#' Title Plot scatterplot of imputed data
#'
#' @param imp A multiply imputed data set (`mids`) object
#' @param x A variable to plot
#' @param y A second variable to plot
#'
#' @return A ggplot object to replace xyplot
#' @export
plot_xy <- function(imp, x, y) {
  # plot xy datapoints (scatterplot)
  p <- imp %>% plot_imps(x, y) +
    ggplot2::geom_point(
      ggplot2::aes(
        x = .data[[x]],
        y = .data[[y]],
        group = .id,
        color = datapoint
      ),
      position = ggplot2::position_jitter(height = 0.1, width = 0.1)
    )
  # output
  return(p)
}
