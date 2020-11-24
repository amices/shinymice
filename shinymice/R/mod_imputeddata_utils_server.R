# descriptives for imputed data
#' Title
#'
#' @param imp
#'
#' @return
#' @export
#'
#' @examples
imp_descr <- function(imp) {
  tab <- imp %>% mice::complete("all") %>%
    purrr::map_df(., ~ {
      psych::describe(.)[, c("n", "mean", "sd", "min", "max", "median")] %>%
        cbind(., variable = rownames(.))
    }) %>%
    aggregate(. ~ variable, data = ., FUN = "mean") %>%
    cbind(., n_imputed = as.integer(imp$nmis))
  tab[, 2] <- as.integer(tab[, 2])
  return(tab)
}

# imputation plot: extract imputations and initialize plot
#' Title
#'
#' @param imp
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
plot_imps <- function(imp, x, y = NULL) {
  # escape function if no variable is selected
  if (x == "Select a variable") {
    return(ggplot2::ggplot(imp$data) + ggplot2::ggtitle("Please select variable(s)"))
  }
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
        !is.na(imp$data[[y]]), ]
  xy_imps <- imp %>%
    mice::complete("long") %>%
    cbind(datapoint = "imputed", .) %>%
    .[.$.id %nin% xy_obs$.id,]
  xy_dat <- rbind(xy_obs, xy_imps) %>%
    dplyr::mutate(datapoint = factor(datapoint, levels = c("observed", "imputed")))
  # initialize plot
  p <- xy_dat %>%
    ggplot2::ggplot() +
    theme_mice()
  # output
  return(p)
}

# boxplot (not informative with categorical variable)
#' Title
#'
#' @param imp
#' @param x
#'
#' @return
#' @export
#'
#' @examples
plot_bw <- function(imp, x) {
  # escape function if no variable is selected
  if (x == "Select a variable") {
    return(ggplot2::ggplot(imp$data) + ggplot2::ggtitle("Please select variable(s)"))
  }
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

# stripplot
#' Title
#'
#' @param imp
#' @param x
#'
#' @return
#' @export
#'
#' @examples
plot_strip <- function(imp, x) {
  # escape function if no variable is selected
  if (x == "Select a variable") {
    return(ggplot2::ggplot(imp$data) + ggplot2::ggtitle("Please select variable(s)"))
  }
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

# density plot
#' Title
#'
#' @param imp
#' @param x
#'
#' @return
#' @export
#'
#' @examples
plot_dens <- function(imp, x) {
  # escape function if no variable is selected
  if (x == "Select a variable") {
    return(ggplot2::ggplot(imp$data) + ggplot2::ggtitle("Please select variable(s)"))
  }
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

# xyplot
#' Title
#'
#' @param imp
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
plot_xy <- function(imp, x, y) {
  # escape function if no variable is selected
  if (x == "Select a variable" |
      y == "Select a variable") {
    return(ggplot2::ggplot(imp$data) + ggplot2::ggtitle("Please select variable(s)"))
  }
  # plot xy datapoints (scatterplot)
  p <- imp %>% plot_imps(x, y) +
    ggplot2::geom_point(ggplot2::aes(
      x = .data[[x]],
      y = .data[[y]],
      group = .id,
      color = datapoint
    ))
  # output
  return(p)
}
