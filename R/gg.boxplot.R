#' Helper function to create bw/box plots
#'
#' @param x A string specifying the variable to plot.
#' @param y Not used.
#' @param cd A dataframe created with the \code{ggmice} function.
#' @param m An integer denoting the number of imputations.
#' @param ... Optional additional ggplot2 arguments.
#'
#' @return A list with ggplot elements.
boxplot_function <- function(x, y = NULL, cd, m, ...) {
  list(
    ggplot2::stat_boxplot(
      ggplot2::aes(
        x = as.factor(.data$.imp),
        y = .data[[x]],
        color = .data$imputed
      ),
      na.rm = TRUE,
      width = 0.25,
      size = 1,
      geom = "errorbar",
      ...
    ),
    ggplot2::geom_boxplot(
      ggplot2::aes(
        x = as.factor(.data$.imp),
        y = .data[[x]],
        color = .data$imputed
      ),
      na.rm = TRUE,
      size = 1,
      ...
    ),
    ggplot2::scale_x_discrete(breaks = c(0, 1:m), drop = FALSE),
    ggplot2::xlab("Imputation (0 = observed data)")
  )
}
