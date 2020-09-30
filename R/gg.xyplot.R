#' Helper function xyplot
#'
#' @param x A string specifying the variable to plot on the x axis.
#' @param y A string specifying the variable to plot on the y axis.
#' @param cd A dataframe created with the \code{ggmice} function.
#' @param m An integer denoting the number of imputations.
#' @param ... Optional additional ggplot2 arguments.
#'
#' @return A list with ggplot elements.
xyplot_function <- function(x, y, cd, m, ...) {
  list(ggplot2::geom_point(
    ggplot2::aes(
    x = .data[[x]],
    y = .data[[y]],
    color = .data$imputed
  ),
  na.rm = TRUE,
  ...))
}
