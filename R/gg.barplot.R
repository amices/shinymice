#' Helper function to create histograms (for categorical data)
#'
#' @param x A string specifying the variable to plot.
#' @param y Not used.
#' @param cd A dataframe created with the \code{ggmice} function.
#' @param m An integer denoting the number of imputations.
#' @param scaling A logical indicator specifying whether to sclae the y axis.
#' @param ... Optional additional ggplot2 arguments.
#'
#' @return A list with ggplot elements.
barplot_function <- function(x, y = NULL, cd, m, scaling = FALSE, ...) {
  if (scaling == FALSE) {
    a <- ggplot2::aes(x = .data[[x]],
             group = .data$.imp,
             color = .data$imputed,
             fill = .data$imputed)
  } 
  if (scaling == TRUE) {
    a <- ggplot2::aes(
      x = .data[[x]],
      y = ..prop..,
      group = .data$.imp,
      color = .data$imputed,
      fill = .data$imputed
    )
  }
  list(
    ggplot2::geom_bar(
      #data = cd[!is.na(cd[[x]]),],
      mapping = a,
      position = "identity",
      alpha = 0.25,
      na.rm = TRUE,
      ...
    )
  )
}
