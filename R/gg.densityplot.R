#' Helper function to create density plots
#'
#' @param x A string specifying the variable to plot.
#' @param y Not used.
#' @param cd A dataframe created with the \code{ggmice} function.
#' @param m An integer denoting the number of imputations.
#' @param ... Optional additional ggplot2 arguments.
#'
#' @return A list with ggplot elements.
densityplot_function <- function(x, y = NULL, cd, m, ...) {
  list(ggplot2::geom_density(
    ggplot2::aes(
    x = .data[[x]],
    group = .data$.imp,
    color = .data$imputed,
    size = .data$imputed,
    ...
  ),
  na.rm = TRUE),
  ggplot2::scale_size_manual(values = c(0.5, 1) %>% setNames(c("Imputed", "Observed"))))
}
