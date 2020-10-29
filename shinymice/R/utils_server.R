#' server utility
#'
#' @description A shiny utility
#'
#' @param Internal parameters for the utility.
#'
#' @return The return value, if any, from executing the utility.
#'
#' @export
dummy_plot <- function(){
  ggplot2::ggplot(data = data.frame(a = c(0, 1), b = c(1, 0), c = c("dummy", "plot"))) +
  ggplot2::geom_text(ggplot2::aes(x = a, y = b, label = c), size = 5)
}
