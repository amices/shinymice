#' server utility
#'
#' @description A shiny utility
#'
#' @param Internal parameters for the utility.
#'
#' @return The return value, if any, from executing the utility.
#'
#' @export
dummy_plot <- function() {
  ggplot2::ggplot(data = data.frame(
    a = c(0, 1),
    b = c(1, 0),
    c = c("dummy", "plot")
  )) +
    ggplot2::geom_text(ggplot2::aes(x = a, y = b, label = c), size = 5) +
    ggplot2::ggtitle("Dummy plot (to be replaced)")
}

# hide some plotly options, see https://plotly-r.com/control-modebar.html
clean_plotly <- function(pp, ...) {
  plotly::config(
    p = pp,
    displaylogo = FALSE,
    modeBarButtonsToRemove = c(
      "zoomIn2d",
      "zoomOut2d",
      "autoScale2d",
      "pan2d",
      "lasso2d",
      "select2d",
      "toggleSpikelines",
      "hoverCompareCartesian",
      "hoverClosestCartesian"
    )
  )
}
