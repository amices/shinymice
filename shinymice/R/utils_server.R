#' server utility
#'
#' @description A shiny utility
#'
#' @param Internal parameters for the utility.
#'
#' @return The return value, if any, from executing the utility.
#'
#' @export

# not in
`%nin%` <- Negate(`%in%`)

# mice theme
theme_mice <- function() {
  # change default plotting options (move this outside of the function to unnecessary avoid re-running?)
  ggplot2::update_geom_defaults("point", list(
    shape = 21,
    size = 1.9,
    stroke = 1.1,
    alpha = 0.5
  ))
  ggplot2::update_geom_defaults("boxplot", list(size = 1))
  ggplot2::update_geom_defaults("line", list(size = 1))
  # mice style settings
  theme <- list(
    ggplot2::theme_classic(),
    ggplot2::theme(legend.position = "top"),
    ggplot2::scale_color_manual(
      values = c(
        "observed" = mice:::mdc(1),
        "missing" = mice:::mdc(2),
        "imputed" = mice:::mdc(2)
      )
    ),
    ggplot2::scale_fill_manual(
      values = c(
        "observed" = mice:::mdc(1),
        "missing" = mice:::mdc(2),
        "imputed" = mice:::mdc(2)
      )
    ),
    ggplot2::scale_size_manual(values = c(
      "observed" = 1,
      "imputed" = 0.5
    )),
    ggplot2::labs(
      color = NULL,
      fill = NULL,
      size = NULL
    )
  )
  # output
  return(theme)
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

# mock plot for complete datasets
plot_a_mouse <- function() {
  ggplot2::ggplot(mouse, ggplot2::aes(x = X, y = Y)) +
    ggplot2::geom_point(size = 5,
                        shape = 20,
                        alpha = 1) +
    ggplot2::scale_x_continuous(limits = c(0, 100)) +
    ggplot2::scale_y_continuous(limits = c(0, 100)) +
    ggplot2::geom_text(ggplot2::aes(x = 50, y = 10, label = "No need for mice!"), size = 10) +
    ggplot2::theme_classic()
}
