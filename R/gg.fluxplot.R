#' Helper function to create flux plot
#'
#' @param data A \code{mids} object, typically created by \code{mice()} or
#'\code{mice.mids()}.
#'
#' @return A ggplot object
fluxplot_function <- function(data) {
  # add step to check data type and subset if necessary
  data$data %>%
    mice::flux() %>%
    dplyr::mutate(variable = rownames(.)) %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(
      data = data.frame(x = 0:1, y = 1:0),
      ggplot2::aes(x = x, y = y),
      linetype = "dashed",
      color = "gray"
    ) +
    ggplot2::geom_text(ggplot2::aes(x = influx, y = outflux, label = variable)) +
    ggplot2::scale_x_continuous(limits = c(0, 1)) +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::labs(x = "Influx", y = "Outflux") +
    ggplot2::theme_classic()
}
