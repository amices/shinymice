# plot distributions before imputation
plot_NA_margins <- function(data, x, y = NULL) {
  # parse inputs
  if (is.null(y)) {
    data <- data %>% dplyr::mutate(id = 1:nrow(data))
    y = x
    x = "id"
  }
  
  # preprocessing
  geom_x_list <-
    geom_y_list <-
    scale_x_list <- geom_x_y_list <- scale_y_list <- list(NULL)
  if (any(is.na(data[[x]]))) {
    geom_x_list <- list(ggplot2::geom_point(
      ggplot2::aes(x = R_x, y = .data[[y]]),
      color = mice:::mdc(2),
      na.rm = TRUE
    ))
    if (is.numeric(data[[x]])) {
      NA_level_x <-
        min(data[[x]], na.rm = TRUE) - sd(data[[x]], na.rm = TRUE)
      data <-
        data %>% dplyr::mutate(R_x = ifelse(is.na(.data[[x]]), NA_level_x, NA))
      scale_x_list <-
        list(ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.01, .05))))
    } else {
      data <-
        data %>% dplyr::mutate(R_x = ifelse(is.na(.data[[x]]), "   ", NA))
      scale_x_list <-
        list(ggplot2::scale_x_discrete(expand = ggplot2::expansion(mult = c(0.01, .05))))
    }
  }
  
  if (any(is.na(data[[y]]))) {
    geom_y_list <- list(ggplot2::geom_point(
      ggplot2::aes(x = .data[[x]], y = R_y),
      color = mice:::mdc(2),
      na.rm = TRUE
    ))
    if (is.numeric(data[[y]])) {
      NA_level_y <-
        min(data[[y]], na.rm = TRUE) - sd(data[[y]], na.rm = TRUE)
      data <-
        data %>% dplyr::mutate(R_y = ifelse(is.na(.data[[y]]), NA_level_y, NA))
      scale_y_list <-
        list(ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.01, .05))))
    } else {
      data <-
        data %>% dplyr::mutate(R_y = ifelse(is.na(.data[[y]]), "   ", NA))
      scale_y_list <-
        list(ggplot2::scale_y_discrete(expand = ggplot2::expansion(mult = c(0.01, .05))))
    }
  }
  
  if (any(is.na(data[[x]])) & any(is.na(data[[y]]))) {
    geom_x_y_list <-
      list(ggplot2::geom_point(
        ggplot2::aes(x = R_x, y = R_y),
        color = mice:::mdc(2),
        na.rm = TRUE
      ))
  }
  
  p <-
    data %>%
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x = .data[[x]], y = .data[[y]]),
                        color = mice:::mdc(1),
                        na.rm = TRUE) +
    geom_x_list +
    geom_y_list +
    geom_x_y_list +
    scale_x_list +
    scale_y_list +
    ggplot2::theme_classic()
  
  return(p)
  
  
}
set.seed(123)
data <- mice::boys[sample.int(748, 100), ]

plot_NA_margins(data, x = "hc")
plot_NA_margins(data, x = "phb")
plot_NA_margins(data, x = "age")
plot_NA_margins(data, x = "age", y = "hc")
plot_NA_margins(data, x = "hgt", y = "hc")
plot_NA_margins(data, x = "gen", y = "phb")
# add some jitter to make plots of categocial variables more informative?
plot_NA_margins(data, x = "gen", y = "hc")
