# plot distributions before imputation
plot_NA_margins <- function(data, x, y = NULL) {
  # parse inputs
  if (is.null(y)) {
    data <- data %>% mutate(id = 1:nrow(data))
    y = x
    x = "id"
  }
  
  # preprocessing
  geom_x_list <-
    geom_y_list <- scale_x_list <- scale_y_list <- list(NULL)
  if (any(is.na(data[[x]]))) {
    geom_x_list <- list(geom_point(
      aes(x = R_x, y = .data[[y]]),
      color = mice:::mdc(2),
      na.rm = TRUE
    ))
    if (is.numeric(data[[x]])) {
      NA_level_x <-
        min(data[[x]], na.rm = TRUE) - sd(data[[x]], na.rm = TRUE)
      data <-
        data %>% mutate(R_x = ifelse(is.na(.data[[x]]), NA_level_x, NA))
      scale_x_list <-
        list(scale_x_continuous(expand = expansion(mult = c(0.01, .05))))
    } else {
      data <- data %>% mutate(R_x = ifelse(is.na(.data[[x]]), "   ", NA))
      scale_x_list <-
        list(scale_x_discrete(expand = expansion(mult = c(0.01, .05))))
    }
  }
  
  if (any(is.na(data[[y]]))) {
    geom_y_list <- list(geom_point(
      aes(x = .data[[x]], y = R_y),
      color = mice:::mdc(2),
      na.rm = TRUE
    ))
    if (is.numeric(data[[y]])) {
      NA_level_y <-
        min(data[[y]], na.rm = TRUE) - sd(data[[y]], na.rm = TRUE)
      data <-
        data %>% mutate(R_y = ifelse(is.na(.data[[y]]), NA_level_y, NA))
      scale_y_list <-
        list(scale_y_continuous(expand = expansion(mult = c(0.01, .05))))
    } else {
      data <- data %>% mutate(R_y = ifelse(is.na(.data[[y]]), "   ", NA))
      scale_y_list <-
        list(scale_y_discrete(expand = expansion(mult = c(0.01, .05))))
    }
  }
  
  
  # do not do any further processing if there are no NAs
  # if (all(!is.na(data[[x]])) & all(!is.na(data[[y]]))) {
  #   return(ggplot(data, aes(x = .data[[x]], y = .data[[y]])) +
  #            geom_point(color = mice:::mdc(1)) +
  #            theme_classic())
  # }
  
  # for continuous variables
  # if (is.numeric(data[[y]])) {
  #   NA_level_x <-
  #     min(data[[x]], na.rm = TRUE) - sd(data[[x]], na.rm = TRUE)
  #   NA_level_y <-
  #     min(data[[y]], na.rm = TRUE) - sd(data[[y]], na.rm = TRUE)
  #   p <-
  #     data %>% mutate(R_x = ifelse(is.na(.data[[x]]), NA_level_x, NA),
  #                     R_y = ifelse(is.na(.data[[y]]), NA_level_y, NA)) %>%
  #     ggplot() +
  #     geom_point(aes(x = .data[[x]], y = .data[[y]]),
  #                color = mice:::mdc(1),
  #                na.rm = TRUE) +
  #     geom_point(aes(x = .data[[x]], y = R_y),
  #                color = mice:::mdc(2),
  #                na.rm = TRUE) +
  #     geom_point(aes(x = R_x, y = .data[[y]]),
  #                color = mice:::mdc(2),
  #                na.rm = TRUE) +
  #     scale_x_continuous(expand = expansion(mult = c(0.01, .05))) +
  #     scale_y_continuous(expand = expansion(mult = c(0.01, .05))) +
  #     theme_classic()
  #   return(p)
  # } else {
  #   # for (ordered) factors and logical variables
  p <-
    data %>% #mutate(R_x = ifelse(is.na(.data[[x]]), "   ", NA),
    #      R_y = ifelse(is.na(.data[[y]]), "   ", NA)) %>%
    ggplot() +
    geom_point(aes(x = .data[[x]], y = .data[[y]]),
               color = mice:::mdc(1),
               na.rm = TRUE) +
    # geom_point(aes(x = R_x, y = .data[[y]]),
    #            color = mice:::mdc(2),
    #            na.rm = TRUE) +
    # geom_point(aes(x = .data[[x]], y = R_y),
    #            color = mice:::mdc(2),
    #            na.rm = TRUE) +
    #scale_x_discrete(expand = expansion(mult = c(0.01, .05))) +
    #scale_y_discrete(expand = expansion(mult = c(0.01, .05))) +
    geom_x_list +
    geom_y_list +
    scale_x_list +
    scale_y_list +
    theme_classic()
  return(p)
  
  
}
set.seed(123)
data <- mice::boys[sample.int(748, 100), ]

plot_NA_margins(data, x = "hc")
plot_NA_margins(data, x = "phb")
plot_NA_margins(data, x = "age")
plot_NA_margins(data, x = "age", y = "hc")
plot_NA_margins(data, x = "hgt", y = "hc")
