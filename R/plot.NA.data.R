# plot distributions before imputation
plot_NA_margins <- function(data, x) {
  data <- data %>% mutate(id = 1:nrow(data))
  # do not do any further processing if there are no NAs
  if (all(!is.na(data[[x]]))) {
    return(ggplot(data, aes(x = id, y = .data[[x]])) +
             geom_point(color = mice:::mdc(1)) +
             theme_classic())
  } else {
    # for continuous variables
    if (is.numeric(data[[x]])) {
      NA_level <-
        min(data[[x]], na.rm = TRUE) - sd(data[[x]], na.rm = TRUE)
      data %>% mutate(R = ifelse(is.na(.data[[x]]), NA_level, NA)) %>%
        ggplot() +
        geom_point(aes(x = id, y = .data[[x]]),
                   color = mice:::mdc(1),
                   na.rm = TRUE) +
        geom_point(aes(x = id, y = R),
                   color = mice:::mdc(2),
                   na.rm = TRUE) +
        scale_y_continuous(expand = expansion(mult = c(0.01, .05))) +
        theme_classic()
    } else {
      # for (ordered) factors and logical variables
      data %>% mutate(R = ifelse(is.na(.data[[x]]), " (NA)", NA)) %>%
        ggplot() +
        geom_point(aes(x = id, y = .data[[x]]),
                   color = mice:::mdc(1),
                   na.rm = TRUE) +
        geom_point(aes(x = id, y = R),
                   color = mice:::mdc(2),
                   na.rm = TRUE) +
        scale_y_discrete(expand = expansion(mult = c(0.01, .05))) +
        theme_classic()
    }
  }
}
set.seed(123)
data <- mice::boys[sample.int(748, 100), ]

plot_NA_margins(data, x = "hc")
plot_NA_margins(data, x = "phb")
plot_NA_margins(data, x = "age")
