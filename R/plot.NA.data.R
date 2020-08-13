# plot distributions before imputation
plot_NA_margins <- function(data, x, y = NULL) {
  if(is.null(y)){
    data <- data %>% mutate(id = 1:nrow(data))
    y = x
    x = "id"
  }
  
  # do not do any further processing if there are no NAs
  if (all(!is.na(data[[y]]))) {
    return(ggplot(data, aes(x = .data[[x]], y = .data[[y]])) +
             geom_point(color = mice:::mdc(1)) +
             theme_classic())
  } else {
    # for continuous variables
    if (is.numeric(data[[y]])) {
      NA_level <-
        min(data[[y]], na.rm = TRUE) - sd(data[[y]], na.rm = TRUE)
      p <- data %>% mutate(R = ifelse(is.na(.data[[y]]), NA_level, NA)) %>%
        ggplot() +
        geom_point(aes(x = .data[[x]], y = .data[[y]]),
                   color = mice:::mdc(1),
                   na.rm = TRUE) +
        geom_point(aes(x = .data[[x]], y = R),
                   color = mice:::mdc(2),
                   na.rm = TRUE) +
        scale_y_continuous(expand = expansion(mult = c(0.01, .05))) +
        theme_classic()
      return(p)
    } else {
      # for (ordered) factors and logical variables
      p <- data %>% mutate(R = ifelse(is.na(.data[[y]]), "   ", NA)) %>%
        ggplot() +
        geom_point(aes(x = .data[[x]], y = .data[[y]]),
                   color = mice:::mdc(1),
                   na.rm = TRUE) +
        geom_point(aes(x = .data[[x]], y = R),
                   color = mice:::mdc(2),
                   na.rm = TRUE) +
        scale_y_discrete(expand = expansion(mult = c(0.01, .05))) +
        theme_classic()
      return(p)
    }
  }
}
set.seed(123)
data <- mice::boys[sample.int(748, 100), ]

plot_NA_margins(data, x = "hc")
plot_NA_margins(data, x = "phb")
plot_NA_margins(data, x = "age")
