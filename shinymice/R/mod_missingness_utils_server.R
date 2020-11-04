# descriptive statistics for incomplete data

# show NAs in tables
options("DT.TOJSON_ARGS" = list(na = "string"))

# descriptive table
mis_descr <- function(d) {
  tab <-
    psych::describe(d)[, c("n", "mean", "sd", "min", "max", "median")] %>%
    cbind(variable = rownames(.), ., n_missing = as.integer(nrow(d) - .$n))
  tab[, 2] <- as.integer(tab[, 2])
  return(tab)
}

# conditional distribution plot
plot_conditional <- function(dat, x, z) {
  if (is.numeric(dat[[x]])) {
    geom <-
      ggplot2::geom_density(ggplot2::aes(x = .data[[x]], y = ..count.., color = conditional), size = 1)
  } else {
    geom <-
      ggplot2::geom_bar(
        ggplot2::aes(x = .data[[x]], color = conditional),
        position = ggplot2::position_dodge(),
        fill = "white",
        size = 1
      )
  }
  dat %>%
    dplyr::mutate(conditional = factor(
      is.na(.data[[z]]),
      levels = c(TRUE, FALSE),
      labels = c("Missing", "Observed")
    )) %>%
    ggplot2::remove_missing(., vars = x) %>%
    ggplot2::ggplot() +
    geom +
    theme_mice() +
    ggplot2::labs(color = paste0("Variable '", z, "' is:"))
}