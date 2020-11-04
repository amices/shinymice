# descriptive statistics for incomplete data

# show NAs in tables
options("DT.TOJSON_ARGS" = list(na = "string"))

# descriptives table
mis_descr <- function(d) {
  tab <-
    psych::describe(d)[, c("n", "mean", "sd", "min", "max", "median")] %>%
    cbind(variable = rownames(.), ., n_missing = as.integer(nrow(d) - .$n))
  tab[, 2] <- as.integer(tab[, 2])
  return(tab)
}

# conditional distribution plot
plot_conditional <- function(dat, x, z) {
  # define geom for numerical vs. categorical data
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
  # preprocess and plot the data
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

# bivariate distribution plot
plot_bivariate <- function(dat, x, y){
  # set NA value and scale for variable 'x'
  if (is.numeric(dat[[x]])) {
    NA_x <-
      min(dat[[x]], na.rm = TRUE) - .5 * sd(dat[[x]], na.rm = TRUE)
    scale_x <- ggplot2::scale_x_continuous(
      expand = c(0.01, 0.01),
      limits = c(NA_x, max(dat[[x]]))
    )
  } else {
    NA_x <- "NA"
    scale_x <-
      ggplot2::scale_x_discrete(
        expand = c(0.01, 0.01),
        limits = c("NA", levels(dat[[x]]))
      )
  }
  
  # add geom for NAs in the 'x' space
  if (any(is.na(dat[[x]]))) {
    geom_x <- ggplot2::geom_point(
      color = mice:::mdc(2),
      shape = 4,
      mapping = ggplot2::aes(x = NA_x, y = .data[[y]]),
      data = dat[is.na(dat[[x]]),]
    )
  } else {
    geom_x <- NULL
  }
  
  # set NA value and scale for variable 'y'
  if (is.numeric(dat[[y]])) {
    NA_y <-
      min(dat[[y]], na.rm = TRUE) - .5 * sd(dat[[y]], na.rm = TRUE)
    scale_y <- ggplot2::scale_y_continuous(
      expand = c(0.01, 0.01),
      limits = c(NA_y, max(dat[[y]])))
  } else {
    NA_y <- "NA"
    scale_y <-
      ggplot2::scale_y_discrete(
        expand = c(0.01, 0.01),
        limits = c("NA", levels(dat[[y]]))
      )
  }
  
  # add geom for NAs in the 'y' space
  if (any(is.na(dat[[y]]))) {
    geom_y <- ggplot2::geom_point(
      color = mice:::mdc(2),
      shape = 4,
      mapping = ggplot2::aes(x = .data[[x]], y = NA_y),
      data = dat[is.na(dat[[y]]),]
    )} else{geom_y <- NULL}
  
  # add geom for NAs in the 'x' and 'y' space
  if (any(is.na(dat[[x]]) & is.na(dat[[y]]))){
    geom_xy <- ggplot2::geom_point(
      color = mice:::mdc(2),
      shape = 4,
      mapping = ggplot2::aes(x = NA_x, y = NA_y),
      data = dat[is.na(dat[[x]]) & is.na(dat[[y]]),]
    )} else{geom_xy <- NULL}
  
  # plot 
  p <- dat %>% ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x = .data[[x]], y = .data[[y]]), color = mice::mdc(1)) +
    geom_x +
    geom_y +
    geom_xy +
    theme_mice() +
    scale_x +
    scale_y 
  
  # output
  return(p)
}