# descriptives for imputed data
imp_descr <- function(imp){
  tab <- imp %>% mice::complete("all") %>% 
    purrr::map_df(., ~{psych::describe(.)[,c("n", "mean", "sd", "min", "max", "median")] %>% 
    cbind(., variable = rownames(.))}) %>% 
    aggregate(.~variable, data = ., FUN = "mean") %>% 
    cbind(., n_imputed = as.integer(imp$nmis))
  tab[,2] <- as.integer(tab[,2])
  return(tab)
}

####################
# plotting functions
####################

# preprocess
prepare_plotting <- function(mids, x, y = NULL) {
  imps <- mids$imp[[x]] %>%
    dplyr::mutate(.id = row.names(.)) %>%
    tidyr::pivot_longer(
      cols = 1:mids$m,
      names_to = ".imp",
      values_to = x,
      names_transform = list(.imp = as.numeric)
    )
  
  # preprocess incomplete data
  d <-
    mids$data %>%
    dplyr::mutate(.id = rownames(.)) %>%
    .[c(x, y, ".id")] %>%
    cbind(.imp = 0)
  
  # combine incomplete and imputed data
  if (is.null(y)) {
    cd <- d %>%
      dplyr::bind_rows(., imps) %>%
      dplyr::mutate(imputed = ifelse(.imp > 0, 'Imputed', 'Observed'))
  }
  return(cd)
}

# define theme
theme_mice <- function() {
  ggplot2::update_geom_defaults("point", list(
    shape = 21,
    size = 1.9,
    stroke = 1.1,
    alpha = 0.5
  ))
  ggplot2::update_geom_defaults("boxplot", list(
    size = 1
  ))
  ggplot2::update_geom_defaults("line", list(
    size = 1
  ))
  
  theme <- list(
    ggplot2::theme_classic(),
    ggplot2::theme(legend.position = "bottom"),
    ggplot2::scale_color_manual(
      values = c(
        "Observed" = mice:::mdc(1),
        "Missing" = mice:::mdc(2),
        "Imputed" = mice:::mdc(2)
      )
    ),
    ggplot2::scale_fill_manual(
      values = c(
        "Observed" = mice:::mdc(1),
        "Missing" = mice:::mdc(2),
        "Imputed" = mice:::mdc(2)
      )
    )
  )
  return(theme)
}

# stripplot
geom_stripplot <- function(x, m) {
  p <- list(
    ggplot2::geom_jitter(
      ggplot2::aes(
        x = as.factor(.data$.imp),
        y = .data[[x]],
        color = .data$imputed
      ),
      height = 0.1,
      width = 0.1
    ),
    ggplot2::scale_x_discrete(limits = as.character(0:m)),
    ggplot2::labs(
      x = "Imputation (0 = observed data)",
      y = paste0("Variable: '", x, "'"),
      color = "",
      fill = "",
      size = ""
    )
  )
  return(p)
}

# bwplot
geom_bwplot <- function(x, m){
  list(
    ggplot2::geom_boxplot(
      ggplot2::aes(
        x = as.factor(.data$.imp),
        y = .data[[x]],
        color = .data$imputed
      ),
    ),
    ggplot2::scale_x_discrete(limits = as.character(0:m)),
    ggplot2::labs(
      x = "Imputation (0 = observed data)",
      y = paste0("Variable: '", x, "'"),
      color = "",
      fill = "",
      size = ""
    ))
}

# densityplot
geom_densityplot <- function(x, m){
  list(ggplot2::geom_density(
    ggplot2::aes(
      x = .data[[x]],
      group = .data$.imp,
      color = .data$imputed,
      size = .data$imputed)),
    ggplot2::scale_size_manual(values = c(0.5, 1) %>% setNames(c("Imputed", "Observed")))
  )}
