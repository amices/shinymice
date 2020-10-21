# mice theme
# maybe use this: https://www.shanelynn.ie/themes-and-colours-for-r-ggplots-with-ggthemr/
theme_mice <- list(
  # theme
  ggplot2::theme_classic(),
  ggplot2::theme(legend.position = "bottom"),
  #colors
  ggplot2::scale_color_manual(values = c(
    "Observed" = mice:::mdc(1),
    "Imputed" = mice:::mdc(2)
  )),
  ggplot2::scale_fill_manual(values = c(
    "Observed" = mice:::mdc(1),
    "Imputed" = mice:::mdc(2)
  ))
)