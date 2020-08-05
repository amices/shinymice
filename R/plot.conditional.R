# plot histogram of one veriable depending on missingness in another
conditional_hist <- function(dat, x, y, scaler, binner = NULL){
# choose hist or bar depending of variable type
if (is.numeric(dat[[x]])) {
  geom <- list(geom_histogram(binwidth = binner, na.rm = TRUE))
} else {
  geom <- list(geom_bar(binwidth = binner, na.rm = TRUE))
}
# define facet labels
labs <- c(
  paste0("Missing ", y), 
  paste0("Observed ", y)) %>% 
  setNames(c(
    "Imputed", "Observed"
  ))
# plot
dat %>%
  dplyr::mutate(R = factor(
    is.na(!!y),
    levels = c(FALSE, TRUE),
    labels = c("Observed", "Imputed")
  )) %>%  #factor(is.na(!!input$histvar2), levels = c("Observed", "Missing"))) %>%
  ggplot(aes(x = !!x, fill = R)) +
  geom +
  mice:::theme_mice +
  theme(legend.position = "none") +
  facet_wrap(~ R,
             ncol = 1,
             scales = scaler,
             labeller = labeller(R = labs))
}

