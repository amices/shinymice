# plot histogram of one veriable depending on missingness in another
conditional_hist <- function(dat, x, y, scaler = c(TRUE, FALSE), binner = 0){
# parse inputs
scl <- ifelse(scaler, "fixed", "free_y")
if(binner == 0){bn <- NULL} else {bn <- binner}

# choose hist or bar depending of variable type
if (is.numeric(dat[[x]])) {
  geom <- list(ggplot2::geom_histogram(binwidth = bn, na.rm = TRUE))
} else {
  geom <- list(ggplot2::geom_bar(binwidth = bn, na.rm = TRUE))
}
# define facet labels
labs <- c(
  paste0("Missing ", y), 
  paste0("Observed ", y)) %>% 
  setNames(c(
    "Imputed", "Observed"
  ))
# plot
dat[!is.na(dat[[x]]), ] %>% 
  dplyr::mutate(R = factor(
    is.na(!!y),
    levels = c(FALSE, TRUE),
    labels = c("Observed", "Imputed")
  )) %>%  #factor(is.na(!!input$histvar2), levels = c("Observed", "Missing"))) %>%
  ggplot2::ggplot(ggplot2::aes(x = !!x, fill = R)) +
  geom +
  mice:::theme_mice +
  ggplot2::theme(legend.position = "none") +
  ggplot2::facet_wrap(~ R,
             ncol = 1,
             scales = scl,
             labeller = ggplot2::labeller(R = labs))
   #check tooltip argument
}

