# plot histogram of one veriable depending on missingness in another
conditional_hist <- function(dat, x, y, scaler = c(TRUE, FALSE), binner = 0){
# parse inputs
scl <- ifelse(scaler, "fixed", "free_y")
if(binner == 0){bn <- NULL} else {bn <- binner}

# choose hist or bar depending of variable type
if (is.numeric(dat[[x]])) {
  geom <- list(geom_histogram(binwidth = bn, na.rm = TRUE))
} else {
  geom <- list(geom_bar(binwidth = bn, na.rm = TRUE))
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
             scales = scl,
             labeller = labeller(R = labs))
   #check tooltip argument
}

