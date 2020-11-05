# fluxplot
plot_flux <- function(dat) {
  flx <- mice::flux(dat)
  p <- flx %>%
    cbind(variable = rownames(.), .) %>%
    ggplot2::ggplot() +
    ggplot2::geom_text(ggplot2::aes(x = influx, y = outflux, label = variable)) +
    ggplot2::geom_abline(intercept = 1,
                         slope = -1,
                         linetype = "dashed") +
    ggplot2::lims(x = c(0, 1), y = c(0, 1)) +
    ggplot2::theme_classic() +
    ggplot2::ggtitle("Influx-outflux plot")
  pp <-
    plotly::ggplotly(p) %>% clean_plotly(pp = .)
  return(pp)
}

# traceplot functions

# make chain means and variances tidy
preprocess_thetas <- function(imp) {
  d <- imp$chainMean %>%
    dplyr::na_if(., "NaN") %>%
    as.data.frame() %>%
    dplyr::mutate(var = row.names(.), theta = "Chain means") %>%
    rbind(
      .,
      imp$chainVar %>%
        sqrt() %>%
        as.data.frame(.) %>%
        dplyr::mutate(var = row.names(.), theta = "Chain standard deviations")
    ) %>%
    tidyr::pivot_longer(-c(var, theta)) %>%
    cbind(.it = as.integer(1:imp$iteration),
          .imp = as.factor(rep(1:imp$m, each = imp$iteration)))
  return(d)
}
