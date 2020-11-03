# fluxplot
plot_flux <- function(dat){
  flx <- mice::flux(dat)
  p <- flx %>% 
    cbind(variable = rownames(.), .) %>% 
    ggplot2::ggplot() +
    ggplot2::geom_text(
      ggplot2::aes(x = influx, y = outflux, label = variable)
    ) +
    ggplot2::geom_abline(intercept = 1, slope = -1, linetype = "dashed") +
    ggplot2::lims(x = c(0,1), y = c(0,1)) +
    ggplot2::theme_classic()
  return(p)
}
