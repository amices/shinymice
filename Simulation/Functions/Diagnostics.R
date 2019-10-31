# function to extract diagnostics
# requires ...

diagnostics.function <- function(mipo_object, true_effect, m = 5, z_value = z_value) {
  # extract est and ci
  imp_est <- mipo_object$pooled$estimate[[2]]
  imp_var <- mipo_object$pooled$b[[2]] + (mipo_object$pooled$b[[2]] / m)
  imp_se <- sqrt(imp_var)
  ci_low <- imp_est - z_value * imp_se
  ci_up <- imp_est + z_value * imp_se
  
  # eval
  # for each sim, for each maxit value...
  # ... compute bias
  # bias <- mi.lm$est - compl.lm$est per sim, per maxit value
  bias <-
    as.numeric(imp_est - true_effect)
  
  # ... compute confidence interval width (ciw): Wider intervals are associated with more uncertainty and the more narrow interval that is still properly covered indicates a sharper inference
  # ciw <- mi.lm$ci95_ul - mi.lm$ci95_ll
  ciw <-
    ci_low - ci_up
  
  # ... indicate if 'true' value is within ci95%
  # cov.ind <- ifelse(compl.lm$est %in% mi.lm$ci95, 1, 0)
  cov.ind <-
    dplyr::between(true_effect, ci_low, ci_up)
  # cov = coverage rate (95 out of 100 CI95%s should contain the 'true' value): "coverage rate (cov) of the 95% confidence interval"
  # cov.rate <- mean(cov.ind)
  
  return(c(bias, ciw, cov.ind))
}