# evaluation function
# requires packages 'miceadds' and 'dplyr'

########## Part 1 ############
# extract R hat values
convergence.diag <- function(sims) {
  # create objects to store output in
  r.hat <- matrix(NA, nrow = n.iter, ncol = n.sim)
  convergence <- matrix(NA, n.iter, ncol = 4)
  
  # R hat
  for (i in 2:n.iter) {
    for (j in 1:n.sim) {
      # extract R hat
      r.hat[i, j] <-
        max(as.numeric(Rhat.mice(sims[[i]][[j]])$Rhat.M.imp))
    }
  }
  
  # store the diagnostics in convergence matrix
  convergence[, 1] <- mean.r.hat <- rowMeans(r.hat, na.rm = T)
  convergence[, 2] <-
    conv.rate.broad <- rowMeans(r.hat < 1.2, na.rm = T)
  convergence[, 3] <-
    conv.rate.middle <- rowMeans(r.hat < 1.1, na.rm = T)
  convergence[, 4] <-
    conv.rate.narrow <- rowMeans(r.hat < 1.01, na.rm = T)
  
  #output
  return(convergence)
}

########## Part 2 ############
# apply lm() on each sim, on each maxit value
my.lm <-
  function(OUT)
    summary(pool(lm.mids(V1 ~ V2, OUT)), conf.int = T)

# testing
test1 <- mice(data)
test2 <- lm.mids(Y ~ X + Z1 + Z2, test)
test3 <- pool(test2)
# use t of finite pop.
imp_est <- test3$pooled$estimate[[2]]
imp_var <- test3$pooled$b[[2]] + (test3$pooled$b[[2]]/test3$m)
imp_se <- sqrt(imp_var)
z_value <- qnorm(.025,lower.tail = F)
ci_low <- imp_est - z_value * imp_se
ci_up <- imp_est + z_value * imp_se
# what to save?
out <- c(imp_est, ci_low, ci_up)

########## Part 3 ############
# produce simulation diagnostics
evaluate.function <- function(OUT) {
  # set.seed(123)
  
  # for each sim, for each maxit value...
  # ... compute bias
  # bias <- mi.lm$est - compl.lm$est per sim, per maxit value
  bias <-
    as.numeric(OUT$estimate[2] - true_effect)
  
  # ... compute confidence interval width (ciw): Wider intervals are associated with more uncertainty and the more narrow interval that is still properly covered indicates a sharper inference
  # ciw <- mi.lm$ci95_ul - mi.lm$ci95_ll
  ciw <-
    OUT$`97.5 %`[2] - OUT$`2.5 %`[2]
  
  # ... indicate if 'true' value is within ci95%
  # cov.ind <- ifelse(compl.lm$est %in% mi.lm$ci95, 1, 0)
  cov.ind <-
    dplyr::between(true_effect,
                   OUT$`2.5 %`[2],
                   OUT$`97.5 %`[2])
  # cov = coverage rate (95 out of 100 CI95%s should contain the 'true' value): "coverage rate (cov) of the 95% confidence interval"
  # cov.rate <- mean(cov.ind)
  
  # now apply colMeans for mean over nsims
  
  # and get se for plot with: sapply(df,function(x)sd(x)/sqrt(length(x)))
  
  
  # output
  return(c(bias, ciw, cov.ind))
}
