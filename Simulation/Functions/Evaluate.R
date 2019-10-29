# evaluation function
# requires packages 'miceadds' and 'dplyr'

# extract R hat values
# Rhat.mice(sims[[2]][[1]])
convergence.diag <- function(sims) {
  r.hat <- matrix(NA, nrow = n.iter, ncol = n.sim)
  convergence <- matrix(NA, n.iter, ncol = 4)
  # R hat
  for (i in 2:n.iter) {
    
    # r.hat[[i]] <- lapply(sims[[i]], Rhat.mice)
    for (j in 1:n.sim) {
      # extract R hat
      r.hat[i, j] <- max(as.numeric(Rhat.mice(sims[[i]][[j]])$Rhat.M.imp))
    }
    # # calculate your evaluations vector - so Rhat, bias, cov and ciw
    # # 2. unlist(eval.out)
    # # 3. Reduce("+", eval.out) / length(eval.out)
    # r.hat.out[[i]] <- r.hat
  }
  convergence[,1] <- mean.r.hat <- rowMeans(r.hat, na.rm = T)
  convergence[,2] <- conv.rate.broad <- rowMeans(r.hat < 1.2, na.rm = T)
  convergence[,3] <- conv.rate.middle <- rowMeans(r.hat < 1.1, na.rm = T)
  convergence[,4] <- conv.rate.narrow <- rowMeans(r.hat < 1.01, na.rm = T)
  
  #return(c(mean.r.hat, conv.rate.broad, conv.rate.middle, conv.rate.narrow))
  return(convergence)
}

# apply lm() on each sim, on each maxit value
# this works:
# summary(pool(lm.mids(V1~V2, test)), conf.int = T)
my.lm <-
  function(OUT)
    summary(pool(lm.mids(V1 ~ V2, OUT)), conf.int = T)
# this works:
# out1 <- sims[[1]]
# lapply(out1, my.lm)
# lapply(sims[[1]], my.lm)
# lapply(sims, lapply, my.lm)

evaluate.function <- function(OUT) {
  # set.seed(123)
  # eval.out <-  list()

  # for each sim, for each maxit value...
  # ... compute bias
  # bias <- mi.lm$est - compl.lm$est per sim, per maxit value
  bias <-
    as.numeric(OUT$estimate[2] - data$compl.lm$coefficients[2])

  # ... compute ciw: Wider intervals are associated with more uncertainty and the more narrow interval that is still properly covered indicates a sharper inference
  # ciw <- mi.lm$ci95_ul - mi.lm$ci95_ll
  ciw <-
    OUT$`97.5 %`[2] - OUT$`2.5 %`[2]

  # ... indicate if 'true' value is within ci95%
  # cov.ind <- ifelse(compl.lm$est %in% mi.lm$ci95, 1, 0)
  cov.ind <-
    dplyr::between(data$compl.lm$coefficients[2],
                   OUT$`2.5 %`[2],
                   OUT$`97.5 %`[2])

  # cov = coverage rate (95 out of 100 ci95%s should contain the 'true' value)
  # cov.rate <- mean(cov.ind)

  # output
  return(c(bias, ciw, cov.ind))

}


# extract.function <- function(OUT) {
#   extr.out <-  list()
#   # this is what we want for each sim and each maxit value: evals[[1]][[1]][1]
#   mean.bias <- mean(OUT[1])
#   mean.ciw <- mean(OUT[2])
#   mean.cov <- mean(OUT[3])
#   extr.out <- c(mean.bias, mean.ciw, mean.cov)
#  return(extr.out) 
# }

# evaluate.function <- function(mi.lm) {
#   eval.out <-  list()
#   
#   # for each sim, for each maxit value...
#   # ... compute bias
#   # bias <- mi.lm$est - compl.lm$est per sim, per maxit value
#   bias <-
#     mi.lm[[1]][[1]]$estimate[2] - data$compl.lm$coefficients[2]
#   
#   # ... compute ciw: Wider intervals are associated with more uncertainty and the more narrow interval that is still properly covered indicates a sharper inference
#   # ciw <- mi.lm$ci95_ul - mi.lm$ci95_ll
#   ciw <-
#     mi.lm[[1]][[1]]$`97.5 %`[2] - mi.lm[[1]][[1]]$`2.5 %`[2]
#   
#   # ... indicate if 'true' value is within ci95%
#   # cov.ind <- ifelse(compl.lm$est %in% mi.lm$ci95, 1, 0)
#   cov.ind <-
#     dplyr::between(data$compl.lm$coefficients[2],
#                    mi.lm[[1]][[1]]$`2.5 %`[2],
#                    mi.lm[[1]][[1]]$`97.5 %`[2])
#   
#   # cov = coverage rate (95 out of 100 ci95%s should contain the 'true' value)
#   cov.rate <- mean(cov.ind)
#   
#   # output
#   eval.out <- c(bias, ciw, cov.rate)
#   
# }
#sapply is 'sneller?' en geeft je direct matrix
#sapply(out, evaluate.function) en dan colMeans
# sapply(mylist, "[", y), where y = 2:n.iter

# evaluate for each simulated iteration in n.iter
# so 100 averaged (in evaluate.function()) simulations
# or use purrr::map()
# for (i in OUT) {
#   evaluate.function
# }
# 
# evaluate.function(sims)
# mi.lm <- lapply(sims[[1]], sapply, my.lm)
