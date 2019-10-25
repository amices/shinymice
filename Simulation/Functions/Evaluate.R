# evaluation function
# requires packages 'miceadds' and 'dplyr'

# extract R hat values
# Rhat.mice(sims[[2]][[1]])

convergence.diag <- function(sims) {
  eval.out <- r.hat <- r.hat.out <- list()
  
  # R hat
  for (i in 2:n.iter) {
    for (j in 1:n.sim) {
      # extract R hat
      r.hat[[j]] <- Rhat.mice(sims[[i]][[j]])
    }
    #calculate your evaluations vector - so Rhat, bias, cov and ciw
    # 2. unlist(eval.out)
    # 3. Reduce("+", eval.out) / length(eval.out)
    r.hat.out[[i]] <- r.hat
  }
  return(list(r.hat.out))
}

# evaluate.function <- function(OUT) {
#   eval.out <-  list()
#   
#   # apply lm()
#   # this works:
#   # summary(pool(lm.mids(V1~V2, test)), conf.int = T)
#   my.lm <-
#     function(OUT)
#       summary(pool(lm.mids(V1 ~ V2, OUT)), conf.int = T)
#   # this works:
#   # out1 <- sims[[1]]
#   # lapply(out1, my.lm)
#   # lapply(sims[[1]], my.lm)
#   # lapply(sims, lapply, my.lm)
#   mi.lm <- lapply(OUT, lapply, my.lm)
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
# 
# 
# #sapply is 'sneller?' en geeft je direct matrix
# #sapply(out, evaluate.function) en dan colMeans
# # sapply(mylist, "[", y), where y = 2:n.iter
# 
# # evaluate for each simulated iteration in n.iter
# # so 100 averaged (in evaluate.function()) simulations
# # or use purrr::map()
# for (i in OUT) {
#   evaluate.function
# }
# 
# evaluate.function(sims)
# mi.lm <- lapply(sims[[1]], sapply, my.lm)
