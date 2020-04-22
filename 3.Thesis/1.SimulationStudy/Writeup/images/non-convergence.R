# non-conv
library(mice)

meth <- make.method(boys)
meth["bmi"] <- "~I(wgt / (hgt / 100)^2)"
imp.bmi1 <- mice(boys, meth = meth, maxit = 10,
                 print = FALSE, seed = 60109)
nonc <- plot(imp.bmi1, "wgt")[1]
nonc$layout <- c(1,1)
nonc[["condlevels"]][[".ms"]] <- c("", "sd")
nonc$ylab <- "Chain mean"

save(nonc, file = "3.Thesis/1.SimulationStudy/Writeup/images/nonc.Rdata")

pred <- make.predictorMatrix(boys)
pred[c("hgt", "wgt"), "bmi"] <- 0
imp.bmi2 <- mice(boys, meth = meth, pred = pred, maxit = 10,
                 print = FALSE, seed = 60109)
conv <- plot(imp.bmi2, "wgt")[1]
conv$layout <- c(1,1)
conv[["condlevels"]][[".ms"]] <- c("", "sd")
conv$ylab <- "Chain mean"

save(conv, file = "3.Thesis/1.SimulationStudy/Writeup/images/conv.Rdata")
