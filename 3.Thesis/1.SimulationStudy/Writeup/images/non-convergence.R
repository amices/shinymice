# non-conv
library(mice)
library(purrr)
library(dplyr)

source('./3.Thesis/1.SimulationStudy/Functions/Convergence_supplement.R')
source('./3.Thesis/1.SimulationStudy/Functions/Autocorrelation_supplement.R')

# Non-convergence
meth <- make.method(boys)
meth["bmi"] <- "~I(wgt / (hgt / 100)^2)"
imp.bmi1 <- mice(boys, meth = meth, maxit = 10,
                 print = FALSE, seed = 60109)

# plot
nonc <- plot(imp.bmi1, "wgt")[1]
nonc$layout <- c(1,1)
nonc[["condlevels"]][[".ms"]] <- c("", "sd")
nonc$ylab <- "Chain mean"
save(nonc, file = "3.Thesis/1.SimulationStudy/Writeup/images/nonc.Rdata")

# diagnostics
# adjust this to compute per iteration 
chainmean1 <- imp.bmi1$chainMean[3,,] #chain mean of wgt (rows are iterations, columns are imputations)
ac1 <- ac_adapted(chainmean1)
acf1 <- ac_adapted(chainmean1, "acf")
rhat1 <- rhat_adapted(chainmean1)

# Regular convergence
pred <- make.predictorMatrix(boys)
pred[c("hgt", "wgt"), "bmi"] <- 0
imp.bmi2 <- mice(boys, meth = meth, pred = pred, maxit = 10,
                 print = FALSE, seed = 60109)

# plot
conv <- plot(imp.bmi2, "wgt")[1]
conv$layout <- c(1,1)
conv[["condlevels"]][[".ms"]] <- c("", "sd")
conv$ylab <- "Chain mean"
save(conv, file = "3.Thesis/1.SimulationStudy/Writeup/images/conv.Rdata")

# diagnostics
# adjust this to compute per iteration 
chainmean2 <- imp.bmi2$chainMean[3,,] #chain mean of wgt (rows are iterations, columns are imputations)
ac2 <- ac_adapted(chainmean2)
acf2 <- ac_adapted(chainmean2, "acf")
rhat2 <- rhat_adapted(chainmean2)
