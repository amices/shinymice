# reproduce example of pathological non-convergence traceplots
# by van Buuren (2018) and apply non-convergence diagnostics
# requires the packages 'mice', 'purrr', and 'dplyr'/'magrittr'
# and the non-convergence diagnostics function

# load packages and function
library(mice)
library(purrr)
library(dplyr)
source('Functions/ComputeDiagnostics.R')


#####################
# Pathological non-c.
#####################

# impute missingness with mis-specified imputation model
meth <- make.method(boys) #define imputation model
meth["bmi"] <- "~I(wgt / (hgt / 100)^2)" #mis-specify model
nonconv <- mice( #impute missingness
  boys,
  meth = meth,
  maxit = 10,
  print = FALSE,
  seed = 60109
)

# save results
save(nonconv, file = "Results/example_nonconv.Rdata")


#####################
# Regular convergence
#####################

# impute missingness with correctly specified imputation model
pred <- make.predictorMatrix(boys) #mend imputation method
pred[c("hgt", "wgt"), "bmi"] <- 0 #remove dependency
conv <- mice( #impute missingness
  boys,
  meth = meth,
  pred = pred,
  maxit = 10,
  print = FALSE,
  seed = 60109
)

# save results
save(conv, file = "Results/example_conv.Rdata")


#####################
# Apply diagnostics
#####################

# apply diagnostics on chain means of wgt (rows are iterations, columns are imputations)
diagnostics1 <-
  nonconv$chainMean[3, , ] %>% 
  cbind(patho = "Non-convergence", convergence(., include_acf = TRUE), .) %>% 
  mutate(signif = c(NA, NA, 1, qnorm((1 + .95) / 2) / sqrt(iteration[-(1:3)])))

# repeat for typical convergence and combine
diagnostics <-
  conv$chainMean[3, , ] %>% 
  cbind(patho = "Typical convergence", convergence(., include_acf = TRUE), .) %>% 
  mutate(signif = c(NA, NA, 1, qnorm((1 + .95) / 2) / sqrt(iteration[-(1:3)]))) %>% 
  rbind(diagnostics1)

# save results
save(diagnostics, file = "Results/example_diagnostics.Rdata")
