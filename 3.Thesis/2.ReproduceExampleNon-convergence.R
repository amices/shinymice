# Reproduce example of pathological non-convergence traceplots
# and apply non-convergence diagnostics
# requires the packages 'mice', 'purrr', and 'dplyr'/'magrittr'
# and the non-convergence diagnostics functions

# load packages and functions
library(mice)
library(purrr)
library(dplyr)
source('./3.Thesis/Functions/Convergence.R')
source('./3.Thesis/Functions/AC.R')
source('./3.Thesis/Functions/AC_supplement.R')
source('./3.Thesis/Functions/Rhat.R')
source('./3.Thesis/Functions/Rhat_supplement.R')


#####################
# Pathological non-c.
#####################

# impute missingness with mis-specified imputation model
meth <- make.method(boys)
meth["bmi"] <- "~I(wgt / (hgt / 100)^2)"
nonconv <- mice(
  boys,
  meth = meth,
  maxit = 10,
  print = FALSE,
  seed = 60109
)

# save
save(nonconv, file = "3.Thesis/Results/example_nonconv.Rdata")


#####################
# Regular convergence
#####################

# impute missingness with correctly specified imputation model
pred <- make.predictorMatrix(boys)
pred[c("hgt", "wgt"), "bmi"] <- 0
conv <- mice(
  boys,
  meth = meth,
  pred = pred,
  maxit = 10,
  print = FALSE,
  seed = 60109
)

# save
save(conv, file = "3.Thesis/Results/example_conv.Rdata")


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

# save
save(diagnostics, file = "3.Thesis/Results/example_diagnostics.Rdata")
