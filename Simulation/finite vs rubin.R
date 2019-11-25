library(mice)
library(dplyr)
library(magrittr)
library(purrr)

## RUBIN's RULES
test <- function(){
  y <- rnorm(1000)
  x <- rnorm(1000)
  d <- cbind(y, x)
  d <- ampute(d, mech = "MCAR", prop = .5)$amp
  imp <- mice(d, printFlag = FALSE, method = "norm")
  out <- imp %>% with(lm(y~x)) %>% pool %>% .$pooled
  #mids <- complete(imp, "all")
  #out <- mids %>% map(~lm(y ~ x, data = .$x)) %>% pool %>% .$pooled
  out$upper <- out$estimate + qt(.975, df = out$df) * sqrt(out$t)
  out$lower <- out$estimate - qt(.975, df = out$df) * sqrt(out$t)
  out$cov <- out$lower < 0 & 0 < out$upper
  return(out)
}

sim1 <- replicate(n = 1000, test(), simplify = FALSE)
Reduce("+", sim1) / length(sim1)

## FINITE RULES

y <- rnorm(1000)
x <- rnorm(1000)
d <- cbind(y, x)
truth <- lm(y ~ x) %>% coef %>% .[2]
test2 <- function(d, true){
  d <- ampute(d, mech = "MCAR", prop = .5)$amp
  imp <- mice(d, printFlag = FALSE, method = "norm")
  out <- imp %>% with(lm(y~x)) %>% pool %>% .$pooled
  #mids <- complete(imp, "all")
  #out <- mids %>% map(~lm(y ~ x, data = .$x)) %>% pool %>% .$pooled
  out$upper <- out$estimate + qt(.975, df = 4) * sqrt(out$b + out$b / 5)
  out$lower <- out$estimate - qt(.975, df = 4) * sqrt(out$b + out$b / 5)
  out$cov <- out$lower < true & true < out$upper
  return(out)
}

sim2 <- replicate(n = 1000, test2(d = d, true = truth), simplify = FALSE)
Reduce("+", sim2) / length(sim2)
