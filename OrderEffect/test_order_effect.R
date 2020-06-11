# plot to check order effects

# set-up environment
library(dplyr)
library(mice)
library(purrr)
library(ggplot2)
library(patchwork) 


# set random seed
set.seed(1111)

# set proportions
# p.inc <- c(.05, .25, .5, .75, .95)

# set orders
var.order <- list(
  c("X1", "X2", "X3"), 
  c("X1", "X3", "X2"), 
  c("X2", "X1", "X3"), 
  c("X2", "X3", "X1"), 
  c("X3", "X1", "X2"), 
  c("X3", "X2", "X1"))

# set number of simulations
n.sims <- 100

# generate some data and get true values
# data generating mechanism = multivariate normal distribution
means <- c(12, 3, 0.5) #means
vars  <- c(4, 16, 9) #variances
R <- matrix(numeric(3 * 3), nrow = 3) #correlation matrix
diag(R) <- 1 #set diagonal to 1
R[upper.tri(R)] <-
  R[lower.tri(R)] <- c(.5, .3, .4) #set bivariate correlations
sigma <-
  diag(sqrt(vars)) %*% R %*% diag(sqrt(vars)) #variance-covariance matrix
dat <-
  as.data.frame(mvtnorm::rmvnorm(n = 100, mean = means, sigma = sigma)) #create data
colnames(dat) <- c("X1", "X2", "X3") #set predictors names

# true estimates
# means <- colMeans(dat)
betas <- lm(X1~., data = dat)$coefficients

# create patterns to ampute the data with multivariate missingness
amp.pat <-
  expand.grid(c(0, 1), c(0, 1), c(0, 1)) %>% #define all possible combinations of univariate and multivariate missingness
  .[c(-1,-8), ] #remove the completely (un)observed cases  
names(amp.pat) <- ampute(dat)$patterns %>% names() #obtain correct names of patterns

simulation <- function(dataset, orders, pattern){
  purrr::map_dfr(orders, .id = "ord", function(o){
    mice::ampute(
      data = dat,
      patterns = pattern,
      prop = .5,
      mech = "MCAR"
    )$amp %>% 
      mice::mice(., 
                 visitSequence = o, 
                 maxit = 2,
                 print = FALSE,
                 seed = 1111) %>% 
      mice::complete(., "all") %>% 
      map(., lm, formula = X1 ~ X2 + X3) %>% 
      pool() %>% 
      .$pooled %>% 
      .$estimate %>% 
      data.frame(est = ., var = c("Intercept", "X2", "X3"))
  })
}

# run simulation n.sim times
out <-
  replicate(n = n.sims,
            expr = simulation(dataset = dat, orders = var.order, pattern = amp.pat),
            simplify = FALSE) %>% 
  map_df(., ~ {
    as.data.frame(.)
  })

# save raw
save(out, file = "OrderEffect/order_effect_sim_raw.Rdata")

# evaluate across sims
results_ord <-  out %>% 
  aggregate(. ~ ord+var, data = ., mean) %>% 
  mutate(
  # sd = aggregate(. ~ ord+var, data = out, sd)[,3])#, 
    ci_lo = aggregate(. ~ ord+var, data = out, quantile, probs = 0.025)[,3],
    ci_hi = aggregate(. ~ ord+var, data = out, quantile, probs = 0.975)[,3])

# save results
save(results_ord, file = "OrderEffect/order_effect_sim.Rdata")


###############################################################################

# set default graphing behavior
theme_update(
  plot.title = element_text(hjust = 0.5),
  plot.subtitle = element_text(hjust = 0.5),
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  axis.line = element_line(colour = "black"),
  legend.key = element_blank(),
  legend.position = "bottom"
)

# define colorblind friendly colors
paint5 <- c('#228833', '#66CCEE', '#CCBB44','#EE6677', '#AA3377')

# load results
load("OrderEffect/order_effect_sim.Rdata") 

# plot for just one proportion
results_ord %>% filter(., var != "Intercept") %>% 
  ggplot(.) +
  geom_hline(yintercept=0.2425, linetype = "dashed", color ="gray") +
  geom_hline(yintercept=0.0977, linetype = "dashed", color ="gray") +
  #geom_jitter(aes(x=ord, y=est, color = var), width = 0.1, height = 0) +
  geom_point(aes(x=ord, y=est, color = var)) +
  geom_errorbar(
    aes(x = ord, ymin = ci_lo, ymax = ci_hi, color = var),
    width = .2,
    alpha = .25) 
  


# # regression coeff
# ggplot() +
#   geom_line(aes(x = reg$t, y = reg$bias.est.X1, color = as.factor(reg$p*100)), size = .25, na.rm = TRUE) +
#   geom_line(aes(x = vis$t, y = vis$bias.est.X1, color = as.factor(vis$p*100)), linetype = "dashed", size = .25, na.rm = TRUE) +
#   geom_line(aes(x = dat$t, y = dat$bias.est.X1, color = as.factor(dat$p*100)), linetype = "dotted", size = .25, na.rm = TRUE) +
#   scale_colour_manual(values=paint5) +
#   scale_x_continuous(breaks = 1:10) +
#   xlab("Number of iterations") +
#   ylab(bquote("Bias (Q = " ~ beta[1] ~ "= 2.06)")) +
#   labs(colour = "Proportion of missing cases (%)")
# 
# # r squared
# ggplot() +
#   geom_line(aes(x = reg$t, y = reg$bias.R.sq, color = as.factor(reg$p*100)), size = .25, na.rm = TRUE) +
#   geom_line(aes(x = vis$t, y = vis$bias.R.sq, color = as.factor(vis$p*100)), linetype = "dashed", size = .25, na.rm = TRUE) +
#   geom_line(aes(x = dat$t, y = dat$bias.R.sq, color = as.factor(dat$p*100)), linetype = "dotted", size = .25, na.rm = TRUE) +
#   scale_colour_manual(values=paint5) +
#   scale_x_continuous(breaks = 1:10) +
#   xlab("Number of iterations") +
#   ylab(bquote("Bias (Q = " ~ r^2 ~ "= 0.19)")) +
#   labs(colour = "Proportion of missing cases (%)")

