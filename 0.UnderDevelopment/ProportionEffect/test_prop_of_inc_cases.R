# to do: korte note schrijven over hoe de hoeveelheid missingness regelateerd is aan de estimates --> bias trend boven 50% miss
# does the miss prop shift the bias towards a particular direction?

# set-up environment
library(dplyr)
library(mice)
library(purrr)
library(ggplot2)

# set random seed
set.seed(1111)

# set proportions
# p.inc <- c(.05, .25, .5, .75, .95)
p.inc <- seq(0.05, 0.95, by = 0.05)

# set number of simulations
n.sims <- 1000

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
means <- colMeans(dat)
# betas <- lm(X1~., data = dat)$coefficients

# create patterns to ampute the data with multivariate missingness
amp.pat <-
  expand.grid(c(0, 1), c(0, 1), c(0, 1)) %>% #define all possible combinations of univariate and multivariate missingness
  .[c(-1, -8),] #remove the completely (un)observed cases
names(amp.pat) <-
  ampute(dat)$patterns %>% names() #obtain correct names of patterns

simulation <- function(dataset, proportions, pattern) {
  # ampute data with different missingness
  purrr::map_dfr(p.inc, function(p) {
    mice::ampute(
      data = dat,
      patterns = pattern,
      prop = p,
      mech = "MCAR"
    )$amp %>%
      mice::mice(., print = FALSE) %>%
      mice::complete(., "all") %>% purrr::map_df(., ~ {
        colMeans(.)
      }) %>%
      colMeans() %>% .[2] %>% 
      data.frame(est = ., prop = p)
  })
}

# run simulation n.sim times
out <-
  replicate(
    n = n.sims,
    expr = simulation(
      dataset = dat,
      proportions = p.inc,
      pattern = amp.pat
    ),
    simplify = FALSE
  ) %>%
  map_df(., ~ {
    as.data.frame(.)
  })

save(out, file = "ProportionEffect/multivar_missingness_proportions_raw.Rdata")


results_prop <-  out %>%
  aggregate(. ~ prop, data = ., mean) %>%
  mutate(
    sd = aggregate(. ~ prop, data = out, sd)[, 2],
    ci_lo = aggregate(. ~ prop, data = out, quantile, probs = 0.025)[, 2],
    ci_hi = aggregate(. ~ prop, data = out, quantile, probs = 0.975)[, 2]
  )

save(results_prop, file = "ProportionEffect/multivar_missingness_proportions.Rdata")

###################################################################################
# load packages (again)
# set-up environment
library(dplyr)
library(mice)
library(purrr)
library(ggplot2)

# load data
source("ProportionEffect/multivar_missingness_proportions.Rdata")

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

# plot
results_prop %>% ggplot() +
  geom_point(aes(x = prop, y = est)) +
  # geom_hline(yintercept = 3.509, linetype = "dashed") +
  geom_errorbar(aes(x = prop, ymin = ci_lo, ymax = ci_hi),
                width = .02,
                color = "grey") +
  xlab("Proportion of incomplete cases") +
  ylab("Estimated mean") +
  ggtitle("Small simulation for effect of missingness", subtitle = "(MCAR; 5 iterations, 95% CI in 1000 simulation runs)")
