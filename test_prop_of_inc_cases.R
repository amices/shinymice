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
means <- colMeans(dat)
# betas <- lm(X1~., data = dat)$coefficients


simulation <- function(dataset, proportions){
# ampute data with different missingness
purrr::map_dfr(p.inc, function(p){
  mice::ampute(
    data = dat,
    prop = p,
    mech = "MCAR"
  )$amp %>% 
    mice::mice(., print = FALSE) %>% 
    mice::complete(., "all") %>% purrr::map_df(., ~ {
      colMeans(.)
    }) %>% 
    apply(., 1, mean) %>% 
    .[2] %>% 
    data.frame(est = ., prop = p)
})
}

# run simulation n.sim times
out <-
  replicate(n = n.sims,
            expr = simulation(dataset = dat, proportions = p.inc),
            simplify = FALSE) %>% 
  map_df(., ~ {
              as.data.frame(.)
  })

results <-  out %>% aggregate(. ~ prop, data = ., mean) %>% mutate(sd = aggregate(. ~ prop, data = out, sd)[,2])

save(results, file = "effect_of_missingness.Rdata")


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
  legend.position = "bottom",
  legend.margin = margin(0,0,0,0)
)

# plot
results %>% ggplot() + 
  geom_point(aes(x = prop, y = est)) +
  geom_hline(yintercept=means[2], color = "gray") +
  geom_errorbar(
        aes(x = prop, ymin = est-sd, ymax = est+sd),
        width = .02,
        color = "grey"
      ) + 
  xlab("Proportion of incomplete cases") + 
  ylab("Estimated mean") +
  ggtitle("Small simulation for effect of missingness", subtitle = "(MCAR; error bars are SD between 100 simulation runs)")
