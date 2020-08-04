# small sim for trending
# over-disp versie maken!

# set-up
library(dplyr)
library(ggplot2)
library(patchwork)
library(purrr)
source("Thesis/Functions/ComputeDiagnostics.R")
set.seed(11)
n <- 1000

# define colorblind friendly colors
paint3 <- c('#CCBB44', '#66CCEE', '#EE6677')

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

# create data
dat <- data.frame(
  t = rep(1:n,3),
  chain1 = c(
    rnorm(n, 0, 1),
    rnorm(n, 0, 1) + 1:n / (0.2 * n),
    rnorm(n, 0, 1) + ((n - (1:n)) / (0.5 * n)) ^ 2),
  chain2 = c(
    rnorm(n, 0, 1), 
    rnorm(n, 0, 1) + 1:n / (0.2 * n),
    rnorm(n, 0, 1) - ((n - (1:n)) / (0.5 * n)) ^ 2),
  scenario = c(rep("stat", n), rep("trend", n), rep("overd", n)) 
)

# plot data
chains <- dat %>% 
  ggplot() +
  geom_line(aes(x = t, y = chain1, color = scenario)) +
  geom_line(aes(x = t, y = chain2, color = scenario)) +
  xlab("Iteration number") +
  ylab("Chain value") +
  labs(color = "Trending scenario")



# check convergence
stat <- dat %>% 
  filter(scenario == "stat") %>% select(c("chain1", "chain2")) %>% 
  convergence() %>% cbind(sim = "Stationary")
trend <- dat %>% 
  filter(scenario == "trend") %>% select(c("chain1", "chain2")) %>% 
  convergence() %>% cbind(sim = "Upward trending")
diver <- dat %>% 
  filter(scenario == "overd") %>% select(c("chain1", "chain2")) %>% 
  convergence() %>% cbind(sim = "Over-dispersion")
# combine
results_trend <- rbind(stat, trend, diver)

# plot results
rhat <-
  results_trend %>% ggplot(aes(x = iteration, y = r.hat.max, color = sim)) +
  geom_hline(yintercept = 1,
             color = "grey",
             lwd = 1) +
  geom_point(size = .25, na.rm = TRUE) +
  geom_line(size = .25, na.rm = TRUE) +
  xlab("Iteration number") +
  ylab(bquote("Adapted" ~ widehat(R))) +
  labs(color = "Trending scenario")

old_rhat <-
  results_trend %>% ggplot(aes(x = iteration, y = r.hat, color = sim)) +
  geom_hline(yintercept = 1,
             color = "grey",
             lwd = 1) +
  geom_point(size = .25, na.rm = TRUE) +
  geom_line(size = .25, na.rm = TRUE) +
  xlab("Iteration number") +
  ylab(bquote("Original" ~ widehat(R))) +
  labs(color = "Trending scenario")

ac <-
  results_trend %>% ggplot(aes(x = iteration, y = ac.max, color = sim)) +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 1) +
  geom_point(size = .25, na.rm = TRUE) +
  geom_line(size = .25, na.rm = TRUE) +

    xlab("Iteration number") +
  ylab("Autocorrelation") +
  labs(color = "Trending scenario")

chains + ac + old_rhat + rhat + plot_layout(ncol = 1)
