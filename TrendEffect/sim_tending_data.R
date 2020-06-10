# small sim for trending

# set-up
library(dplyr)
library(ggplot2)
library(patchwork)
source("Functions/ComputeDiagnostics.R")
set.seed(11)
n <- 100

# define colorblind friendly colors
paint3 <- c('#CCBB44', '#66CCEE','#EE6677')

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
  stat1 = rnorm(n, 0, 1),
  stat2 = rnorm(n, 0, 1),
  up1 = rnorm(n, 0, 1)+1:n/10, 
  up2 = rnorm(n, 0, 1)+1:n/10,
  up3 = rnorm(n, 0, 1)+1:n/10,
  down = rnorm(n, 0, 1)-1:n/10,
  t = 1:n)

# plot data
chains <- dat %>% 
  ggplot() + 
  geom_line(aes(x=t, y=stat1), color = paint3[1]) +
  geom_line(aes(x=t, y=stat2), color = paint3[1]) +
  geom_line(aes(x=t, y=up1), color = paint3[2]) + 
  geom_line(aes(x=t, y=up2), color = paint3[2]) +
  geom_line(aes(x=t, y=up3), color = paint3[3]) +
  geom_line(aes(x=t, y=down), color = paint3[3]) + 
  xlab("Iteration number") +
  ylab("Chain value") 
  

# check convergence
stat <- convergence(dat[,c("stat1", "stat2")]) %>% cbind(sim = "Stationary")
trend <- convergence(dat[,c("up1", "up2")]) %>% cbind(sim = "Upward trending")
diver <- convergence(dat[,c("up3", "down")]) %>% cbind(sim = "Divergence")
# combine
results <- rbind(stat, trend, diver)

# plot results
rhat <- results %>% ggplot(aes(x = iteration, y = r.hat.max, color = sim)) +
  geom_hline(yintercept = 1,
             color = "grey",
             lwd = 1) +
  geom_point(size = .25, na.rm = TRUE) +
  geom_line(size = .25, na.rm = TRUE) +
  scale_colour_manual(values=paint3) +
  xlab("Iteration number") +
  ylab(bquote("Adapted"~widehat(R))) +
  labs(color = "Trending scenario")  

old_rhat <- results %>% ggplot(aes(x = iteration, y = r.hat, color = sim)) +
  geom_hline(yintercept = 1,
             color = "grey",
             lwd = 1) +
  geom_point(size = .25, na.rm = TRUE) +
  geom_line(size = .25, na.rm = TRUE) +
  scale_colour_manual(values=paint3) +
  xlab("Iteration number") +
  ylab(bquote("Original"~widehat(R))) +
  labs(color = "Trending scenario")  

ac <- results %>% ggplot(aes(x = iteration, y = ac.max, color = sim)) +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 1) +
  geom_point(size = .25, na.rm = TRUE) +
  geom_line(size = .25, na.rm = TRUE) +
  scale_colour_manual(values=paint3) +
  xlab("Iteration number") +
  ylab("Autocorrelation") +
  labs(color = "Trending scenario")  

chains + rhat + old_rhat + ac + plot_layout(guides = "collect", ncol = 1)

