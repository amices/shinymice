# plot to check order effects

# load packages
library(dplyr) #version 0.8.5
library(ggplot2) #version 3.3.0
library(patchwork) #version 1.0.0

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
load("Results/regular.Rdata") 
reg <- results
load("Results/visitseq_Y_first.Rdata") 
vis <- results
load("Results/data_Y_first.Rdata") 
dat <- results

# regression coeff
ggplot() +
  geom_line(aes(x = reg$t, y = reg$bias.est.X1, color = as.factor(reg$p*100)), size = .25, na.rm = TRUE) +
  geom_line(aes(x = vis$t, y = vis$bias.est.X1, color = as.factor(vis$p*100)), linetype = "dashed", size = .25, na.rm = TRUE) +
  geom_line(aes(x = dat$t, y = dat$bias.est.X1, color = as.factor(dat$p*100)), linetype = "dotted", size = .25, na.rm = TRUE) +
  scale_colour_manual(values=paint5) +
  scale_x_continuous(breaks = 1:10) +
  xlab("Number of iterations") +
  ylab(bquote("Bias (Q = " ~ beta[1] ~ "= 2.06)")) +
  labs(colour = "Proportion of missing cases (%)")

# r squared
ggplot() +
  geom_line(aes(x = reg$t, y = reg$bias.R.sq, color = as.factor(reg$p*100)), size = .25, na.rm = TRUE) +
  geom_line(aes(x = vis$t, y = vis$bias.R.sq, color = as.factor(vis$p*100)), linetype = "dashed", size = .25, na.rm = TRUE) +
  geom_line(aes(x = dat$t, y = dat$bias.R.sq, color = as.factor(dat$p*100)), linetype = "dotted", size = .25, na.rm = TRUE) +
  scale_colour_manual(values=paint5) +
  scale_x_continuous(breaks = 1:10) +
  xlab("Number of iterations") +
  ylab(bquote("Bias (Q = " ~ r^2 ~ "= 0.19)")) +
  labs(colour = "Proportion of missing cases (%)")

