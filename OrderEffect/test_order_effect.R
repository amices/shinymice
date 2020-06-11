# plot to check order effects
# met dezelfde seed in mice 
# note schrijven over deze sim met plotje van resultaten, dat het geen bal uitmaakt!

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
load("OrderEffect/regular.Rdata") 
reg <- results %>% cbind(sim = "Default")

# data order
load("OrderEffect/data_Y_first.Rdata") 
datY <- results %>% cbind(sim = "Data columns Y first")
load("OrderEffect/data_X3_first.Rdata") 
datX3 <- results %>% cbind(sim = "Data columns X3 first")

# visit sequence order
load("OrderEffect/visitseq_Y_first.Rdata") 
seqY <- results %>% cbind(sim = "Visit sequence Y first")
load("OrderEffect/visitseq_X2_first.Rdata") 
seqX2 <- results %>% cbind(sim = "Visit sequence X2 first")
load("OrderEffect/visitseq_X3_first.Rdata") 
seqX3 <- results %>% cbind(sim = "Visit sequence X3 first")

# connect datasets for better figure
full_dat <- rbind(reg, seqY, seqX2, seqX3, datY)

# plot for just one proportion
full_dat %>% 
  filter(p==.5) %>% 
  ggplot() +
  geom_line(aes(x=t, y=bias.est.X1, linetype = sim), color = paint5[2], size = 1) + # add this for percentage bias: /0.0206 
  geom_line(aes(x=t, y=bias.est.X2, linetype = sim), color = paint5[3], size = 1) + # add this for percentage bias: /0.0047 
  geom_line(aes(x=t, y=bias.est.X3, linetype = sim), color = paint5[4], size = 1) + # add this for percentage bias: /0.0076 
  scale_x_continuous(breaks = 1:10) +
  xlab("Number of iterations") +
  ylab("Bias in the regression estimate") +
  labs(linetype = "How was the order determined?")


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

