# create figure to display results
# requires the packages "dplyr", "ggplot2"
# and the object 'results' created with '1.Excecute'

# load package
library("dplyr")
library("ggplot2")
library("ggpubr")


# rename variables 
names(results) <-
  c("it", "bias", "CIW", "CR",  "R_mean", "R_var", "AC_mean", "AC_var")

# create
R_plot <- results %>% .[-1, ] %>% 
  ggplot(aes(x = it, y = R_var, color = "Chain variance")) +
  geom_line(linetype = "dashed", size = 1) +
  geom_line(aes(x = it, y = R_mean, color = "Chain mean")) +
  xlab("") +
  ylab(expression(paste(widehat(R)))) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_color_manual(name = "Legend",
                     values = c("Chain mean" = 1, "Chain variance" = 8), guide = "legend") +
  guides(colour = guide_legend(override.aes = list(linetype = c(1, 8)))) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.position =  c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )

AC_plot <- results %>% .[-1, ] %>% 
  ggplot(aes(x = it, y = AC_var, color = "Chain variance")) +
  geom_line(linetype = "dashed", size = 1) +
  geom_line(aes(x = it, y = AC_mean, color = "Chain mean")) +
  xlab("Number of iterations") +
  ylab("Auto-correlation") +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_color_manual(name = "Legend",
                     values = c("Chain mean" = 1, "Chain variance" = 8)) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black") ,
    legend.position = ""
  )

figure1 <-
  ggarrange(
    R_plot,
    AC_plot,
    labels = c("A", "B"),
    ncol = 1,
    nrow = 2,
    align = "v"
  )

figure1

bias_plot <- ggplot(results, aes(x = it, y = bias)) +
  geom_line() +
  xlab("") +
  ylab("Bias") +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )

CIW_plot <- ggplot(results, aes(x = it, y = CIW)) +
  geom_line() +
  xlab("") +
  ylab("CI width") +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )

cov_plot <- ggplot(results, aes(x = it, y = CR * 100)) +
  geom_line() +
  xlab("Number of iterations") +
  ylab("Coverage (%)") +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )

figure2 <-
  ggarrange(
    bias_plot,
    CIW_plot,
    cov_plot,
    labels = c("A", "B", "C"),
    ncol = 1,
    nrow = 3,
    align = "hv"
  )

figure2
