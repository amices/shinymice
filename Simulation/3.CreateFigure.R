# create figure to display results
# requires the packages "dplyr", "ggplot2"
# and the object 'results' created with '1.Excecute'

# load package
library("dplyr")
library("ggplot2")
library("ggpubr")

# if necessary, load results
# load("C:/Users/User/Desktop/shinyMice/Simulation/Results/results17.Rdata.Rdata")
# results <- dat

# create
R_plot <- results %>% .[-1, ] %>%
  ggplot(aes(x = It., y = R_var_X, color = "Chain variance")) +
  geom_line(linetype = "dashed", size = 1) +
  geom_line(aes(x = It., y = R_var_Z1, color = "Chain variance"), linetype = "dashed", size = 1) +
  geom_line(aes(x = It., y = R_var_Z2, color = "Chain variance"), linetype = "dashed", size = 1) +
  geom_line(aes(x = It., y = R_var_Y, color = "Chain variance"), linetype = "dashed", size = 1) +
  geom_line(aes(x = It., y = R_mean_X, color = "Chain mean")) +
  geom_line(aes(x = It., y = R_mean_Z1, color = "Chain mean")) +
  geom_line(aes(x = It., y = R_mean_Z2, color = "Chain mean")) +
  geom_line(aes(x = It., y = R_mean_Y, color = "Chain mean")) +
  geom_hline(yintercept = 1.1) +
  geom_hline(yintercept = 1.01) +
  xlab("") +
  ylab(expression(paste(widehat(R)))) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_color_manual(
    name = "Legend",
    values = c("Chain mean" = 1, "Chain variance" = 8),
    guide = "legend"
  ) +
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
  ggplot(aes(x = It., y = AC_var_X, color = "Chain variance")) +
  geom_line(linetype = "dashed", size = 1) +
  geom_line(aes(x = It., y = AC_var_Z1, color = "Chain variance"), linetype = "dashed", size = 1) +
  geom_line(aes(x = It., y = AC_var_Z2, color = "Chain variance"), linetype = "dashed", size = 1) +
  geom_line(aes(x = It., y = AC_var_Y, color = "Chain variance"), linetype = "dashed", size = 1) +
  geom_line(aes(x = It., y = AC_mean_X, color = "Chain mean")) +
  geom_line(aes(x = It., y = AC_mean_Z1, color = "Chain mean")) +
  geom_line(aes(x = It., y = AC_mean_Z2, color = "Chain mean")) +
  geom_line(aes(x = It., y = AC_mean_Y, color = "Chain mean")) +
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

bias_plot <- ggplot(results, aes(x = It., y = bias)) +
  geom_line() +
  geom_smooth(
    method = "loess",
    se = FALSE,
    colour = "black",
    size = 0.5,
    linetype = "dashed"
  ) +
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

CIW_plot <- ggplot(results, aes(x = It., y = CIW)) +
  geom_line() +
  geom_smooth(
    method = "loess",
    se = FALSE,
    colour = "black",
    size = 0.5,
    linetype = "dashed"
  ) +
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

cov_plot <- ggplot(results, aes(x = It., y = cov * 100)) +
  geom_line() +
  geom_smooth(
    method = "loess",
    se = FALSE,
    colour = "black",
    size = 0.5,
    linetype = "dashed"
  ) +
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
