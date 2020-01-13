# create figure to display results
# requires the packages "dplyr", "ggplot2"
# and the object 'results' created with '1.Excecute'

# load package
library("dplyr")
library("ggplot2")
library("ggpubr")

# # if necessary, load results
# load("C:/Users/User/Desktop/shinyMice/Simulation/Results/results.Rdata")
# results <- dat

# create plot for R hat
R_plot <- results_with_CI %>% .[-1,] %>%
  ggplot(aes(x = T, y = R_var_X, color = "Chain variances")) +
  geom_line(linetype = "dashed", size = 1) +
  geom_line(aes(x = T, y = R_var_Z1, color = "Chain variances"),
            linetype = "dashed",
            size = 1) +
  geom_line(aes(x = T, y = R_var_Z2, color = "Chain variances"),
            linetype = "dashed",
            size = 1) +
  geom_line(aes(x = T, y = R_var_Y, color = "Chain variances"),
            linetype = "dashed",
            size = 1) +
  geom_line(aes(x = T, y = R_mean_X, color = "Chain means")) +
  geom_line(aes(x = T, y = R_mean_Z1, color = "Chain means")) +
  geom_line(aes(x = T, y = R_mean_Z2, color = "Chain means")) +
  geom_line(aes(x = T, y = R_mean_Y, color = "Chain means")) +
  # # add error bar when `results_withSEs` is used
  # geom_errorbar(
  #   aes(ymin = R_mean_X_LL, ymax = R_mean_X_UL),
  #   width = .2,
  #   position = position_dodge(0.05)
  # ) +
  xlab("") +
  ylab(expression(paste(widehat(R)))) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(breaks = seq(.8, 2.2, by = .2)) +
  scale_color_manual(
    name = "Legend",
    values = c("Chain means" = 1, "Chain variances" = 8),
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

# create plot for autocorrelations
AC_plot <- results_with_CI %>% .[-1,] %>%
  ggplot(aes(x = T, y = AC_var_X, color = "Chain variances")) +
  geom_line(linetype = "dashed", size = 1) +
  geom_line(
    aes(x = T, y = AC_var_Z1, color = "Chain variances"),
    linetype = "dashed",
    size = 1
  ) +
  geom_line(
    aes(x = T, y = AC_var_Z2, color = "Chain variances"),
    linetype = "dashed",
    size = 1
  ) +
  geom_line(aes(x = T, y = AC_var_Y, color = "Chain variances"),
            linetype = "dashed",
            size = 1) +
  geom_line(aes(x = T, y = AC_mean_X, color = "Chain means")) +
  geom_line(aes(x = T, y = AC_mean_Z1, color = "Chain means")) +
  geom_line(aes(x = T, y = AC_mean_Z2, color = "Chain means")) +
  geom_line(aes(x = T, y = AC_mean_Y, color = "Chain means")) +
  # # add error bar when `results_withSEs` is used
  # geom_errorbar(
  #   aes(ymin = AC_mean_X_LL, ymax = AC_mean_X_UL),
  #   width = .2#,
  #   #position = position_dodge(0.05)
  # ) +
  xlab("Number of iterations") +
  ylab("Auto-correlation") +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(breaks = seq(-.8, .6, by = .2)) +
  scale_color_manual(name = "Legend",
                     values = c("Chain means" = 1, "Chain variances" = 8)) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black") ,
    legend.position = ""
  )

# combine convergence plots into figure
figure1 <-
  ggarrange(
    R_plot,
    AC_plot,
    labels = c("A", "B"),
    ncol = 1,
    nrow = 2,
    align = "v"
  )
# print figure
figure1

# create plot for bias
bias_plot <- ggplot(results_with_CI, aes(x = T, y = bias)) +
  geom_line() +
  geom_smooth(
    method = "loess",
    se = FALSE,
    colour = "black",
    size = 0.5,
    linetype = "dashed"
  ) +
  # # add error bar when `results_withSEs` is used
  # geom_errorbar(
  #   aes(ymin = bias_LL, ymax = bias_UL),
  #   width = .2,
  #   colour = "grey"
  # ) +
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

# create plot for confidence interval width
CIW_plot <- ggplot(results_with_CI, aes(x = T, y = CIW)) +
  geom_line() +
  geom_smooth(
    method = "loess",
    se = FALSE,
    colour = "black",
    size = 0.5,
    linetype = "dashed"
  ) +
  # # add error bar when `results_withSEs` is used
  # geom_errorbar(
  #   aes(ymin = CIW_LL, ymax = CIW_UL),
  #   width = .2,
  #   colour = "grey"
  # ) +
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

# create plot for coverage rate
cov_plot <- ggplot(results_with_CI, aes(x = T, y = cov * 100)) +
  geom_line() +
  geom_smooth(
    method = "loess",
    se = FALSE,
    colour = "black",
    size = 0.5,
    linetype = "dashed"
  ) +
  # regular CI is useless here! No error bars added.
  xlab("Number of iterations (T)") +
  ylab("Coverage (%)") +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )

# combine plots of simulation diagnostics into figure
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
# print figure
figure2

# combine all plots together for presentation
figure3 <-
  ggarrange(
    R_plot,
    AC_plot,
    bias_plot,
    CIW_plot,
    cov_plot,
    labels = c("A", "B", "C", "D", "E"),
    ncol = 1,
    nrow = 5,
    align = "v"
  )
# print figure
figure3
