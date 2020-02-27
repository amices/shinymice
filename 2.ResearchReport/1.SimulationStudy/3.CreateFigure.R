# create figure to display results
# requires the packages "dplyr", "ggplot2"
# and the object 'results' created with '1.Excecute'

# load package
library("dplyr")
library("ggplot2")
library("ggpubr")

# # if necessary, load results
load("C:/Users/User/Desktop/shinyMice/2.ResearchReport/1.SimulationStudy/Results/full_results.Rdata")
# results <- dat
results_with_CI <- dat #results
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
  scale_color_manual(name = "Legend", labels = c("Chain means", "Chain variances"), values = c("black", "grey"), guide = "legend") +
  guides(color = guide_legend(override.aes = list(linetype = c(1, 8)))) +
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
  scale_color_manual(name = "Legend", labels = c("Chain means", "Chain variances"), values = c("black", "grey"), guide = "legend") +
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
  geom_point() +
  geom_smooth(
    method = "loess",
    se = FALSE,
    colour = "black",
    size = 0.5,
    linetype = "dashed"
  ) +
  geom_errorbar(
    aes(ymin = bias_LL, ymax = bias_UL),
    width = .2,
    colour = "grey"
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

###################

bias_univ_plot <- ggplot(results_with_CI, aes(x = T, y = bias_X, color = "X")) +
  geom_point() +
  geom_smooth(se = F) +
  # geom_errorbar(
  #   aes(ymin = bias_X_LL, ymax = bias_X_UL),
  #   width = .2,
  #   colour = "grey"
  # ) +
  geom_point(aes(x = T, y = bias_Y, color = "Y")) +
  geom_smooth(aes(x = T, y = bias_Y, color = "Y"), se = F) +
  geom_point(aes(x = T, y = bias_Z1, color = "Z1")) +
  geom_smooth(aes(x = T, y = bias_Z1, color = "Z1"), se = F) +
  geom_point(aes(x = T, y = bias_Z2, color = "Z2")) +
  geom_smooth(aes(x = T, y = bias_Z2, color = "Z2"), se = F) +
  xlab("Number of iterations") +
  ylab("Bias") +
  #scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )

AC_means_plot <- results_with_CI %>% .[-1,] %>%
  ggplot() +
  geom_point(aes(x = T, y = AC_mean_X, color = "X")) +
  geom_point(aes(x = T, y = AC_mean_Y, color = "Y")) +
  geom_point(aes(x = T, y = AC_mean_Z1, color = "Z1")) +
  geom_point(aes(x = T, y = AC_mean_Z2, color = "Z2")) +
  geom_smooth(aes(x = T, y = AC_mean_X, color = "X"), se = FALSE) +
  geom_smooth(aes(x = T, y = AC_mean_Y, color = "Y"), se = FALSE) +
  geom_smooth(aes(x = T, y = AC_mean_Z1, color = "Z1"), se = FALSE) +
  geom_smooth(aes(x = T, y = AC_mean_Z2, color = "Z2"), se = FALSE) +
  xlab("Number of iterations") +
  ylab("Auto-correlation") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black") #,
    #legend.position = ""
  )

R_means_plot <- results_with_CI %>% .[-1,] %>%
  ggplot() +
  geom_point(aes(x = T, y = R_mean_X, color = "X")) +
  geom_point(aes(x = T, y = R_mean_Y, color = "Y")) +
  geom_point(aes(x = T, y = R_mean_Z1, color = "Z1")) +
  geom_point(aes(x = T, y = R_mean_Z2, color = "Z2")) +
  geom_smooth(aes(x = T, y = R_mean_X, color = "X"), se = FALSE) +
  geom_smooth(aes(x = T, y = R_mean_Y, color = "Y"), se = FALSE) +
  geom_smooth(aes(x = T, y = R_mean_Z1, color = "Z1"), se = FALSE) +
  geom_smooth(aes(x = T, y = R_mean_Z2, color = "Z2"), se = FALSE) +
  xlab("Number of iterations") +
  #ylab(expression(paste(widehat(R), " (in chain means)"))) +
  ylab(expression(paste(widehat(R)))) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black") #,
    #legend.position = ""
  )

univar_plot <-
  ggarrange(
    R_means_plot,
    AC_means_plot,
    bias_univ_plot,
    ncol = 1,
    nrow = 3,
    align = "v"
  )
# print figure
univar_plot
