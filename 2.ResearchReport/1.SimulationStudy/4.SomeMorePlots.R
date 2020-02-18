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

# create plot for autocorrelations
new_AC_plot <- results_with_CI %>% .[-1,] %>%
  # ggplot(aes(x = T, y = AC_var_X, color = "Chain variances")) +
  # geom_line(linetype = "dashed", size = 1) +
  # geom_line(
  #   aes(x = T, y = AC_var_Z1, color = "Chain variances"),
  #   linetype = "dashed",
  #   size = 1
  # ) +
  # geom_line(
  #   aes(x = T, y = AC_var_Z2, color = "Chain variances"),
  #   linetype = "dashed",
  #   size = 1
  # ) +
  # geom_line(aes(x = T, y = AC_var_Y, color = "Chain variances"),
  #           linetype = "dashed",
  #           size = 1) +
  ggplot() + 
  geom_line(aes(x = T, y = AC_mean_X, color = "Chain means")) +
  geom_line(aes(x = T, y = AC_mean_Z1, color = "Chain means")) +
  geom_line(aes(x = T, y = AC_mean_Z2, color = "Chain means")) +
  geom_line(aes(x = T, y = AC_mean_Y, color = "Chain means")) +
  xlab(" ") +
  ylab("Auto-correlation") +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(breaks = seq(-.8, .6, by = .2)) +
  scale_color_manual(name = "Legend", labels = c("Chain means", "Chain variances"), values = c("black", "grey"), guide = "legend") +
  theme_bw() +
  # theme(
  #   panel.border = element_blank(),
  #   panel.grid.major = element_blank(),
  #   panel.grid.minor = element_blank(),
  #   axis.line = element_line(colour = "black"),
  #   legend.position =  c(.95, .95),
  #   legend.justification = c("right", "top"),
  #   legend.box.just = "right",
  #   legend.margin = margin(6, 6, 6, 6)
  # )
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black") ,
    legend.position = ""
  )

# create between chain variances plot
between_plot <- results_with_CI %>% .[-1,] %>%
  # ggplot(aes(x = T, y = between_var_X, color = "Chain variances")) +
  # geom_line(linetype = "dashed", size = 1) +
  # geom_line(
  #   aes(x = T, y = between_var_Z1, color = "Chain variances"),
  #   linetype = "dashed",
  #   size = 1
  # ) +
  # geom_line(
  #   aes(x = T, y = between_var_Z2, color = "Chain variances"),
  #   linetype = "dashed",
  #   size = 1
  # ) +
  # geom_line(aes(x = T, y = between_var_Y, color = "Chain variances"),
  #           linetype = "dashed",
  #           size = 1) +
  ggplot() +
  geom_line(aes(x = T, y = between_mean_X, color = "Chain means")) +
  geom_line(aes(x = T, y = between_mean_Z1, color = "Chain means")) +
  geom_line(aes(x = T, y = between_mean_Z2, color = "Chain means")) +
  geom_line(aes(x = T, y = between_mean_Y, color = "Chain means")) +
  xlab(" ") +
  ylab("Between chain variance") +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  #scale_y_continuous(breaks = seq(-.8, .6, by = .2)) +
  scale_color_manual(name = "Legend", labels = c("Chain means", "Chain variances"), values = c("black", "grey"), guide = "legend") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black") ,
    legend.position = ""
  )

within_plot <- results_with_CI %>% .[-1,] %>%
  ggplot() +
  # ggplot(aes(x = T, y = within_mean_X, color = "Chain variances")) +
  # geom_line(linetype = "dashed", size = 1) +
  # geom_line(
  #   aes(x = T, y = within_var_Z1, color = "Chain variances"),
  #   linetype = "dashed",
  #   size = 1
  # ) +
  # geom_line(
  #   aes(x = T, y = within_var_Z2, color = "Chain variances"),
  #   linetype = "dashed",
  #   size = 1
  # ) +
  # geom_line(aes(x = T, y = within_var_Y, color = "Chain variances"),
  #           linetype = "dashed",
  #           size = 1) +
  geom_line(aes(x = T, y = within_mean_X, color = "Chain means")) +
  geom_line(aes(x = T, y = within_mean_Z1, color = "Chain means")) +
  geom_line(aes(x = T, y = within_mean_Z2, color = "Chain means")) +
  geom_line(aes(x = T, y = within_mean_Y, color = "Chain means")) +
  xlab("Number of iterations") +
  ylab("Within chain variance") +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  #scale_y_continuous(breaks = seq(-.8, .6, by = .2)) +
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
new_figure <-
  ggarrange(
    new_AC_plot,
    between_plot,
    within_plot,
    labels = c("A", "B", "C"),
    ncol = 1,
    nrow = 3,
    align = "v"
  )
# print figure
new_figure

