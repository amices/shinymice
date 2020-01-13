# Figure for presentation
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

bias_plot <- ggplot(results_with_CI, aes(x = T, y = bias)) +
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

CIW_plot <- ggplot(results_with_CI, aes(x = T, y = CIW)) +
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

cov_plot <- ggplot(results_with_CI, aes(x = T, y = cov * 100)) +
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
    #labels = c("A", "B", "C"),
    ncol = 1,
    nrow = 3,
    align = "hv"
  )

figure2

# create
R_plot <- results_with_CI %>% .[-1,] %>%
  ggplot(aes(x = T, y = R_mean_X)) +
  geom_line() +
  geom_smooth(
    method = "loess",
    se = FALSE,
    colour = "grey",
    size = 0.5,
    linetype = "dashed"
  ) + 
  xlab("") +
  ylab(expression(paste(widehat(R)))) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.position =  ""
  )

AC_plot <- results_with_CI %>% .[-1,] %>%
  ggplot(aes(x = T, y = AC_mean_X)) +
  geom_line() +
  geom_smooth(
    method = "loess",
    se = FALSE,
    colour = "grey",
    size = 0.5,
    linetype = "dashed"
  ) + 
  xlab("Number of iterations") +
  ylab("Auto-correlation") +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
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
    #labels = c("A", "B"),
    ncol = 1,
    nrow = 2,
    align = "v"
  )

figure1

# cov without x label
cov_plot2 <- ggplot(results_with_CI, aes(x = T, y = cov * 100)) +
  geom_line() +
  geom_smooth(
    method = "loess",
    se = FALSE,
    colour = "black",
    size = 0.5,
    linetype = "dashed"
  ) +
  xlab("") +
  ylab("Coverage (%)") +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )

figure3 <-
  ggarrange(
    bias_plot,
    CIW_plot,
    cov_plot2,
    R_plot,
    AC_plot,
    #labels = c("A", "B", "C", "D", "E"),
    ncol = 1,
    nrow = 5,
    align = "v"
  )
figure3
