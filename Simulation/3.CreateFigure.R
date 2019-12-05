# create figure to display results
# requires the packages "dplyr", "ggplot2"
# and the object 'results' created with '1.Excecute'

# load package
library("dplyr")
library("ggplot2")
library("ggpubr")


# create table with only specific rows
df <- results[-1,] 
names(df) <- c("it", "bias", "CIW", "CR",  "R_mean", "R_var", "AC_mean", "AC_var")

R_plot <- ggplot(df, aes(x = it, y = R_var, color = "Chain variance")) + 
  geom_line() +
  geom_line(aes(x = it, y = R_mean, color = "Chain mean")) +
  #geom_smooth(se = FALSE) +
  xlab("Number of iterations") + ylab("PSRF") +
  scale_color_manual(name = "Maximum widehat{R} value", 
                     values = c("Chain mean" = 1, "Chain variance" = 8)) +
  #theme(legend.position = c(.8, .9)) +
  #theme_minimal()
  theme_bw() + 
  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"),
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )

AC_plot <- ggplot(df, aes(x = it, y = AC_var, color = "Chain variance")) + 
  geom_line() +
  geom_line(aes(x = it, y = AC_mean, color = "Chain mean")) +
  #geom_smooth(se = FALSE) +
  xlab("Number of iterations") + ylab("Auto-correlation") +
  scale_color_manual(name = "Maximum auto-correlation", 
                     values = c("Chain mean" = 1, "Chain variance" = 8)) +
  #theme(legend.position = c(.8, .9)) +
  #theme_minimal()
  theme_bw() + 
  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"),
    legend.position = c(.95, .20),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )

