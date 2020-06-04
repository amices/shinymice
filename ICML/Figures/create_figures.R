# create ICML figures, save in size 5.5x8.5in to fit half a page
# requires the results file 'complete.Rdata'

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
  legend.position = "bottom",
  legend.margin = margin(0,0,0,0),
  text = element_text(size = 7),
  title = element_text(size = 7),
  legend.title = element_text(size = 7),
  axis.text=element_text(size=7),
  axis.title=element_text(size=7)
)

# load results and figures
load("Results/complete.Rdata") 
results <- results %>% filter(t<51)
source("Figures/PlotResults.R")

# plot estimates, save 6.75x4.5in or 5.5
mean_bias + est_bias + est_cov + est_ciw + plot_annotation(tag_levels = "A", tag_suffix = ".") + plot_layout(ncol = 1, guides = "collect") 


# plot diagnostics, save 6.75x6.5in or 8.0
mean_Rh + mean_AC + est_Rh + est_AC + PCA_Rh + PCA_AC + plot_layout(guides = "collect", ncol = 2) + plot_annotation(tag_levels = "A", tag_suffix = ".")

