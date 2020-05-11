# generate supplementary figures that display complete results
# (we only display conditions where T<51 in the manuscript)
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
  title = element_text(size = 8),
  legend.title = element_text(size = 7)
)

# load results and figures
load("Results/complete.Rdata") 
source("Figures/PlotResults.R")

# plot the performance measures
estimates <- mean_bias + sd_bias + Rsq_bias + est_bias + est_cov + est_ciw + plot_layout(ncol = 2, guides = "collect") + plot_annotation(tag_levels = "A", tag_suffix = ".", title = "Impact of non-convergence on statistical inferences (see thesis manuscript for definitions)")
ggsave("Figures/ResultsEstimates.png", estimates, width = 8.3, height =  5.8)

# plot the non-convergence diagnostics
diagnostics <- mean_Rh + mean_AC + var_Rh + var_AC + est_Rh + est_AC + PCA_Rh + PCA_AC + plot_layout(guides = "collect", ncol = 2) + plot_annotation(tag_levels = "A", tag_suffix = ".", title = "Non-convergence identified by diagnostics (see thesis manuscript for definitions)")
ggsave("Figures/ResultsDiagnostics.png", diagnostics, width = 8.3, height =  5.8)

