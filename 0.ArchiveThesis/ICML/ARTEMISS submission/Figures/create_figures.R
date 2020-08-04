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
  text = element_text(size = 8),
  title = element_text(size = 8),
  legend.title = element_text(size = 8),
  axis.text=element_text(size=8),
  axis.title=element_text(size=8)
)

# load results and figures
load("Results/complete.Rdata") 
# test correlation between two thetas
r <- cor(results$r.hat.max.beta, results$r.hat.max.pca, use = "complete.obs", method = "spearman")
# test correlation between two thetas
r.ac <- cor(results$ac.max.beta, results$ac.max.pca, use = "complete.obs", method = "spearman")

# pre-processing
results <- results %>% filter(t<51)
results <- results %>% mutate(bias.est.X1 = bias.est.X1/.0206)

# create figures
# source("Figures/PlotResults.R")

# add diagnostic thresholds
results <- results %>% mutate(crit = qnorm((1 + .95) / 2) / sqrt(t), thresh1.01 = 1.01, thresh1.1 = 1.1, thresh1.2 = 1.2)
results$crit[results$crit>1] <- NA

# define colorblind friendly colors
paint5 <- c('#228833', '#66CCEE', '#CCBB44','#EE6677', '#AA3377')

# create plots
est_bias <- results %>% ggplot(aes(x = t, y = bias.est.X1, color = as.factor(p*100))) +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 1) +
  geom_point(size = .25, na.rm = TRUE) +
  geom_line(size = .25, na.rm = TRUE) +
  scale_colour_manual(values=paint5) +
  xlab("") +
  ylab("Bias (%)") +
  # ylab(bquote("Bias (Q = " ~ beta[1] ~ "= 2.06)")) +
  labs(colour = "Proportion of missing cases (%)")

est_Rh <- results %>% ggplot(aes(x = t, y = r.hat.max.beta, color = as.factor(p*100))) +
  geom_hline(yintercept = 1,
             color = "grey",
             lwd = 1) +
  geom_point(size = .25, na.rm = TRUE) +
  geom_line(size = .25, na.rm = TRUE) +
  geom_line(aes(x=t, y=thresh1.2), color = "grey", linetype = "dashed", size = .25, na.rm = TRUE) +
  geom_line(aes(x=t, y=thresh1.1), color = "grey", linetype = "dashed", size = .25, na.rm = TRUE) +
  geom_line(aes(x=t, y=thresh1.01), color = "grey", linetype = "dashed", size = .25, na.rm = TRUE) +
  scale_colour_manual(values=paint5) +
  scale_y_continuous(limits = c(1,1.53), breaks = c(1,1.1,1.2,1.3,1.4,1.5)) +
  xlab("Number of iterations") +
  # ylab(bquote(widehat(R)~" ("~theta~"= "~hat(Q)~")")) +
  ylab(bquote(widehat(R))) +
  labs(colour = "Proportion of missing cases (%)")

est_AC <- results %>% ggplot(aes(x = t, y = ac.max.beta, color = as.factor(p*100))) +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 1) +
  geom_line(aes(x = t, y = crit), color = "grey", linetype = "dashed", size = .25, na.rm = TRUE) + 
  geom_point(size = .25, na.rm = TRUE) +
  geom_line(size = .25, na.rm = TRUE) +
  scale_colour_manual(values=paint5) +
  xlab("Number of iterations") +
  # ylab(bquote("AC ("~theta~ " = "~hat(Q)~")")) +
  ylab("Autocorrelation") + 
  labs(colour = "Proportion of missing cases (%)")

est_cov <- results %>% ggplot(aes(x = t, y = cov.est.X1*100, color = as.factor(p*100))) +
  geom_hline(yintercept = 95,
             color = "grey",
             lwd = 1) +
  # geom_hline(yintercept = .936,
  #            color = "grey",
  #            lwd = 1) +
  # geom_hline(yintercept = .964,
  #            color = "grey",
  #            lwd = 1) +
  geom_point(size = .25, na.rm = TRUE) +
  geom_line(size = .25, na.rm = TRUE) +
  scale_colour_manual(values=paint5) +
  # xlab("") +
  xlab("") +
  ylab("Coverage rate (%)") +
  # ylab(bquote("CR (Q = " ~ beta[1] ~ ")")) +
  labs(colour = "Proportion of missing cases (%)")

# plot estimates, save 6.75x4.5in or 5.5
est_bias + est_cov + est_AC + est_Rh + plot_annotation(tag_levels = "A", tag_suffix = ".") + plot_layout(ncol = 2, guides = "collect") 



