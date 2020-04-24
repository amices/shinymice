# create figure to display results
# requires the packages "dplyr", "ggplot2"
# and the object 'results' created with '1.Excecute'

# load packages
library(dplyr)
library(ggplot2)
library(patchwork)

# # if necessary, load data
# load("3.Thesis/1.SimulationStudy/Results/complete.Rdata")

# set default for plot layout
theme_update(
  plot.title = element_text(hjust = 0.5),
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  axis.line = element_line(colour = "black"),
  legend.key = element_blank(),
  legend.position = "bottom"
)


####################
#### UNIVARIATE ####
####################

# Univariate: mean estimate
mean_bias <- results %>% ggplot(aes(x = t, y = bias.mean.Y, color = as.factor(p*100))) +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 2) +
  geom_point(size = .75) +
  # geom_errorbar(
  #       aes(x = t, ymin = bias.mean.Y.LL, ymax = bias.mean.Y.UL),
  #       width = .2,
  #       color = "grey"
  #     ) +
  geom_line() +
  xlab("Number of iterations") +
  # ylab(expression(paste("Bias in ", hat(mu)))) +
  ylab(expression(paste(bar(Q) - Q))) +
  ggtitle("Bias in \bar{Q}; Q = \mu_Y = 25.81") +
  labs(colour = "Missingness (%)") 


# Univariate: convergence chain mean
mean_Rh <- results %>% ggplot(aes(x = t, y = max.r.hat.chain.mean.Y, color = as.factor(p*100))) +
  geom_hline(yintercept = 1,
             color = "grey",
             lwd = 2) +
  geom_point(size = .75, na.rm = TRUE) +
  geom_line(na.rm = TRUE) +
  xlab("Number of iterations") +
  ylab(expression(paste(widehat(R)))) +
  labs(colour = "Missingness (%)")

mean_AC <- results %>% ggplot(aes(x = t, y = ac.max.chain.mean.Y, color = as.factor(p*100))) +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 2) +
  geom_point(size = .75, na.rm = TRUE) +
  geom_line(na.rm = TRUE) +
  xlab("Number of iterations") +
  ylab("Autocorrelation") +
  labs(colour = "Missingness (%)")

# mean_ACF <- results %>% ggplot(aes(x = t, y = ACF.mean.X1, color = as.factor(p*100))) +
#   geom_hline(yintercept = 0,
#              color = "grey",
#              lwd = 2) +
#   geom_point(size = .75, na.rm = TRUE) +
#   geom_line(linetype = "dotted", na.rm = TRUE) +
#   geom_point(aes(x = t, y = AC.mean.X1, color = as.factor(p*100)), size = .75, na.rm = TRUE) +
#   geom_line(aes(x = t, y = AC.mean.X1, color = as.factor(p*100)), na.rm = TRUE) +
#   xlab("Number of iterations") +
#   ylab("Autocorrelation") +
#   labs(colour = "Missingness (%)")

####################
#### UNIVARIATE ####
####################

# Univariate: variance estimate
results %>% ggplot(aes(x = t, y = bias.sd.Y, color = as.factor(p*100))) +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 2) +
  # geom_errorbar(
  #       aes(x = t, ymin = bias.sd.Y.LL, ymax = bias.sd.Y.UL),
  #       width = .2,
  #       color = "grey"
  #     ) +
  geom_point(size = .75) +
  geom_line() +
  xlab("Number of iterations") +
  ylab(expression(paste("Bias in ", hat(sigma ^ 2)))) +
  labs(colour = "Missingness (%)")

# Univariate: convergence chain variance
results %>% ggplot(aes(x = t, y = max.r.hat.chain.var.Y, color = as.factor(p*100))) +
  geom_hline(yintercept = 1,
             color = "grey",
             lwd = 2) +
  geom_point(size = .75, na.rm = TRUE) +
  geom_line(na.rm = TRUE) +
  xlab("Number of iterations") +
  ylab(expression(paste(widehat(R)))) +
  labs(colour = "Missingness (%)")

results %>% ggplot(aes(x = t, y = ac.max.chain.var.Y, color = as.factor(p*100))) +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 2) +
  geom_point(size = .75, na.rm = TRUE) +
  geom_line(na.rm = TRUE) +
  xlab("Number of iterations") +
  ylab("Autocorrelation") +
  labs(colour = "Missingness (%)")

####################
### MULTIVARIATE ###
####################

# Bias in regression coefficient
est_bias <- results %>% ggplot(aes(x = t, y = bias.est.X1, color = as.factor(p*100))) +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 2) +
  geom_point(size = .75) +
  geom_line() +
  xlab("Number of iterations") +
  ylab(expression(paste("Bias in ", hat(beta)))) +
  labs(colour = "Missingness (%)")

# Coverage rate regression coefficient
est_cov <- results %>% ggplot(aes(x = t, y = cov.est.X1, color = as.factor(p*100))) +
  geom_hline(yintercept = .95,
             color = "grey",
             lwd = 2) +
  geom_point(size = .75) +
  geom_line() +
  xlab("Number of iterations") +
  ylab(expression(paste("Coverage of ", hat(beta)))) +
  labs(colour = "Missingness (%)")

# CI length
est_CIL <- results %>% ggplot(aes(x = t, y = CIW.est.X1, color = as.factor(p*100))) +
  # geom_hline(yintercept = .95,
  #            color = "grey",
  #            lwd = 2) +
  geom_point(size = .75) +
  geom_line() +
  xlab("Number of iterations") +
  ylab(expression(paste("Length 95% CI ", hat(beta)))) +
  labs(colour = "Missingness (%)")

# R hat for est
results %>% ggplot(aes(x = t, y = max.r.hat.beta, color = as.factor(p*100))) +
  geom_hline(yintercept = 1,
             color = "grey",
             lwd = 2) +
  geom_point(size = .75, na.rm = TRUE) +
  geom_line(na.rm = TRUE) +
  xlab("Number of iterations") +
  ylab(expression(paste(widehat(R)))) +
  labs(colour = "Missingness (%)")

# AC for est
results %>% ggplot(aes(x = t, y = ac.max.beta, color = as.factor(p*100))) +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 2) +
  geom_point(size = .75, na.rm = TRUE) +
  geom_line(na.rm = TRUE) +
  xlab("Number of iterations") +
  ylab("Auto-correlation") +
  labs(colour = "Missingness (%)") 


####################
### MULTIVARIATE ###
####################

# R squared
Rsq_bias <- results %>% ggplot(aes(x = t, y = bias.R.sq*100, color = as.factor(p*100))) +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 2) +
  geom_point(size = .75) +
  geom_line() +
  xlab("Number of iterations") +
  # ylab(expression(paste("Bias in ", hat(R ^ 2)))) +
  ylab("Bias in estimated explained variance") + # change to bias in bar(Q), Q = 19.25?
  labs(colour = "Missingness (%)")

# R hat for PCA
results %>% ggplot(aes(x = t, y = max.r.hat.pca, color = as.factor(p*100))) +
  geom_hline(yintercept = 1,
             color = "grey",
             lwd = 2) +
  geom_point(size = .75, na.rm = TRUE) +
  geom_line(na.rm = TRUE) +
  xlab("Number of iterations") +
  ylab(expression(paste(widehat(R)))) +
  labs(colour = "Missingness (%)")

# AC for PCA
results %>% ggplot(aes(x = t, y = ac.max.pca, color = as.factor(p*100))) +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 2) +
  geom_point(size = .75, na.rm = TRUE) +
  geom_line(na.rm = TRUE) +
  xlab("Number of iterations") +
  ylab("Auto-correlation") +
  labs(colour = "Missingness (%)") 

# PCA_ACF <- results %>% ggplot(aes(x = t, y = ACF.PCA, color = as.factor(p*100))) +
#   geom_hline(yintercept = 0,
#              color = "grey",
#              lwd = 2) +
#   geom_point(size = .75, na.rm = TRUE) +
#   geom_line(linetype = "dotted", na.rm = TRUE) +
#   geom_point(aes(x = t, y = AC.PCA, color = as.factor(p*100)), size = .75, na.rm = TRUE) +
#   geom_line(aes(x = t, y = AC.PCA, color = as.factor(p*100)), na.rm = TRUE) +
#   xlab("Number of iterations") +
#   ylab("Autocorrelation") +
#   labs(colour = "Missingness (%)")


########################
## NOG IETS MEE DOEN? ##
########################

## shows that higher max(ac) means more bias in estimate
results[-c(1, 51, 101, 151, 201),] %>% ggplot(aes(x = ac.max.beta, y = bias.est.X1, color = as.factor(p*100))) +
  geom_point(na.rm = TRUE) + 
  geom_smooth(na.rm = TRUE, method = lm, se=F) +
  labs(colour = "Missingness (%)")

results[-c(1, 51, 101, 151, 201),] %>% ggplot(aes(x = max.r.hat.beta, y = bias.est.X1, color = as.factor(p*100))) +
  geom_point(na.rm = TRUE) + 
  geom_smooth(na.rm = TRUE, method = lm, se=F) +
  labs(colour = "Missingness (%)")

######################################################

# # if wanted, add CI based on MCMC SEs
# geom_errorbar(
#       aes(x = t, ymin = bias.mean.Y.LL, ymax = bias.mean.Y.UL),
#       width = .2,
#       color = "grey"
#     ) +

# # if wanted, add loess line or average or stepwise line
# geom_smooth(aes(x = t, y = R.mean.X1, color = "X1"), se = FALSE) +
# geom_smooth(aes(x = t, y = CIW.est.4, color = "X3"), method = "lm", formula = "y ~1", se = F) +
# geom_step(aes(x = t, y = 100 * R.sq, color = "R^2"), direction = "mid") +
