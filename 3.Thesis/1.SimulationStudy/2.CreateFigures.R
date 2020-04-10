# create figure to display results
# requires the packages "dplyr", "ggplot2"
# and the object 'results' created with '1.Excecute'

# load packages
library("dplyr")
library("ggplot2")
library("patchwork")

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
dat %>% ggplot(aes(x = T, y = bias.mean.X1, color = as.factor(miss))) +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 2) +
  geom_point(size = .75) +
  geom_line() +
  xlab("Number of iterations") +
  ylab(expression(paste("Bias in ", hat(mu)))) +
  labs(colour = "Missingness (%)")

# Univariate: convergence chain mean
dat %>% ggplot(aes(x = T, y = R.mean.X1, color = as.factor(miss))) +
  geom_hline(yintercept = 1,
             color = "grey",
             lwd = 2) +
  geom_point(size = .75, na.rm = TRUE) +
  geom_line(na.rm = TRUE) +
  xlab("Number of iterations") +
  ylab(expression(paste(widehat(R)))) +
  labs(colour = "Missingness (%)")

dat %>% ggplot(aes(x = T, y = AC.mean.X1, color = as.factor(miss))) +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 2) +
  geom_point(size = .75, na.rm = TRUE) +
  geom_line(na.rm = TRUE) +
  xlab("Number of iterations") +
  ylab("Autocorrelation") +
  labs(colour = "Missingness (%)")

# # Univariate: variance estimate
# dat %>% ggplot(aes(x = T, y = bias.sd.X1, color = as.factor(miss))) +
#   geom_hline(yintercept = 0,
#              color = "grey",
#              lwd = 2) +
#   geom_point(size = .75) +
#   geom_line() +
#   xlab("Number of iterations") +
#   ylab(expression(paste("Bias in ", hat(sigma ^ 2)))) +
#   labs(colour = "Missingness (%)")
# 
# # Univariate: convergence chain variance
# dat %>% ggplot(aes(x = T, y = R.var.X1, color = as.factor(miss))) +
#   geom_hline(yintercept = 1,
#              color = "grey",
#              lwd = 2) +
#   geom_point(size = .75, na.rm = TRUE) +
#   geom_line(na.rm = TRUE) +
#   xlab("Number of iterations") +
#   ylab(expression(paste(widehat(R)))) +
#   labs(colour = "Missingness (%)")
# 
# dat %>% ggplot(aes(x = T, y = AC.var.X1, color = as.factor(miss))) +
#   geom_hline(yintercept = 0,
#              color = "grey",
#              lwd = 2) +
#   geom_point(size = .75, na.rm = TRUE) +
#   geom_line(na.rm = TRUE) +
#   xlab("Number of iterations") +
#   ylab("Autocorrelation") +
#   labs(colour = "Missingness (%)")

####################
### MULTIVARIATE ###
####################

# Bias in regression coefficient
dat %>% ggplot(aes(x = T, y = bias.est.X1, color = as.factor(miss))) +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 2) +
  geom_point(size = .75) +
  geom_line() +
  xlab("Number of iterations") +
  ylab(expression(paste("Bias in ", hat(beta)))) +
  labs(colour = "Missingness (%)")

# Coverage rate regression coefficient
dat %>% ggplot(aes(x = T, y = cov.est.X1, color = as.factor(miss))) +
  geom_hline(yintercept = .95,
             color = "grey",
             lwd = 2) +
  geom_point(size = .75) +
  geom_line() +
  xlab("Number of iterations") +
  ylab(expression(paste("Coverage of ", hat(beta)))) +
  labs(colour = "Missingness (%)")

# R squared
dat %>% ggplot(aes(x = T, y = bias.R.s, color = as.factor(miss))) +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 2) +
  geom_point(size = .75) +
  geom_line() +
  xlab("Number of iterations") +
  ylab(expression(paste("Bias in ", hat(R ^ 2)))) +
  labs(colour = "Missingness (%)")

# R hat for PCA
dat %>% ggplot(aes(x = T, y = R.PCA, color = as.factor(miss))) +
  geom_hline(yintercept = 1,
             color = "grey",
             lwd = 2) +
  geom_point(size = .75, na.rm = TRUE) +
  geom_line(na.rm = TRUE) +
  xlab("Number of iterations") +
  ylab(expression(paste(widehat(R)))) +
  labs(colour = "Missingness (%)")

# AC for PCA
dat %>% ggplot(aes(x = T, y = AC.PCA, color = as.factor(miss))) +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 2) +
  geom_point(size = .75, na.rm = TRUE) +
  geom_line(na.rm = TRUE) +
  xlab("Number of iterations") +
  ylab("Auto-correlation") +
  labs(colour = "Missingness (%)") 

########################
## NOG IETS MEE DOEN? ##
########################
# 
# RMSE_plot <- dat %>% filter(miss == .25) %>%
#   ggplot() +
#   geom_point(aes(x = T, y = RMSE.1, color = "m1"), size = .75) +
#   geom_point(aes(x = T, y = RMSE.2, color = "m2"), size = .75) +
#   geom_point(aes(x = T, y = RMSE.3, color = "m3"), size = .75) +
#   geom_point(aes(x = T, y = RMSE.4, color = "m4"), size = .75) +
#   geom_point(aes(x = T, y = RMSE.5, color = "m5"), size = .75) +
#   geom_line(aes(x = T, y = RMSE.1, color = "m1")) +
#   geom_line(aes(x = T, y = RMSE.2, color = "m2")) +
#   geom_line(aes(x = T, y = RMSE.3, color = "m3")) +
#   geom_line(aes(x = T, y = RMSE.4, color = "m4")) +
#   geom_line(aes(x = T, y = RMSE.5, color = "m5")) +
#   xlab("") +
#   ylab("RMSE") +
#   labs(colour = "Imputation") +
#   theme(legend.position = "")
# 
# MAE_plot <- dat %>% filter(miss == .25) %>%
#   ggplot() +
#   geom_point(aes(x = T, y = MAE.1, color = "m1"), size = .75) +
#   geom_point(aes(x = T, y = MAE.2, color = "m2"), size = .75) +
#   geom_point(aes(x = T, y = MAE.3, color = "m3"), size = .75) +
#   geom_point(aes(x = T, y = MAE.4, color = "m4"), size = .75) +
#   geom_point(aes(x = T, y = MAE.5, color = "m5"), size = .75) +
#   geom_line(aes(x = T, y = MAE.1, color = "m1")) +
#   geom_line(aes(x = T, y = MAE.2, color = "m2")) +
#   geom_line(aes(x = T, y = MAE.3, color = "m3")) +
#   geom_line(aes(x = T, y = MAE.4, color = "m4")) +
#   geom_line(aes(x = T, y = MAE.5, color = "m5")) +
#   xlab("") +
#   ylab("Mean absolute error") +
#   labs(colour = "Imputation") +
#   theme(legend.position = "")
# 
# PCA_plot <- dat %>% filter(miss == .25) %>%
#   ggplot() +
#   geom_point(aes(x = T, y = pca.1, color = "m1"), size = .75) +
#   geom_point(aes(x = T, y = pca.2, color = "m2"), size = .75) +
#   geom_point(aes(x = T, y = pca.3, color = "m3"), size = .75) +
#   geom_point(aes(x = T, y = pca.4, color = "m4"), size = .75) +
#   geom_point(aes(x = T, y = pca.5, color = "m5"), size = .75) +
#   geom_line(aes(x = T, y = pca.1, color = "m1")) +
#   geom_line(aes(x = T, y = pca.2, color = "m2")) +
#   geom_line(aes(x = T, y = pca.3, color = "m3")) +
#   geom_line(aes(x = T, y = pca.4, color = "m4")) +
#   geom_line(aes(x = T, y = pca.5, color = "m5")) +
#   xlab("Number of iterations") +
#   ylab("First PCA component") +
#   labs(colour = "Imputation")
# 
# RMSE_plot + MAE_plot + PCA_plot + plot_layout(nrow = 3) + plot_annotation(title = "Convergence of model errors")


######################################################

# # if wanted, add CI based on MCMC SEs
# geom_errorbar(
#       aes(x = T, ymin = bias.mean.Y.LL, ymax = bias.mean.Y.UL),
#       width = .2,
#       color = "grey"
#     ) +

# # if wanted, add loess line or average or stepwise line
# geom_smooth(aes(x = T, y = R.mean.X1, color = "X1"), se = FALSE) +
# geom_smooth(aes(x = T, y = CIW.est.4, color = "X3"), method = "lm", formula = "y ~1", se = F) +
# geom_step(aes(x = T, y = 100 * R.sq, color = "R^2"), direction = "mid") +
