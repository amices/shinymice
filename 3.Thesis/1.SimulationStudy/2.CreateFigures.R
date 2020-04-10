# create figure to display results
# requires the packages "dplyr", "ggplot2"
# and the object 'results' created with '1.Excecute'

# filter by missingness percentage
miss_perc <- "75"

# load package
library("dplyr")
library("ggplot2")
library("patchwork")

theme_update(plot.title = element_text(hjust = 0.5), 
             panel.border = element_blank(),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             panel.background = element_blank(),
             axis.line = element_line(colour = "black"),
             legend.key = element_blank(),
             legend.position = "bottom")

# load data
# load("3.Thesis/1.SimulationStudy/Results/complete.Rdata")

# R squared
dat %>% ggplot(aes(x = T, y = bias.R.s, color = as.factor(miss))) + 
  geom_point() +
  geom_line() +
  ylab(expression(paste("Bias in ", hat(R ^ 2)))) +
  labs(colour = "Missingness (%)") +
  plot_annotation(title = "The effect of missingness percentage, an example")

# R hat
dat %>% ggplot(aes(x = T, y = R.PCA, color = as.factor(miss))) + 
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 1,
             color = "grey",
             lwd = 2) +
  ylab(expression(paste(widehat(R)))) +
  labs(colour = "Missingness (%)") +
  plot_annotation(title = "The effect of missingness percentage, an example")

# AC
dat %>% ggplot(aes(x = T, y = AC.PCA, color = as.factor(miss))) + 
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 2) +
  ylab("Auto-correlation") +
  labs(colour = "Missingness (%)") +
  plot_annotation(title = "The effect of missingness percentage, an example")

# Regression estimate(s)
# dat %>% ggplot() +
#   geom_point(aes(x = T, y = bias.est.X1, color = as.factor(miss))) +
#   geom_line(aes(x = T, y = bias.est.X1, color = as.factor(miss))) #+
#   # geom_point(aes(x = T, y = bias.est.X2, shape = "X2", color = as.factor(miss))) +
#   # geom_line(aes(x = T, y = bias.est.X2, color = as.factor(miss)))


######################################################
## Univariate estimates: variable means ##############
######################################################
bias_means_plot <- dat %>% filter(miss == miss_perc) %>%
  ggplot() +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 2) +
  geom_point(aes(x = T, y = bias.mean.Y, color = "Y"), size = .75) +
  geom_point(aes(x = T, y = bias.mean.X1, color = "X1"), size = .75) +
  geom_point(aes(x = T, y = bias.mean.X2, color = "X2"), size = .75) +
  geom_point(aes(x = T, y = bias.mean.X3, color = "X3"), size = .75) +
  geom_line(aes(x = T, y = bias.mean.Y, color = "Y")) +
  geom_line(aes(x = T, y = bias.mean.X1, color = "X1")) +
  geom_line(aes(x = T, y = bias.mean.X2, color = "X2")) +
  geom_line(aes(x = T, y = bias.mean.X3, color = "X3")) +
  xlab("") +
  ylab(expression(paste("Bias in ", hat(mu)))) +
  labs(colour = "Variable") +
  theme(legend.position = "")

R_means_plot <- dat %>% filter(miss == miss_perc) %>%
  ggplot() +
  geom_hline(yintercept = 1,
             color = "grey",
             lwd = 2) +
  geom_point(aes(x = T, y = R.mean.Y, color = "Y"), size = .75, na.rm = TRUE) +
  geom_point(aes(x = T, y = R.mean.X1, color = "X1"), size = .75, na.rm = TRUE) +
  geom_point(aes(x = T, y = R.mean.X2, color = "X2"), size = .75, na.rm = TRUE) +
  geom_point(aes(x = T, y = R.mean.X3, color = "X3"), size = .75, na.rm = TRUE) +
  geom_line(aes(x = T, y = R.mean.Y, color = "Y"), na.rm = TRUE) +
  geom_line(aes(x = T, y = R.mean.X1, color = "X1"), na.rm = TRUE) +
  geom_line(aes(x = T, y = R.mean.X2, color = "X2"), na.rm = TRUE) +
  geom_line(aes(x = T, y = R.mean.X3, color = "X3"), na.rm = TRUE) +
  xlab("") +
  ylab(expression(paste(widehat(R)))) +
  labs(colour = "Variable") +
  theme(legend.position = "")

AC_means_plot <- dat %>% filter(miss == miss_perc) %>%
  ggplot(na.rm = TRUE) +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 2) +
  geom_point(aes(x = T, y = AC.mean.Y, color = "Y"), size = .75, na.rm = TRUE) +
  geom_point(aes(x = T, y = AC.mean.X1, color = "X1"), size = .75, na.rm = TRUE) +
  geom_point(aes(x = T, y = AC.mean.X2, color = "X2"), size = .75, na.rm = TRUE) +
  geom_point(aes(x = T, y = AC.mean.X3, color = "X3"), size = .75, na.rm = TRUE) +
  geom_line(aes(x = T, y = AC.mean.Y, color = "Y"), na.rm = TRUE) +
  geom_line(aes(x = T, y = AC.mean.X1, color = "X1"), na.rm = TRUE) +
  geom_line(aes(x = T, y = AC.mean.X2, color = "X2"), na.rm = TRUE) +
  geom_line(aes(x = T, y = AC.mean.X3, color = "X3"), na.rm = TRUE) +
  xlab("Number of iterations") +
  ylab("Auto-correlation") +
  labs(colour = "Variable") 

bias_means_plot + R_means_plot + AC_means_plot + plot_layout(nrow = 3) + plot_annotation(title = "Convergence of means")

######################################################
## Univariate estimates: variable variances ##########
######################################################
bias_sds_plot <- dat %>% filter(miss == miss_perc) %>% 
  ggplot() +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 2) +
  geom_point(aes(x = T, y = bias.sd.Y, color = "Y"), size = .75) +
  geom_point(aes(x = T, y = bias.sd.X1, color = "X1"), size = .75) +
  geom_point(aes(x = T, y = bias.sd.X2, color = "X2"), size = .75) +
  geom_point(aes(x = T, y = bias.sd.X3, color = "X3"), size = .75) +
  geom_line(aes(x = T, y = bias.sd.Y, color = "Y")) +
  geom_line(aes(x = T, y = bias.sd.X1, color = "X1")) +
  geom_line(aes(x = T, y = bias.sd.X2, color = "X2")) +
  geom_line(aes(x = T, y = bias.sd.X3, color = "X3")) +
  xlab("") +
  ylab(expression(paste("Bias in ", hat(sigma)))) +
  labs(colour = "Variable") +
  theme(legend.position = "")

AC_sds_plot <- dat %>% filter(miss == miss_perc) %>%
  ggplot() +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 2) +
  geom_point(aes(x = T, y = AC.var.Y, color = "Y"), size = .75, na.rm = TRUE) +
  geom_point(aes(x = T, y = AC.var.X1, color = "X1"), size = .75, na.rm = TRUE) +
  geom_point(aes(x = T, y = AC.var.X2, color = "X2"), size = .75, na.rm = TRUE) +
  geom_point(aes(x = T, y = AC.var.X3, color = "X3"), size = .75, na.rm = TRUE) +
  geom_line(aes(x = T, y = AC.var.Y, color = "Y"), na.rm = TRUE) +
  geom_line(aes(x = T, y = AC.var.X1, color = "X1"), na.rm = TRUE) +
  geom_line(aes(x = T, y = AC.var.X2, color = "X2"), na.rm = TRUE) +
  geom_line(aes(x = T, y = AC.var.X3, color = "X3"), na.rm = TRUE) +
  xlab("Number of iterations") +
  ylab("Auto-correlation") +
  labs(colour = "Variable")

R_sds_plot <- dat %>% filter(miss == miss_perc) %>%
  ggplot() +
  geom_hline(yintercept = 1,
             color = "grey",
             lwd = 2) +
  geom_point(aes(x = T, y = R.var.Y, color = "Y"), size = .75, na.rm = TRUE) +
  geom_point(aes(x = T, y = R.var.X1, color = "X1"), size = .75, na.rm = TRUE) +
  geom_point(aes(x = T, y = R.var.X2, color = "X2"), size = .75, na.rm = TRUE) +
  geom_point(aes(x = T, y = R.var.X3, color = "X3"), size = .75, na.rm = TRUE) +
  geom_line(aes(x = T, y = R.var.Y, color = "Y"), na.rm = TRUE) +
  geom_line(aes(x = T, y = R.var.X1, color = "X1"), na.rm = TRUE) +
  geom_line(aes(x = T, y = R.var.X2, color = "X2"), na.rm = TRUE) +
  geom_line(aes(x = T, y = R.var.X3, color = "X3"), na.rm = TRUE) +
  xlab("") +
  ylab(expression(paste(widehat(R)))) +
  labs(colour = "Variable") +
  theme(legend.position = "")

bias_sds_plot + R_sds_plot + AC_sds_plot + plot_layout(nrow = 3) + plot_annotation(title = "Convergence of variances")

######################################################
## Multivariate estimates: regression coeff. #########
######################################################
bias_ests_plot <- dat %>% filter(miss == miss_perc) %>%
  ggplot() +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 2) +
  geom_point(aes(x = T, y = bias.est.X1, color = "X1"), size = .75) +
  geom_point(aes(x = T, y = bias.est.X2, color = "X2"), size = .75) +
  geom_point(aes(x = T, y = bias.est.X3, color = "X3"), size = .75) +
  geom_line(aes(x = T, y = bias.est.X1, color = "X1")) +
  geom_line(aes(x = T, y = bias.est.X2, color = "X2")) +
  geom_line(aes(x = T, y = bias.est.X3, color = "X3")) +
  xlab("") +
  ylab(expression(paste("Bias in ", hat(beta)))) +
  labs(colour = "Variable") +
  theme(legend.position = "")

cov_plot <- dat %>% filter(miss == miss_perc) %>%
  ggplot() +
  geom_hline(yintercept = .95,
             color = "grey",
             lwd = 2) +
  geom_point(aes(x = T, y = cov.est.X1, color = "X1"), size = .75) +
  geom_point(aes(x = T, y = cov.est.X2, color = "X2"), size = .75) +
  geom_point(aes(x = T, y = cov.est.X3, color = "X3"), size = .75) +
  geom_line(aes(x = T, y = cov.est.X1, color = "X1")) +
  geom_line(aes(x = T, y = cov.est.X2, color = "X2")) +
  geom_line(aes(x = T, y = cov.est.X3, color = "X3")) +
  ylab(expression(paste("Coverage of ", hat(beta)))) +
  xlab("Number of iterations") +
  labs(colour = "Variable")

ciw_plot <- dat %>% filter(miss == miss_perc) %>%
  ggplot() +
  geom_point(aes(x = T, y = CIW.est.2, color = "X1"), size = .75) +
  geom_point(aes(x = T, y = CIW.est.3, color = "X2"), size = .75) +
  geom_point(aes(x = T, y = CIW.est.4, color = "X3"), size = .75) +
  geom_line(aes(x = T, y = CIW.est.2, color = "X1")) +
  geom_line(aes(x = T, y = CIW.est.3, color = "X2")) +
  geom_line(aes(x = T, y = CIW.est.4, color = "X3")) +
  xlab("") +
  ylab(expression(paste("CI width of ", hat(beta)))) +
  labs(colour = "Variable") +
  theme(legend.position = "")

bias_ests_plot + ciw_plot + cov_plot + plot_layout(nrow = 3) + plot_annotation(title = "Convergence of regression coefficients")

######################################################
## Multivariate estimates: predictive perf. ##########
######################################################
bias_R_sq_plot <- dat %>% filter(miss == miss_perc) %>%
  ggplot() +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 2) +
  geom_point(aes(x = T, y = bias.R.s), color = "black", size = .75) +
  geom_line(aes(x = T, y = bias.R.s), color = "black") +
  xlab("") +
  ylab(expression(paste("Bias in ", hat(R ^ 2)))) +
  theme(legend.position = "")

bias_sigma_plot <- dat %>% filter(miss == miss_perc) %>%
  ggplot() +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 2) +
  geom_point(aes(x = T, y = bias.sigma), color = "black", size = .75) +
  geom_line(aes(x = T, y = bias.sigma), color = "black") +
  xlab("") +
  ylab(expression(paste("Bias in ", hat(sigma[epsilon]^2)))) +
  labs(colour = "Variable") +
  theme(legend.position = "")

bias_R_sq_plot + bias_sigma_plot + plot_layout(nrow = 2) + plot_annotation(title = "Convergence of coefficient of determination")

######################################################
## Multivariate estimates: NOG IETS MEE DOEN #########
######################################################
RMSE_plot <- dat %>% filter(miss == miss_perc) %>%
  ggplot() +
  geom_point(aes(x = T, y = RMSE.1, color = "m1"), size = .75) +
  geom_point(aes(x = T, y = RMSE.2, color = "m2"), size = .75) +
  geom_point(aes(x = T, y = RMSE.3, color = "m3"), size = .75) +
  geom_point(aes(x = T, y = RMSE.4, color = "m4"), size = .75) +
  geom_point(aes(x = T, y = RMSE.5, color = "m5"), size = .75) +
  geom_line(aes(x = T, y = RMSE.1, color = "m1")) +
  geom_line(aes(x = T, y = RMSE.2, color = "m2")) +
  geom_line(aes(x = T, y = RMSE.3, color = "m3")) +
  geom_line(aes(x = T, y = RMSE.4, color = "m4")) +
  geom_line(aes(x = T, y = RMSE.5, color = "m5")) +
  xlab("") +
  ylab("RMSE") +
  labs(colour = "Imputation") +
  theme(legend.position = "")

MAE_plot <- dat %>% filter(miss == miss_perc) %>%
  ggplot() +
  geom_point(aes(x = T, y = MAE.1, color = "m1"), size = .75) +
  geom_point(aes(x = T, y = MAE.2, color = "m2"), size = .75) +
  geom_point(aes(x = T, y = MAE.3, color = "m3"), size = .75) +
  geom_point(aes(x = T, y = MAE.4, color = "m4"), size = .75) +
  geom_point(aes(x = T, y = MAE.5, color = "m5"), size = .75) +
  geom_line(aes(x = T, y = MAE.1, color = "m1")) +
  geom_line(aes(x = T, y = MAE.2, color = "m2")) +
  geom_line(aes(x = T, y = MAE.3, color = "m3")) +
  geom_line(aes(x = T, y = MAE.4, color = "m4")) +
  geom_line(aes(x = T, y = MAE.5, color = "m5")) +
  xlab("") +
  ylab("Mean absolute error") +
  labs(colour = "Imputation") +
  theme(legend.position = "")

PCA_plot <- dat %>% filter(miss == miss_perc) %>%
  ggplot() +
  geom_point(aes(x = T, y = pca.1, color = "m1"), size = .75) +
  geom_point(aes(x = T, y = pca.2, color = "m2"), size = .75) +
  geom_point(aes(x = T, y = pca.3, color = "m3"), size = .75) +
  geom_point(aes(x = T, y = pca.4, color = "m4"), size = .75) +
  geom_point(aes(x = T, y = pca.5, color = "m5"), size = .75) +
  geom_line(aes(x = T, y = pca.1, color = "m1")) +
  geom_line(aes(x = T, y = pca.2, color = "m2")) +
  geom_line(aes(x = T, y = pca.3, color = "m3")) +
  geom_line(aes(x = T, y = pca.4, color = "m4")) +
  geom_line(aes(x = T, y = pca.5, color = "m5")) +
  xlab("Number of iterations") +
  ylab("First PCA component") +
  labs(colour = "Imputation") 

RMSE_plot + MAE_plot + PCA_plot + plot_layout(nrow = 3) + plot_annotation(title = "Convergence of model errors")


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

