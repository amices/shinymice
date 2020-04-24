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
  geom_line() +
  xlab("Number of iterations") +
  ylab(bquote("Bias ("~bar(Q)-Q~")")) +
  #ggtitle(bquote("A) Bias in " ~ bar(mu[Y]) ~ "; " ~ mu[Y] ~ "= 25.81")) +
  labs(colour = "Missingness (%)") 


# Univariate: convergence chain mean
mean_Rh <- results %>% ggplot(aes(x = t, y = r.hat.max.chain.mean.Y, color = as.factor(p*100))) +
  geom_hline(yintercept = 1,
             color = "grey",
             lwd = 2) +
  geom_point(size = .75, na.rm = TRUE) +
  geom_line(na.rm = TRUE) +
  xlab("") +
  ylab(bquote(widehat(R)~" ("~theta~")")) +
  #ggtitle(bquote("B) " ~ widehat(R) ~ "of chain means" ~ y[imp][",Y"])) +
  labs(colour = "Missingness (%)")

mean_AC <- results %>% ggplot(aes(x = t, y = ac.max.chain.mean.Y, color = as.factor(p*100))) +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 2) +
  geom_point(size = .75, na.rm = TRUE) +
  geom_line(na.rm = TRUE) +
  xlab("") +
  ylab(bquote("AC ("~theta~")")) +
  # ggtitle(bquote("Autocorrelation of chain means")) + #~ theta[mean])) +
  #ggtitle(bquote("C) Autocorrelation of chain means" ~ y[imp][",Y"])) +
  labs(colour = "Missingness (%)")

mean_Rh + mean_AC + mean_bias + plot_layout(guides = "collect", ncol = 1)

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
sd_bias <- results %>% ggplot(aes(x = t, y = bias.sd.Y, color = as.factor(p*100))) +
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
  # ylab(expression(paste("Bias in ", hat(sigma ^ 2)))) +
  ylab("Bias") +
  #ggtitle(bquote("A) Bias in " ~ bar(sigma[Y]) ~ "; " ~ sigma[Y] ~ "= 11.32")) +
  labs(colour = "Missingness (%)")

# Univariate: convergence chain variance
var_Rh <- results %>% ggplot(aes(x = t, y = r.hat.max.chain.var.Y, color = as.factor(p*100))) +
  geom_hline(yintercept = 1,
             color = "grey",
             lwd = 2) +
  geom_point(size = .75, na.rm = TRUE) +
  geom_line(na.rm = TRUE) +
  xlab("") +
  ylab(bquote(widehat(R)~" ("~theta~")")) +
  # ggtitle(bquote(widehat(R) ~ "of chain variance Y")) + # ~ "variance in" ~ y[imp][Y])) + #sigma[y[imp]]^2
  #ggtitle(bquote("B) " ~ widehat(R) ~ "of chain variances" ~ y[imp][",Y"])) +
  labs(colour = "Missingness (%)")

var_AC <- results %>% ggplot(aes(x = t, y = ac.max.chain.var.Y, color = as.factor(p*100))) +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 2) +
  geom_point(size = .75, na.rm = TRUE) +
  geom_line(na.rm = TRUE) +
  xlab("") +
  ylab(bquote("AC ("~theta~")")) +
  #ggtitle(bquote("C) Autocorrelation of chain variances" ~ y[imp][",Y"])) +
  labs(colour = "Missingness (%)")

var_Rh + var_AC + sd_bias + plot_layout(guides = "collect", ncol = 1)

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
  ylab("Bias") +
  #ggtitle(bquote("A) Bias in " ~ bar(beta[2]) ~ "; " ~ beta[2] ~ "= 2.06")) +
  labs(colour = "Missingness (%)")

est_Rh <- results %>% ggplot(aes(x = t, y = r.hat.max.beta, color = as.factor(p*100))) +
  geom_hline(yintercept = 1,
             color = "grey",
             lwd = 2) +
  geom_point(size = .75, na.rm = TRUE) +
  geom_line(na.rm = TRUE) +
  xlab("") +
  ylab(bquote(widehat(R)~" ("~theta~")")) +
  #ggtitle(bquote("B) " ~ widehat(R) ~ "of " ~ hat(beta[2]))) +
  labs(colour = "Missingness (%)")

est_AC <- results %>% ggplot(aes(x = t, y = ac.max.beta, color = as.factor(p*100))) +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 2) +
  geom_point(size = .75, na.rm = TRUE) +
  geom_line(na.rm = TRUE) +
  xlab("") +
  ylab(bquote("AC ("~theta~")")) +
  #ggtitle(bquote("C) Autocorrelation of " ~ hat(beta[2]))) +
  labs(colour = "Missingness (%)")

est_Rh + est_AC + est_bias + plot_layout(guides = "collect", ncol = 1)


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
  ylab("Bias") + # change to bias in bar(Q), Q = 19.25?
  #ggtitle(bquote("A) Bias in " ~ bar(R^2) ~ "; " ~ R^2 ~ "= 19.25")) +
  labs(colour = "Missingness (%)")

# R hat for PCA
PCA_Rh <- results %>% ggplot(aes(x = t, y = r.hat.max.pca, color = as.factor(p*100))) +
  geom_hline(yintercept = 1,
             color = "grey",
             lwd = 2) +
  geom_point(size = .75, na.rm = TRUE) +
  geom_line(na.rm = TRUE) +
  xlab("") +
  ylab(bquote(widehat(R)~" ("~theta~")")) +
  #ggtitle(bquote("B) " ~ widehat(R) ~ "of first eigenvalue" ~ Sigma ~ "{" ~ y[obs] ~ "," ~ y[imp] ~ "}")) +
  labs(colour = "Missingness (%)")

# AC for PCA
PCA_AC <- results %>% ggplot(aes(x = t, y = ac.max.pca, color = as.factor(p*100))) +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 2) +
  geom_point(size = .75, na.rm = TRUE) +
  geom_line(na.rm = TRUE) +
  xlab("") +
  ylab(bquote("AC ("~theta~")")) +
  #ggtitle(bquote("C) Autocorrelation of first eigenvalue" ~ Sigma ~ "{" ~ y[obs] ~ "," ~ y[imp] ~ "}")) +
  labs(colour = "Missingness (%)") 

PCA_Rh + PCA_AC + Rsq_bias + plot_layout(guides = "collect", ncol = 1)


####################
### MULTIVARIATE ###
####################

# # Coverage rate regression coefficient
# results %>% ggplot(aes(x = t, y = cov.est.X1, color = as.factor(p*100))) +
#   geom_hline(yintercept = .95,
#              color = "grey",
#              lwd = 2) +
#   geom_point(size = .75) +
#   geom_line() +
#   xlab("Number of iterations") +
#   ylab(expression(paste("Coverage of ", hat(beta)))) +
#   labs(colour = "Missingness (%)")
# 
# # CI length
# results %>% ggplot(aes(x = t, y = CIW.est.X1, color = as.factor(p*100))) +
#   # geom_hline(yintercept = .95,
#   #            color = "grey",
#   #            lwd = 2) +
#   geom_point(size = .75) +
#   geom_line() +
#   xlab("Number of iterations") +
#   ylab(expression(paste("Length 95% CI ", hat(beta)))) +
#   labs(colour = "Missingness (%)")

########################
## NOG IETS MEE DOEN? ##
########################

# ## shows that higher max(ac) means more bias in estimate
# results[-c(1, 51, 101, 151, 201),] %>% ggplot(aes(x = ac.max.beta, y = bias.est.X1, color = as.factor(p*100))) +
#   geom_point(na.rm = TRUE) + 
#   geom_smooth(na.rm = TRUE, method = lm, se=F) +
#   labs(colour = "Missingness (%)")
# 
# results[-c(1, 51, 101, 151, 201),] %>% ggplot(aes(x = r.hat.max.beta, y = bias.est.X1, color = as.factor(p*100))) +
#   geom_point(na.rm = TRUE) + 
#   geom_smooth(na.rm = TRUE, method = lm, se=F) +
#   labs(colour = "Missingness (%)")

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
