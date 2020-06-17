# create figure to display results, displays worst performing estimate in terms of bias by default
# requires the packages "dplyr", "ggplot2" and the object 'results' created with '1.Execute'

# load packages
library(dplyr)
library(ggplot2)
library(patchwork)

# # if necessary, load data
# load("Results/complete.Rdata")

# add diagnostic thresholds
results <- results %>% mutate(crit = qnorm((1 + .95) / 2) / sqrt(t), thresh1.01 = 1.01, thresh1.1 = 1.1, thresh1.2 = 1.2)
results$crit[results$crit>1] <- NA

# define colorblind friendly colors
paint5 <- c('#228833', '#66CCEE', '#CCBB44','#EE6677', '#AA3377')


####################
#### UNIVARIATE ####
####################

# Univariate: mean estimate
mean_bias <- results %>% ggplot(aes(x = t, y = bias.mean.Y, color = as.factor(p*100))) +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 1) +
  geom_point(size = .25, na.rm = TRUE) +
  geom_line(size = .25, na.rm = TRUE) +
  # geom_line(aes(x=t, y=bias.mean.Y.SE, color = as.factor(p*100))) +
  # geom_line(aes(x=t, y=-bias.mean.Y.SE, color = as.factor(p*100))) +
  scale_colour_manual(values=paint5) +
  xlab("") +
  ylab(bquote("Bias in "~bar(Q))) +
  ylab(bquote("Bias (Q = " ~ mu[Y] ~ "= 25.81)")) +
  labs(colour = "Proportion of missing cases (%)") 


# Univariate: convergence chain mean
mean_Rh <- results %>% ggplot(aes(x = t, y = r.hat.max.chain.mean.Y, color = as.factor(p*100))) +
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
  xlab("") +
  ylab(bquote(widehat(R)~" ("~theta~"= chain mean)")) +
  labs(colour = "Proportion of missing cases (%)")  

mean_AC <- results %>% ggplot(aes(x = t, y = ac.max.chain.mean.Y, color = as.factor(p*100))) +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 1) +
  geom_line(aes(x = t, y = crit), color = "grey", linetype = "dashed", size = .25, na.rm = TRUE) + 
  geom_point(size = .25, na.rm = TRUE) +
  geom_line(size = .25, na.rm = TRUE) +
  scale_colour_manual(values=paint5) +
  xlab("") +
  ylab(bquote("AC ("~theta~ " = chain mean)")) +
  labs(colour = "Proportion of missing cases (%)")

mean_bias + mean_Rh + mean_AC + plot_layout(guides = "collect", ncol = 1)

####################
#### UNIVARIATE ####
####################

# Univariate: variance estimate
sd_bias <- results %>% ggplot(aes(x = t, y = bias.sd.Y, color = as.factor(p*100))) +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 1) +
  geom_point(size = .25, na.rm = TRUE) +
  geom_line(size = .25, na.rm = TRUE) +
  scale_colour_manual(values=paint5) +
  xlab("") +
  ylab(bquote("Bias (Q = " ~ sigma[Y] ~ "= 11.32)")) +
  labs(colour = "Proportion of missing cases (%)")

# Univariate: convergence chain variance
var_Rh <- results %>% ggplot(aes(x = t, y = r.hat.max.chain.var.Y, color = as.factor(p*100))) +
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
  xlab("") +
  ylab(bquote(widehat(R)~" ("~theta~"= chain variance)")) +
  labs(colour = "Proportion of missing cases (%)")

var_AC <- results %>% ggplot(aes(x = t, y = ac.max.chain.var.Y, color = as.factor(p*100))) +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 1) +
  geom_line(aes(x = t, y = crit), color = "grey", linetype = "dashed", size = .25, na.rm = TRUE) + 
  geom_point(size = .25, na.rm = TRUE) +
  geom_line(size = .25, na.rm = TRUE) +
  scale_colour_manual(values=paint5) +
  xlab("") +
  ylab(bquote("AC ("~theta~ " = chain variance)")) +
  labs(colour = "Proportion of missing cases (%)")

sd_bias + var_Rh + var_AC + plot_layout(guides = "collect", ncol = 1)

####################
### MULTIVARIATE ###
####################

# Bias in regression coefficient
est_bias <- results %>% ggplot(aes(x = t, y = bias.est.X1, color = as.factor(p*100))) +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 1) +
  geom_point(size = .25, na.rm = TRUE) +
  geom_line(size = .25, na.rm = TRUE) +
  scale_colour_manual(values=paint5) +
  xlab("") +
  ylab(bquote("Bias (Q = " ~ beta[1] ~ "= 2.06)")) +
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
  xlab("") +
  ylab(bquote(widehat(R)~" ("~theta~"= "~hat(Q)~")")) +
  labs(colour = "Proportion of missing cases (%)")

est_AC <- results %>% ggplot(aes(x = t, y = ac.max.beta, color = as.factor(p*100))) +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 1) +
  geom_line(aes(x = t, y = crit), color = "grey", linetype = "dashed", size = .25, na.rm = TRUE) + 
  geom_point(size = .25, na.rm = TRUE) +
  geom_line(size = .25, na.rm = TRUE) +
  scale_colour_manual(values=paint5) +
  xlab("") +
  ylab(bquote("AC ("~theta~ " = "~hat(Q)~")")) +
  labs(colour = "Proportion of missing cases (%)")

est_bias + est_Rh + est_AC + plot_layout(guides = "collect", ncol = 1)


####################
### MULTIVARIATE ###
####################

# R squared
Rsq_bias <- results %>% ggplot(aes(x = t, y = bias.R.sq, color = as.factor(p*100))) +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 1) +
  geom_point(size = .25, na.rm = TRUE) +
  geom_line(size = .25, na.rm = TRUE) +
  scale_colour_manual(values=paint5) +
  xlab("") +
  ylab(bquote("Bias (Q = " ~ r^2 ~ " = 0.19)")) +
  labs(colour = "Proportion of missing cases (%)")

# R hat for PCA
PCA_Rh <- results %>% ggplot(aes(x = t, y = r.hat.max.pca, color = as.factor(p*100))) +
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
  ylab(bquote(widehat(R)~" ("~theta~"= "~lambda[1]~")")) +
  labs(colour = "Proportion of missing cases (%)")

# AC for PCA
PCA_AC <- results %>% ggplot(aes(x = t, y = ac.max.pca, color = as.factor(p*100))) +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 1) +
  geom_line(aes(x = t, y = crit), color = "grey", linetype = "dashed", size = .25, na.rm = TRUE) + 
  geom_point(size = .25, na.rm = TRUE) +
  geom_line(size = .25, na.rm = TRUE) +
  scale_colour_manual(values=paint5) +
  xlab("Number of iterations") +
  ylab(bquote("AC ("~theta~ " = "~lambda[1]~")")) +
  labs(colour = "Proportion of missing cases (%)") 

Rsq_bias + PCA_Rh + PCA_AC + plot_layout(guides = "collect", ncol = 1)


####################
### MULTIVARIATE ###
####################

# Coverage rate regression coefficient
est_cov <- results %>% ggplot(aes(x = t, y = cov.est.X1, color = as.factor(p*100))) +
  geom_hline(yintercept = .95,
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
  xlab("Number of iterations") +
  # ylab("Coverage rate") +
  ylab(bquote("CR (Q = " ~ beta[1] ~ ")")) +
  labs(colour = "Proportion of missing cases (%)")

# CI length
est_ciw <- results %>%  ggplot(aes(x = t, y = CIW.est.X1, color = as.factor(p*100))) +
  # geom_hline(yintercept = .95,
  #            color = "grey",
  #            lwd = 1) +
  geom_point(size = .25, na.rm = TRUE) +
  geom_line(size = .25, na.rm = TRUE) +
  scale_colour_manual(values=paint5) +
  xlab("Number of iterations") +
  ylab(bquote("CIW (Q = " ~ beta[1] ~ ")")) +
  labs(colour = "Proportion of missing cases (%)")



####################
### ALL ESTIMATES ##
####################

mean_X1 <- results %>% ggplot(aes(x = t, y = bias.mean.X1, color = as.factor(p*100))) +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 1) +
  geom_point(size = .25, na.rm = TRUE) +
  geom_line(size = .25, na.rm = TRUE) +
  scale_colour_manual(values=paint5) +
  xlab("") +
  ylab(bquote("Bias (Q = " ~ mu[X[1]] ~ ")")) +
  labs(colour = "Proportion of missing cases (%)") 

mean_X2 <- results %>% ggplot(aes(x = t, y = bias.mean.X2, color = as.factor(p*100))) +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 1) +
  geom_point(size = .25, na.rm = TRUE) +
  geom_line(size = .25, na.rm = TRUE) +
  scale_colour_manual(values=paint5) +
  xlab("") +
  ylab(bquote("Bias (Q = " ~ mu[X[2]] ~ ")")) +
  labs(colour = "Proportion of missing cases (%)") 

mean_X3 <- results %>% ggplot(aes(x = t, y = bias.mean.X3, color = as.factor(p*100))) +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 1) +
  geom_point(size = .25, na.rm = TRUE) +
  geom_line(size = .25, na.rm = TRUE) +
  scale_colour_manual(values=paint5) +
  xlab("Number of iterations") +
  ylab(bquote("Bias (Q = " ~ mu[X[3]] ~ ")")) +
  labs(colour = "Proportion of missing cases (%)") 

sd_X1 <- results %>% ggplot(aes(x = t, y = bias.sd.X1, color = as.factor(p*100))) +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 1) +
  geom_point(size = .25, na.rm = TRUE) +
  geom_line(size = .25, na.rm = TRUE) +
  scale_colour_manual(values=paint5) +
  xlab("") +
  ylab(bquote("Bias (Q = " ~ sigma[X[1]] ~ ")")) +
  labs(colour = "Proportion of missing cases (%)") 

sd_X2 <- results %>% ggplot(aes(x = t, y = bias.sd.X2, color = as.factor(p*100))) +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 1) +
  geom_point(size = .25, na.rm = TRUE) +
  geom_line(size = .25, na.rm = TRUE) +
  scale_colour_manual(values=paint5) +
  xlab("") +
  ylab(bquote("Bias (Q = " ~ sigma[X[3]] ~ ")")) +
  labs(colour = "Proportion of missing cases (%)") 

sd_X3 <- results %>% ggplot(aes(x = t, y = bias.sd.X3, color = as.factor(p*100))) +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 1) +
  geom_point(size = .25, na.rm = TRUE) +
  geom_line(size = .25, na.rm = TRUE) +
  scale_colour_manual(values=paint5) +
  xlab("Number of iterations") +
  ylab(bquote("Bias (Q = " ~ sigma[X[3]] ~ ")")) +
  labs(colour = "Proportion of missing cases (%)") 

est_X2 <- results %>% ggplot(aes(x = t, y = bias.est.X2, color = as.factor(p*100))) +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 1) +
  geom_point(size = .25, na.rm = TRUE) +
  geom_line(size = .25, na.rm = TRUE) +
  scale_colour_manual(values=paint5) +
  xlab("") +
  ylab(bquote("Bias (Q = " ~ beta[2] ~ ")")) +
  labs(colour = "Proportion of missing cases (%)") 

est_X3 <- results %>% ggplot(aes(x = t, y = bias.est.X3, color = as.factor(p*100))) +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 1) +
  geom_point(size = .25, na.rm = TRUE) +
  geom_line(size = .25, na.rm = TRUE) +
  scale_colour_manual(values=paint5) +
  xlab("Number of iterations") +
  ylab(bquote("Bias (Q = " ~ beta[3] ~ ")")) +
  labs(colour = "Proportion of missing cases (%)") 

# Coverage rate regression coefficient
cov_X1 <- results %>% ggplot(aes(x = t, y = cov.est.X1, color = as.factor(p*100))) +
  geom_hline(yintercept = .95,
             color = "grey",
             lwd = 1) +
  geom_point(size = .25, na.rm = TRUE) +
  geom_line(size = .25, na.rm = TRUE) +
  scale_colour_manual(values=paint5) +
  xlab("") +
  # ylab("Coverage rate") +
  ylab(bquote("CR (Q = " ~ beta[1] ~ ")")) +
  labs(colour = "Proportion of missing cases (%)")

cov_X2 <- results %>% ggplot(aes(x = t, y = cov.est.X2, color = as.factor(p*100))) +
  geom_hline(yintercept = 0.95,
             color = "grey",
             lwd = 1) +
  geom_point(size = .25, na.rm = TRUE) +
  geom_line(size = .25, na.rm = TRUE) +
  scale_colour_manual(values=paint5) +
  xlab("") +
  ylab(bquote("CR (Q = " ~ beta[2] ~ ")")) +
  labs(colour = "Proportion of missing cases (%)") 

cov_X3 <- results %>% ggplot(aes(x = t, y = cov.est.X3, color = as.factor(p*100))) +
  geom_hline(yintercept = 0.95,
             color = "grey",
             lwd = 1) +
  geom_point(size = .25, na.rm = TRUE) +
  geom_line(size = .25, na.rm = TRUE) +
  scale_colour_manual(values=paint5) +
  xlab("Number of iterations") +
  ylab(bquote("CR (Q = " ~ beta[3] ~ ")")) +
  labs(colour = "Proportion of missing cases (%)") 

CIW_X2 <- results %>% ggplot(aes(x = t, y = CIX.est.X2, color = as.factor(p*100))) +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 1) +
  geom_point(size = .25, na.rm = TRUE) +
  geom_line(size = .25, na.rm = TRUE) +
  scale_colour_manual(values=paint5) +
  xlab("") +
  ylab(bquote("CIW (Q = " ~ beta[2] ~ ")")) +
  labs(colour = "Proportion of missing cases (%)") 

CIW_X3 <- results %>% ggplot(aes(x = t, y = CIW.est.X3, color = as.factor(p*100))) +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 1) +
  geom_point(size = .25, na.rm = TRUE) +
  geom_line(size = .25, na.rm = TRUE) +
  scale_colour_manual(values=paint5) +
  xlab("") +
  ylab(bquote("CIW (Q = " ~ beta[3] ~ ")")) +
  labs(colour = "Proportion of missing cases (%)") 


####################
### ALL THETAS #####
####################

mean_Rh_X1 <- results %>% ggplot(aes(x = t, y = r.hat.max.chain.mean.X1, color = as.factor(p*100))) +
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
  xlab("") +
  ylab(bquote(widehat(R)~" ("~theta~"= chain mean" ~ X[1] ~")")) +
  labs(colour = "Proportion of missing cases (%)")  

mean_Rh_X2 <- results %>% ggplot(aes(x = t, y = r.hat.max.chain.mean.X2, color = as.factor(p*100))) +
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
  xlab("") +
  ylab(bquote(widehat(R)~" ("~theta~"= chain mean" ~ X[2] ~")")) +
  labs(colour = "Proportion of missing cases (%)")  

mean_Rh_X3 <- results %>% ggplot(aes(x = t, y = r.hat.max.chain.mean.X3, color = as.factor(p*100))) +
  geom_hline(yintercept = 1,
             color = "grey",
             lwd = 1) +
  geom_point(size = .25, na.rm = TRUE) +
  geom_line(size = .25, na.rm = TRUE) +
  geom_line(aes(x=t, y=thresh1.2), color = "grey", linetype = "dashed", size = .25, na.rm = TRUE) +
  geom_line(aes(x=t, y=thresh1.1), color = "grey", linetype = "dashed", size = .25, na.rm = TRUE) +
  geom_line(aes(x=t, y=thresh1.01), color = "grey", linetype = "dashed", size = .25, na.rm = TRUE) +
  scale_y_continuous(limits = c(1,1.53), breaks = c(1,1.1,1.2,1.3,1.4,1.5)) +
  scale_colour_manual(values=paint5) +
  xlab("Number of iterations") +
  ylab(bquote(widehat(R)~" ("~theta~"= chain mean" ~ X[3] ~")")) +
  labs(colour = "Proportion of missing cases (%)")  

var_Rh_X1 <- results %>% ggplot(aes(x = t, y = r.hat.max.chain.var.X1, color = as.factor(p*100))) +
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
  xlab("") +
  ylab(bquote(widehat(R)~" ("~theta~"= chain variance" ~ X[1] ~")")) +
  labs(colour = "Proportion of missing cases (%)")  

var_Rh_X2 <- results %>% ggplot(aes(x = t, y = r.hat.max.chain.var.X2, color = as.factor(p*100))) +
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
  xlab("") +
  ylab(bquote(widehat(R)~" ("~theta~"= chain variance" ~ X[2] ~")")) +
  labs(colour = "Proportion of missing cases (%)")  

var_Rh_X3 <- results %>% ggplot(aes(x = t, y = r.hat.max.chain.var.X3, color = as.factor(p*100))) +
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
  ylab(bquote(widehat(R)~" ("~theta~"= chain variance" ~ X[3] ~")")) +
  labs(colour = "Proportion of missing cases (%)") 


mean_AC_X1 <- results %>% ggplot(aes(x = t, y = ac.max.chain.mean.X1, color = as.factor(p*100))) +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 1) +
  geom_line(aes(x = t, y = crit), color = "grey", linetype = "dashed", size = .25, na.rm = TRUE) + 
  geom_point(size = .25, na.rm = TRUE) +
  geom_line(size = .25, na.rm = TRUE) +
  scale_colour_manual(values=paint5) +
  xlab("") +
  ylab(bquote("AC ("~theta~ " = chain mean" ~ X[1] ~")")) +
  labs(colour = "Proportion of missing cases (%)")

mean_AC_X2 <- results %>% ggplot(aes(x = t, y = ac.max.chain.mean.X2, color = as.factor(p*100))) +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 1) +
  geom_line(aes(x = t, y = crit), color = "grey", linetype = "dashed", size = .25, na.rm = TRUE) + 
  geom_point(size = .25, na.rm = TRUE) +
  geom_line(size = .25, na.rm = TRUE) +
  scale_colour_manual(values=paint5) +
  xlab("") +
  ylab(bquote("AC ("~theta~ " = chain mean" ~ X[2] ~")")) +
  labs(colour = "Proportion of missing cases (%)")

mean_AC_X3 <- results %>% ggplot(aes(x = t, y = ac.max.chain.mean.X3, color = as.factor(p*100))) +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 1) +
  geom_line(aes(x = t, y = crit), color = "grey", linetype = "dashed", size = .25, na.rm = TRUE) + 
  geom_point(size = .25, na.rm = TRUE) +
  geom_line(size = .25, na.rm = TRUE) +
  scale_colour_manual(values=paint5) +
  xlab("Number of iterations") +
  ylab(bquote("AC ("~theta~ " = chain mean" ~ X[3] ~")")) +
  labs(colour = "Proportion of missing cases (%)")

var_AC_X1 <- results %>% ggplot(aes(x = t, y = ac.max.chain.var.X1, color = as.factor(p*100))) +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 1) +
  geom_line(aes(x = t, y = crit), color = "grey", linetype = "dashed", size = .25, na.rm = TRUE) + 
  geom_point(size = .25, na.rm = TRUE) +
  geom_line(size = .25, na.rm = TRUE) +
  scale_colour_manual(values=paint5) +
  xlab("") +
  ylab(bquote("AC ("~theta~ " = chain variance" ~ X[1] ~")")) +
  labs(colour = "Proportion of missing cases (%)")

var_AC_X2 <- results %>% ggplot(aes(x = t, y = ac.max.chain.var.X2, color = as.factor(p*100))) +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 1) +
  geom_line(aes(x = t, y = crit), color = "grey", linetype = "dashed", size = .25, na.rm = TRUE) + 
  geom_point(size = .25, na.rm = TRUE) +
  geom_line(size = .25, na.rm = TRUE) +
  scale_colour_manual(values=paint5) +
  xlab("") +
  ylab(bquote("AC ("~theta~ " = chain variance" ~ X[2] ~")")) +
  labs(colour = "Proportion of missing cases (%)")

var_AC_X3 <- results %>% ggplot(aes(x = t, y = ac.max.chain.var.X3, color = as.factor(p*100))) +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 1) +
  geom_line(aes(x = t, y = crit), color = "grey", linetype = "dashed", size = .25, na.rm = TRUE) + 
  geom_point(size = .25, na.rm = TRUE) +
  geom_line(size = .25, na.rm = TRUE) +
  scale_colour_manual(values=paint5) +
  xlab("Number of iterations") +
  ylab(bquote("AC ("~theta~ " = chain variance" ~ X[3] ~")")) +
  labs(colour = "Proportion of missing cases (%)")



######################################################

# # if wanted, add CI based on MCMC SEs
# geom_errorbar(
#       aes(x = t, ymin = bias.mean.Y.LL, ymax = bias.mean.Y.UL),
#       width = .2,
#       color = "grey"
#     ) +