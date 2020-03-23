# create figure to display results
# requires the packages "dplyr", "ggplot2"
# and the object 'results' created with '1.Excecute'

# load package
library("dplyr")
library("ggplot2")
library("patchwork")

# # if necessary, load results
# load("C:/Users/User/Desktop/shinyMice/3.Thesis/1.SimulationStudy/Results/results.Rdata")
# results_with_CI <- dat


bias_means_plot <- ggplot(results_with_CI, aes(x = T, y = bias.mean.X1, color = "X1")) +
  geom_point() +
  geom_smooth(se = F) +
  geom_point(aes(x = T, y = bias.mean.Y, color = "Y")) +
  geom_smooth(aes(x = T, y = bias.mean.Y, color = "Y"), se = F) +
  geom_point(aes(x = T, y = bias.mean.X2, color = "X2")) +
  geom_smooth(aes(x = T, y = bias.mean.X2, color = "X2"), se = F) +
  geom_point(aes(x = T, y = bias.mean.X3, color = "X3")) +
  geom_smooth(aes(x = T, y = bias.mean.X3, color = "X3"), se = F) +
  # geom_errorbar(
  #       aes(x = T, ymin = bias.mean.Y.LL, ymax = bias.mean.Y.UL),
  #       width = .2,
  #       color = "grey"
  #     ) +
  xlab("") +
  ylab("Bias") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )

AC_means_plot <- results_with_CI %>% .[-1,] %>%
  ggplot() +
  geom_point(aes(x = T, y = AC.mean.X1, color = "X1")) +
  geom_point(aes(x = T, y = AC.mean.Y, color = "Y")) +
  geom_point(aes(x = T, y = AC.mean.X2, color = "X2")) +
  geom_point(aes(x = T, y = AC.mean.X3, color = "X3")) +
  geom_smooth(aes(x = T, y = AC.mean.X1, color = "X1"), se = FALSE) +
  geom_smooth(aes(x = T, y = AC.mean.Y, color = "Y"), se = FALSE) +
  geom_smooth(aes(x = T, y = AC.mean.X2, color = "X2"), se = FALSE) +
  geom_smooth(aes(x = T, y = AC.mean.X3, color = "X3"), se = FALSE) +
  xlab("Number of iterations") +
  ylab("Auto-correlation") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black") #,
    #legend.position = ""
  )

R_means_plot <- results_with_CI %>% .[-1,] %>%
  ggplot() +
  geom_point(aes(x = T, y = R.mean.X1, color = "X1")) +
  geom_point(aes(x = T, y = R.mean.Y, color = "Y")) +
  geom_point(aes(x = T, y = R.mean.X2, color = "X2")) +
  geom_point(aes(x = T, y = R.mean.X3, color = "X3")) +
  geom_smooth(aes(x = T, y = R.mean.X1, color = "X1"), se = FALSE) +
  geom_smooth(aes(x = T, y = R.mean.Y, color = "Y"), se = FALSE) +
  geom_smooth(aes(x = T, y = R.mean.X2, color = "X2"), se = FALSE) +
  geom_smooth(aes(x = T, y = R.mean.X3, color = "X3"), se = FALSE) +
  xlab("") +
  ylab(expression(paste(widehat(R)))) +  #ylab(expression(paste(widehat(R), " (in chain means)"))) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black") #,
    #legend.position = ""
  )

bias_means_plot + R_means_plot + AC_means_plot + plot_layout(nrow = 3)


#########################

bias_sds_plot <- ggplot(results_with_CI, aes(x = T, y = bias.sd.X1, color = "X1")) +
  geom_point() +
  geom_smooth(se = F) +
  # geom_errorbar(
  #   aes(ymin = bias_X1_LL, ymax = bias_X1_UL),
  #   width = .2,
  #   colour = "grey"
  # ) +
  geom_point(aes(x = T, y = bias.sd.Y, color = "Y")) +
  geom_smooth(aes(x = T, y = bias.sd.Y, color = "Y"), se = F) +
  geom_point(aes(x = T, y = bias.sd.X2, color = "X2")) +
  geom_smooth(aes(x = T, y = bias.sd.X2, color = "X2"), se = F) +
  geom_point(aes(x = T, y = bias.sd.X3, color = "X3")) +
  geom_smooth(aes(x = T, y = bias.sd.X3, color = "X3"), se = F) +
  xlab("") +
  ylab("Bias") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )

AC_sds_plot <- results_with_CI %>% .[-1,] %>%
  ggplot() +
  geom_point(aes(x = T, y = AC.var.X1, color = "X1")) +
  geom_point(aes(x = T, y = AC.var.Y, color = "Y")) +
  geom_point(aes(x = T, y = AC.var.X2, color = "X2")) +
  geom_point(aes(x = T, y = AC.var.X3, color = "X3")) +
  geom_smooth(aes(x = T, y = AC.var.X1, color = "X1"), se = FALSE) +
  geom_smooth(aes(x = T, y = AC.var.Y, color = "Y"), se = FALSE) +
  geom_smooth(aes(x = T, y = AC.var.X2, color = "X2"), se = FALSE) +
  geom_smooth(aes(x = T, y = AC.var.X3, color = "X3"), se = FALSE) +
  xlab("Number of iterations") +
  ylab("Auto-correlation") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black") 
  )

R_sds_plot <- results_with_CI %>% .[-1,] %>%
  ggplot() +
  geom_point(aes(x = T, y = R.var.X1, color = "X1")) +
  geom_point(aes(x = T, y = R.var.Y, color = "Y")) +
  geom_point(aes(x = T, y = R.var.X2, color = "X2")) +
  geom_point(aes(x = T, y = R.var.X3, color = "X3")) +
  geom_smooth(aes(x = T, y = R.var.X1, color = "X1"), se = FALSE) +
  geom_smooth(aes(x = T, y = R.var.Y, color = "Y"), se = FALSE) +
  geom_smooth(aes(x = T, y = R.var.X2, color = "X2"), se = FALSE) +
  geom_smooth(aes(x = T, y = R.var.X3, color = "X3"), se = FALSE) +
  xlab("") +
  #ylab(expression(paste(widehat(R), " (in chain means)"))) +
  ylab(expression(paste(widehat(R)))) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black") #,
    #legend.position = ""
  )

bias_sds_plot + R_sds_plot + AC_sds_plot + plot_layout(nrow = 3)

################################

PCA_plot <- results_with_CI %>% #.[-1,] %>%
  ggplot() +
  geom_point(aes(x = T, y = pca.1, color = "m1")) +
  geom_point(aes(x = T, y = pca.2, color = "m2")) +
  geom_point(aes(x = T, y = pca.3, color = "m3")) +
  geom_point(aes(x = T, y = pca.4, color = "m4")) +
  geom_point(aes(x = T, y = pca.5, color = "m5")) +
  # geom_line(aes(x = T, y = pca.1, color = "m1")) +
  # geom_line(aes(x = T, y = pca.2, color = "m2")) +
  # geom_line(aes(x = T, y = pca.3, color = "m3")) +
  # geom_line(aes(x = T, y = pca.4, color = "m4")) +
  # geom_line(aes(x = T, y = pca.5, color = "m5")) +
  geom_smooth(aes(x = T, y = pca.1, color = "m1"), se = FALSE) +
  geom_smooth(aes(x = T, y = pca.2, color = "m2"), se = FALSE) +
  geom_smooth(aes(x = T, y = pca.3, color = "m3"), se = FALSE) +
  geom_smooth(aes(x = T, y = pca.4, color = "m4"), se = FALSE) +
  geom_smooth(aes(x = T, y = pca.5, color = "m5"), se = FALSE) +
  xlab("Number of iterations") +
  ylab("First PCA component") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black") 
  )

bias_ests_plot <- results_with_CI %>% #.[-1,] %>% 
  ggplot() +
  #geom_point(aes(x = T, y = bias.est..Intercept., color = "Intercept")) +
  geom_point(aes(x = T, y = bias.est.X1, color = "X1")) +
  geom_point(aes(x = T, y = bias.est.X2, color = "X2")) +
  geom_point(aes(x = T, y = bias.est.X3, color = "X3")) +
  #geom_smooth(aes(x = T, y = bias.est..Intercept., color = "Intercept"), se = FALSE) +
  geom_smooth(aes(x = T, y = bias.est.X1, color = "X1"), se = FALSE) +
  geom_smooth(aes(x = T, y = bias.est.X2, color = "X2"), se = FALSE) +
  geom_smooth(aes(x = T, y = bias.est.X3, color = "X3"), se = FALSE) +
  xlab("") +
  ylab("Bias in regression coefficient") +
  ggtitle("Multivariate estimates") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black") 
  )

bias_ests_plot + PCA_plot + plot_layout(nrow = 2)

####################################################################

# prediction

R_sq_plot <- results_with_CI %>% #.[-1,] %>% 
  ggplot() +
  geom_hline(yintercept = 100 * 0.2062, color = "grey", lwd = 2) +
  geom_point(aes(x = T, y = 100 * R.sq, color = "R^2")) +
  geom_line(aes(x = T, y = 100 * R.sq, color = "R^2")) +
  #geom_smooth(aes(x = T, y = 100 * R.sq, color = "R^2"), se = FALSE) +
  xlab("") +
  ylab("Explained variance (%)") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black") 
  )

RMSE_plot <- results_with_CI %>% 
  ggplot() +
  geom_point(aes(x = T, y = RMSE.1, color = "m1")) +
  geom_point(aes(x = T, y = RMSE.2, color = "m2")) +
  geom_point(aes(x = T, y = RMSE.3, color = "m3")) +
  geom_point(aes(x = T, y = RMSE.4, color = "m4")) +
  geom_point(aes(x = T, y = RMSE.5, color = "m5")) +
  geom_smooth(aes(x = T, y = RMSE.1, color = "m1"), se = FALSE) +
  geom_smooth(aes(x = T, y = RMSE.2, color = "m2"), se = FALSE) +
  geom_smooth(aes(x = T, y = RMSE.3, color = "m3"), se = FALSE) +
  geom_smooth(aes(x = T, y = RMSE.4, color = "m4"), se = FALSE) +
  geom_smooth(aes(x = T, y = RMSE.5, color = "m5"), se = FALSE) +
  xlab("") +
  ylab("Root mean squared error") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black") 
  )

MAE_plot <- results_with_CI %>% 
  ggplot() +
  geom_point(aes(x = T, y = MAE.1, color = "m1")) +
  geom_point(aes(x = T, y = MAE.2, color = "m2")) +
  geom_point(aes(x = T, y = MAE.3, color = "m3")) +
  geom_point(aes(x = T, y = MAE.4, color = "m4")) +
  geom_point(aes(x = T, y = MAE.5, color = "m5")) +
  geom_smooth(aes(x = T, y = MAE.1, color = "m1"), se = FALSE) +
  geom_smooth(aes(x = T, y = MAE.2, color = "m2"), se = FALSE) +
  geom_smooth(aes(x = T, y = MAE.3, color = "m3"), se = FALSE) +
  geom_smooth(aes(x = T, y = MAE.4, color = "m4"), se = FALSE) +
  geom_smooth(aes(x = T, y = MAE.5, color = "m5"), se = FALSE) +
  geom_errorbar(
        aes(x = T, ymin = MAE.1.LL, ymax = MAE.1.UL),
        width = .2,
        color = "grey"
      ) +
  xlab("") +
  ylab("Mean absolute error") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black") 
  )

err_plot <- results_with_CI %>% 
  ggplot() +
  geom_point(aes(x = T, y = error.var.1, color = "m1")) +
  geom_point(aes(x = T, y = error.var.2, color = "m2")) +
  geom_point(aes(x = T, y = error.var.3, color = "m3")) +
  geom_point(aes(x = T, y = error.var.4, color = "m4")) +
  geom_point(aes(x = T, y = error.var.5, color = "m5")) +
  geom_smooth(aes(x = T, y = error.var.1, color = "m1"), se = FALSE) +
  geom_smooth(aes(x = T, y = error.var.2, color = "m2"), se = FALSE) +
  geom_smooth(aes(x = T, y = error.var.3, color = "m3"), se = FALSE) +
  geom_smooth(aes(x = T, y = error.var.4, color = "m4"), se = FALSE) +
  geom_smooth(aes(x = T, y = error.var.5, color = "m5"), se = FALSE) +
  xlab("") +
  ylab("Error variance") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black") 
  )

R_sq_plot + RMSE_plot + MAE_plot + err_plot + plot_layout(nrow = 4)

