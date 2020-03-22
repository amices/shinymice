# create figure to display results
# requires the packages "dplyr", "ggplot2"
# and the object 'results' created with '1.Excecute'

# load package
library("dplyr")
library("ggplot2")
library("ggpubr")

# if necessary, load results
# load("C:/Users/User/Desktop/shinyMice/2.ResearchReport/1.SimulationStudy/Results/full_results.Rdata")
# results <- dat
# results_with_CI <- dat #results

# # create plot for R hat
# R_plot <- results_with_CI %>% .[-1,] %>%
#   ggplot(aes(x = T, y = R.var.X1, color = "Chain variances")) +
#   geom_line(linetype = "dashed", size = 1) +
#   geom_line(aes(x = T, y = R.var.X2, color = "Chain variances"),
#             linetype = "dashed",
#             size = 1) +
#   geom_line(aes(x = T, y = R.var.X3, color = "Chain variances"),
#             linetype = "dashed",
#             size = 1) +
#   geom_line(aes(x = T, y = R.var.Y, color = "Chain variances"),
#             linetype = "dashed",
#             size = 1) +
#   geom_line(aes(x = T, y = R.mean.X1, color = "Chain means")) +
#   geom_line(aes(x = T, y = R.mean.X2, color = "Chain means")) +
#   geom_line(aes(x = T, y = R.mean.X3, color = "Chain means")) +
#   geom_line(aes(x = T, y = R.mean.Y, color = "Chain means")) +
#   # # add error bar when `results_withSEs` is used
#   # geom_errorbar(
#   #   aes(ymin = R.mean.X1_LL, ymax = R.mean.X1_UL),
#   #   width = .2,
#   #   position = position_dodge(0.05)
#   # ) +
#   xlab("") +
#   ylab(expression(paste(widehat(R)))) +
#   scale_x_continuous(breaks = seq(0, 100, by = 10)) +
#   scale_y_continuous(breaks = seq(.8, 2.2, by = .2)) +
#   scale_color_manual(name = "Legend", labels = c("Chain means", "Chain variances"), values = c("black", "grey"), guide = "legend") +
#   guides(color = guide_legend(override.aes = list(linetype = c(1, 8)))) +
#   theme_bw() +
#   theme(
#     panel.border = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     axis.line = element_line(colour = "black"),
#     legend.position =  c(.95, .95),
#     legend.justification = c("right", "top"),
#     legend.box.just = "right",
#     legend.margin = margin(6, 6, 6, 6)
#   )
# 
# # create plot for autocorrelations
# AC_plot <- results_with_CI %>% .[-1,] %>%
#   ggplot(aes(x = T, y = AC.var.X1, color = "Chain variances")) +
#   geom_line(linetype = "dashed", size = 1) +
#   geom_line(
#     aes(x = T, y = AC.var.X2, color = "Chain variances"),
#     linetype = "dashed",
#     size = 1
#   ) +
#   geom_line(
#     aes(x = T, y = AC.var.X3, color = "Chain variances"),
#     linetype = "dashed",
#     size = 1
#   ) +
#   geom_line(aes(x = T, y = AC.var.Y, color = "Chain variances"),
#             linetype = "dashed",
#             size = 1) +
#   geom_line(aes(x = T, y = AC.mean.X1, color = "Chain means")) +
#   geom_line(aes(x = T, y = AC.mean.X2, color = "Chain means")) +
#   geom_line(aes(x = T, y = AC.mean.X3, color = "Chain means")) +
#   geom_line(aes(x = T, y = AC.mean.Y, color = "Chain means")) +
#   # # add error bar when `results_withSEs` is used
#   # geom_errorbar(
#   #   aes(ymin = AC.mean.X1_LL, ymax = AC.mean.X1_UL),
#   #   width = .2#,
#   #   #position = position_dodge(0.05)
#   # ) +
#   xlab("Number of iterations") +
#   ylab("Auto-correlation") +
#   scale_x_continuous(breaks = seq(0, 100, by = 10)) +
#   scale_y_continuous(breaks = seq(-.8, .6, by = .2)) +
#   scale_color_manual(name = "Legend", labels = c("Chain means", "Chain variances"), values = c("black", "grey"), guide = "legend") +
#   theme_bw() +
#   theme(
#     panel.border = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     axis.line = element_line(colour = "black") ,
#     legend.position = ""
#   )
# 
# # combine convergence plots into figure
# figure1 <-
#   ggarrange(
#     R_plot,
#     AC_plot,
#     labels = c("A", "B"),
#     ncol = 1,
#     nrow = 2,
#     align = "v"
#   )
# # print figure
# figure1

# create plot for bias
# bias_plot <- ggplot(results_with_CI, aes(x = T, y = bias)) +
#   geom_point() +
#   geom_smooth(
#     method = "loess",
#     se = FALSE,
#     colour = "black",
#     size = 0.5,
#     linetype = "dashed"
#   ) +
#   geom_errorbar(
#     aes(ymin = bias_LL, ymax = bias_UL),
#     width = .2,
#     colour = "grey"
#   ) +
#   xlab("") +
#   ylab("Bias") +
#   scale_x_continuous(breaks = seq(0, 100, by = 10)) +
#   theme_bw() +
#   theme(
#     panel.border = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     axis.line = element_line(colour = "black")
#   )
# 
# 
# # create plot for confidence interval width
# CIW_plot <- ggplot(results_with_CI, aes(x = T, y = CIW)) +
#   geom_line() +
#   geom_smooth(
#     method = "loess",
#     se = FALSE,
#     colour = "black",
#     size = 0.5,
#     linetype = "dashed"
#   ) +
#   # # add error bar when `results_withSEs` is used
#   # geom_errorbar(
#   #   aes(ymin = CIW_LL, ymax = CIW_UL),
#   #   width = .2,
#   #   colour = "grey"
#   # ) +
#   xlab("") +
#   ylab("CI width") +
#   scale_x_continuous(breaks = seq(0, 100, by = 10)) +
#   theme_bw() +
#   theme(
#     panel.border = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     axis.line = element_line(colour = "black")
#   )
# 
# # create plot for coverage rate
# cov_plot <- ggplot(results_with_CI, aes(x = T, y = cov * 100)) +
#   geom_line() +
#   geom_smooth(
#     method = "loess",
#     se = FALSE,
#     colour = "black",
#     size = 0.5,
#     linetype = "dashed"
#   ) +
#   # regular CI is useless here! No error bars added.
#   xlab("Number of iterations (T)") +
#   ylab("Coverage (%)") +
#   scale_x_continuous(breaks = seq(0, 100, by = 10)) +
#   theme_bw() +
#   theme(
#     panel.border = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     axis.line = element_line(colour = "black")
#   )
# 
# # combine plots of simulation diagnostics into figure
# figure2 <-
#   ggarrange(
#     bias_plot,
#     CIW_plot,
#     cov_plot,
#     labels = c("A", "B", "C"),
#     ncol = 1,
#     nrow = 3,
#     align = "hv"
#   )
# # print figure
# figure2
# 
# # combine all plots together for presentation
# figure3 <-
#   ggarrange(
#     R_plot,
#     AC_plot,
#     bias_plot,
#     CIW_plot,
#     cov_plot,
#     labels = c("A", "B", "C", "D", "E"),
#     ncol = 1,
#     nrow = 5,
#     align = "v"
#   )
# # print figure
# figure3

###################

bias_means_plot <- ggplot(results_with_CI, aes(x = T, y = bias.mean.X1, color = "X1")) +
  geom_point() +
  geom_smooth(se = F) +
  # geom_errorbar(
  #   aes(ymin = bias_X1_LL, ymax = bias_X1_UL),
  #   width = .2,
  #   colour = "grey"
  # ) +
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
  ggtitle("Univariate means") + 
  #scale_x_continuous(breaks = seq(0, 100, by = 10)) +
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

means_plot <-
  ggarrange(
    bias_means_plot,
    R_means_plot,
    AC_means_plot,
    ncol = 1,
    nrow = 3,
    align = "v"
  )

# print figure
means_plot

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
  ggtitle("Univariate variances") + 
  #scale_x_continuous(breaks = seq(0, 100, by = 10)) +
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
    axis.line = element_line(colour = "black") #,
    #legend.position = ""
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

sds_plot <-
  ggarrange(
    bias_sds_plot,
    R_sds_plot,
    AC_sds_plot,
    ncol = 1,
    nrow = 3,
    align = "v"
  )

# print figure
sds_plot

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
    axis.line = element_line(colour = "black") #,
    #legend.position = ""
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
  # geom_errorbar(
  #       aes(x = T, ymin = bias.est.X1.LL, ymax = bias.est.X1.UL),
  #       width = .2,
  #       color = "grey"
  #     ) +
  xlab("") +
  ylab("Bias in regression coefficient") +
  ggtitle("Multivariate estimates") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black") #,
    #legend.position = ""
  )


ests_plot <-
  ggarrange(
    bias_ests_plot,
    #R_sq_plot,
    PCA_plot,
    ncol = 1,
    nrow = 2,
    align = "v"
  )

# print figure
ests_plot

####################################################################

# prediction

R_sq_plot <- results_with_CI %>% #.[-1,] %>% 
  ggplot() +
  geom_hline(yintercept = 0.2062, color = "grey", lwd = 2) +
  geom_point(aes(x = T, y = R.sq, color = "R^2")) +
  geom_line(aes(x = T, y = R.sq, color = "R^2")) +
  geom_smooth(aes(x = T, y = R.sq, color = "R^2"), se = FALSE) +
  xlab("") +
  ylab("Explained variance") +
  #ggtitle("Multivariate estimates") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black") #,
    #legend.position = ""
  )

RMSE_plot <- results_with_CI %>% #.[-1,] %>% 
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
  #ggtitle("Multivariate estimates") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black") #,
    #legend.position = ""
  )

MAE_plot <- results_with_CI %>% #.[-1,] %>% 
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
  #ggtitle("Multivariate estimates") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black") #,
    #legend.position = ""
  )

err_plot <- results_with_CI %>% #.[-1,] %>% 
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
  #ggtitle("Multivariate estimates") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black") #,
    #legend.position = ""
  )

pred_plot <-
  ggarrange(
    R_sq_plot,
    RMSE_plot,
    MAE_plot,
    err_plot,
    ncol = 1,
    nrow = 4,
    align = "v"
  )

# print figure
pred_plot


