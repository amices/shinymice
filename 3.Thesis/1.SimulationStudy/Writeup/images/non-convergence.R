# create figure to display pathological non-convergence
# requires the packages 'mice', 'dplyr', 'ggplot2'

# load packages and functions
library(mice)
library(purrr)
library(dplyr)
library(ggplot2)
library(patchwork)
source('./3.Thesis/1.SimulationStudy/Functions/Convergence.R')
source('./3.Thesis/1.SimulationStudy/Functions/AC.R')
source('./3.Thesis/1.SimulationStudy/Functions/AC_supplement.R')
source('./3.Thesis/1.SimulationStudy/Functions/Rhat.R')
source('./3.Thesis/1.SimulationStudy/Functions/Rhat_supplement.R')

#####################
# Pathological non-c.
#####################
meth <- make.method(boys)
meth["bmi"] <- "~I(wgt / (hgt / 100)^2)"
imp.bmi1 <- mice(
  boys,
  meth = meth,
  maxit = 10,
  print = FALSE,
  seed = 60109
)

# plot chain means of wgt (rows are iterations, columns are imputations)
nonc <- plot(imp.bmi1, "wgt")[1]
nonc$layout <- c(1, 1)
nonc[["condlevels"]][[".ms"]] <- c("", "sd")
nonc$ylab <- "Chain mean"
nonc$main <- list(label = "Non-convergence", fontsize=12, fontface = 1)
nonc$x.scales$tick.number <-10
save(nonc, file = "3.Thesis/1.SimulationStudy/Writeup/images/nonc_plot.Rdata")

# diagnostics on chain means of wgt (rows are iterations, columns are imputations)
diagnostics1 <-
  imp.bmi1$chainMean[3, ,] %>% cbind(patho = "Pathological", convergence(., include_acf = TRUE), .)

#####################
# Regular convergence
#####################
pred <- make.predictorMatrix(boys)
pred[c("hgt", "wgt"), "bmi"] <- 0
imp.bmi2 <- mice(
  boys,
  meth = meth,
  pred = pred,
  maxit = 10,
  print = FALSE,
  seed = 60109
)

# plot chain means of wgt (rows are iterations, columns are imputations)
conv <- plot(imp.bmi2, "wgt")[1]
conv$layout <- c(1, 1)
conv[["condlevels"]][[".ms"]] <- c("", "sd")
conv$ylab <- "Chain mean"
conv$main <- list(label = "Typical convergence", fontsize=12, fontface = 1)
conv$x.scales$tick.number <-10
save(conv, file = "3.Thesis/1.SimulationStudy/Writeup/images/conv_plot.Rdata")

# diagnostics on chain means of wgt (rows are iterations, columns are imputations)
diagnostics2 <-
  imp.bmi2$chainMean[3, ,] %>% cbind(patho = "Typical", convergence(., include_acf = TRUE), .)

#####################
# Combine diagnostics
#####################
diagnostics <- rbind(diagnostics1, diagnostics2)
save(diagnostics, file = "3.Thesis/1.SimulationStudy/Writeup/images/diagnostics.Rdata")

# set default for plot layout
# theme_update(
#   plot.title = element_text(hjust = 0.5),
#   plot.subtitle = element_text(hjust = 0.5),
#   panel.border = element_blank(),
#   panel.grid.major = element_blank(),
#   panel.grid.minor = element_blank(),
#   panel.background = element_blank(),
#   axis.line = element_line(colour = "black"),
#   legend.key = element_blank(),
#   legend.position = "bottom"
# )
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
  # adjust text size
  text = element_text(size = 8),
  title = element_text(size = 8)
  # axis.title = element_text(size = 8),
  # axis.text = element_text(size = 7),
  # legend.text = element_text(size = 7)
)

# plot chain means
theta <- ggplot(diagnostics) +
  geom_point(aes(x = iteration, y = `Chain 1`, color = patho),
             size = .75,
             na.rm = TRUE) +
  geom_point(aes(x = iteration, y = `Chain 2`, color = patho),
             size = .75,
             na.rm = TRUE) +
  geom_point(aes(x = iteration, y = `Chain 3`, color = patho),
             size = .75,
             na.rm = TRUE) +
  geom_point(aes(x = iteration, y = `Chain 4`, color = patho),
             size = .75,
             na.rm = TRUE) +
  geom_point(aes(x = iteration, y = `Chain 5`, color = patho),
             size = .75,
             na.rm = TRUE) +
  geom_line(aes(x = iteration, y = `Chain 1`, color = patho), na.rm = TRUE) +
  geom_line(aes(x = iteration, y = `Chain 2`, color = patho), na.rm = TRUE) +
  geom_line(aes(x = iteration, y = `Chain 3`, color = patho), na.rm = TRUE) +
  geom_line(aes(x = iteration, y = `Chain 4`, color = patho), na.rm = TRUE) +
  geom_line(aes(x = iteration, y = `Chain 5`, color = patho), na.rm = TRUE) +
  scale_x_continuous(breaks = 1:10) +
  xlab("") +
  ylab(bquote(theta)) +
  # ggtitle(expression(paste("Chain means (", theta, ")"))) +
  #ggtitle(expression(paste("Scalar summary (", theta, ")"))) +
  theme(legend.position = "")

# plot rhat
rhat <-
  ggplot(diagnostics, aes(x = iteration, y = r.hat.max, color = patho)) +
  geom_hline(yintercept = 1,
             color = "grey",
             lwd = 2) +
  geom_point(size = .75, na.rm = TRUE) +
  geom_line(na.rm = TRUE) +
  scale_x_continuous(breaks = 1:10) +
  xlab("") +
  ylab(bquote(widehat(R)~" of "~theta)) +
  #ggtitle(expression(paste(widehat(R), " of ", theta))) + 
  labs(color = "Convergence")

# plot default ac (with acf())
acf <-
  ggplot(diagnostics, aes(x = iteration, y = acf.max, color = patho)) +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 2) +
  geom_point(size = .75, na.rm = TRUE) +
  geom_line(na.rm = TRUE) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(limits = c(-0.5,1)) +
  xlab("Iteration") +
  ylab(bquote("AC of "~theta~"(default)")) +
  #ggtitle(expression(paste("AC of ", theta, " (default calculation)"))) + 
  theme(legend.position = "")

# plot manual ac
ac <-
  ggplot(diagnostics, aes(x = iteration, y = ac.max, color = patho)) +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 2) +
  geom_point(size = .75, na.rm = TRUE) +
  geom_line(na.rm = TRUE) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(limits = c(-0.5,1)) +
  xlab("Iteration") +
  ylab(bquote("AC of "~theta~"(manual)")) +
  #ggtitle(expression(paste("AC of ", theta, " (manual calculation)"))) + 
  theme(legend.position = "")

# combine
diagnostics_plot <- theta + rhat + acf  + ac + plot_layout(guides = "collect", ncol = 2) + plot_annotation(tag_levels = "A", tag_suffix = ".", title = "Performance of convergence diagnostics", subtitle = bquote(theta ~ "= chain mean in "~ y[imp][",m"])) # add 'ncol=1' for plots under each other

# save
save(diagnostics_plot, file = "3.Thesis/1.SimulationStudy/Writeup/images/diagnostics_plot.Rdata")


# # plot rhat
# old_rhat <-
#   ggplot(diagnostics, aes(x = iteration, y = r.hat, color = patho)) +
#   geom_hline(yintercept = 1,
#              color = "grey",
#              lwd = 2) +
#   geom_point(size = .75, na.rm = TRUE) +
#   geom_line(na.rm = TRUE) +
#   scale_x_continuous(breaks = 1:10) +
#   scale_y_continuous(limits = c(0.98,1.86)) +
#   xlab("") +
#   ylab(bquote(widehat(R)~" of "~theta)) +
#   #ggtitle(expression(paste(widehat(R), " of ", theta))) + 
#   labs(color = "Convergence")
# 
# # plot rhat
# new_rhat <-
#   ggplot(diagnostics, aes(x = iteration, y = r.hat.max, color = patho)) +
#   geom_hline(yintercept = 1,
#              color = "grey",
#              lwd = 2) +
#   geom_point(size = .75, na.rm = TRUE) +
#   geom_line(na.rm = TRUE) +
#   scale_x_continuous(breaks = 1:10) +
#   scale_y_continuous(limits = c(0.98,1.86)) +
#   xlab("") +
#   ylab(bquote(widehat(R)~" of "~theta)) +
#   #ggtitle(expression(paste(widehat(R), " of ", theta))) + 
#   labs(color = "Convergence")
