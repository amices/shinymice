# create figure to display pathological non-convergence
# requires the packages 'mice', 'dplyr', 'ggplot2'

# load packages and functions
library(mice)
library(purrr)
library(dplyr)
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
save(nonc, file = "3.Thesis/1.SimulationStudy/Writeup/images/nonc_plot.Rdata")

# diagnostics on chain means of wgt (rows are iterations, columns are imputations)
diagnostics1 <-
  imp.bmi1$chainMean[3, , ] %>% convergence(., include_acf = TRUE) %>% cbind(patho = "Typical", .)

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
save(conv, file = "3.Thesis/1.SimulationStudy/Writeup/images/conv_plot.Rdata")

# diagnostics on chain means of wgt (rows are iterations, columns are imputations)
diagnostics2 <-
  imp.bmi2$chainMean[3, , ] %>% convergence(., include_acf = TRUE) %>% cbind(patho = "Pathological", .)

#####################
# Combine diagnostics
#####################
diagnostics <- rbind(diagnostics1, diagnostics2)

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

rhat <-
  ggplot(diagnostics, aes(x = iteration, y = max.r.hat, color = patho)) +
  geom_hline(yintercept = 1,
             color = "grey",
             lwd = 2) +
  geom_point(size = .75, na.rm = TRUE) +
  geom_smooth(na.rm = TRUE) +
  scale_x_continuous(breaks = 1:10) +
  xlab("Iteration") +
  ylab(expression(paste(widehat(R)))) +
  labs(colour = "Convergence")

acf <-
  ggplot(diagnostics, aes(x = iteration, y = ac.max, color = patho)) +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 2) +
  geom_point(size = .75, na.rm = TRUE) +
  geom_smooth(aes(x = iteration, y = acf.max, color = patho),
            na.rm = TRUE,
            linetype = "dotted") +
  geom_point(aes(x = iteration, y = acf.max, color = patho),
             size = .75,
             na.rm = TRUE) +
  geom_smooth(na.rm = TRUE) +
  scale_x_continuous(breaks = 1:10) +
  xlab("Iteration") +
  ylab("AC") +
  theme(legend.position = "")

rhat + acf + plot_layout(guides = "collect") # add 'ncol=1' for plots under each other



############## With lm instead of lines
rhat <-
  ggplot(diagnostics, aes(x = iteration, y = max.r.hat, color = patho)) +
  geom_hline(yintercept = 1,
             color = "grey",
             lwd = 2) +
  geom_point(size = .75, na.rm = TRUE) +
  geom_smooth(na.rm = TRUE, se=F, method = "lm") +
  scale_x_continuous(breaks = 1:10) +
  xlab("Iteration") +
  ylab(expression(paste(widehat(R)))) +
  labs(colour = "Convergence")

acf <-
  ggplot(diagnostics, aes(x = iteration, y = ac.max, color = patho)) +
  geom_hline(yintercept = 0,
             color = "grey",
             lwd = 2) +
  geom_point(size = .75, na.rm = TRUE) +
  geom_smooth(aes(x = iteration, y = acf.max, color = patho),
              na.rm = TRUE,
              linetype = "dotted", se=F, method = "lm") +
  geom_point(aes(x = iteration, y = acf.max, color = patho),
             size = .75,
             na.rm = TRUE) +
  geom_smooth(na.rm = TRUE, se=F, method = "lm") +
  scale_x_continuous(breaks = 1:10) +
  xlab("Iteration") +
  ylab("AC") +
  theme(legend.position = "")

rhat + acf + plot_layout(guides = "collect") # add 'ncol=1' for plots under each other

