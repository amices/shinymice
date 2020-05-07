# Create figures to display pathological non-convergence
# and non-convergence diagnostics figures
# Requires the packages 'ggplot2', 'dplyr'/'magrittr', and 'patchwork'
# and the files with imputations and non-convergence diagnostics

# load packages and functions
library(mice)
library(dplyr)
library(ggplot2)
library(patchwork)

# # if necessary, load example non-convergence data and diagnostics
# load("3.Thesis/Results/example_nonconv.Rdata")
# load("3.Thesis/Results/example_conv.Rdata")
# load("3.Thesis/Results/example_diagnostics.Rdata")

# define colorblind friendly colors
paint2 <- c('#66CCEE', '#EE6677')

#####################
# Pathological non-c.
#####################

# create traceplot for chain means of wgt (rows are iterations, columns are imputations)
non_plot <- plot(nonconv, "wgt", type="l")[1]
non_plot$layout <- c(1, 1)
non_plot[["condlevels"]][[".ms"]] <- c("", "sd")
non_plot$ylab <- "Chain mean"
non_plot$x.scales$tick.number <-10
non_plot$main <- list(label = "Non-convergence", fontsize=12, fontface = 1)


#####################
# Regular convergence
#####################

# create traceplot for chain means of wgt (rows are iterations, columns are imputations)
con_plot <- plot(conv, "wgt", type="l")[1]
con_plot$layout <- c(1, 1)
con_plot[["condlevels"]][[".ms"]] <- c("", "sd")
con_plot$ylab <- "Chain mean"
con_plot$x.scales$tick.number <-10
con_plot$main <- list(label = "Typical convergence", fontsize=12, fontface = 1)


#####################
# Plot non-conv. diag
#####################

# plot chain means
theta <- ggplot(diagnostics) +
  geom_point(aes(x = iteration, y = `Chain 1`, color = patho),
             size = .25,
             na.rm = TRUE) +
  geom_point(aes(x = iteration, y = `Chain 2`, color = patho),
             size = .25,
             na.rm = TRUE) +
  geom_point(aes(x = iteration, y = `Chain 3`, color = patho),
             size = .25,
             na.rm = TRUE) +
  geom_point(aes(x = iteration, y = `Chain 4`, color = patho),
             size = .25,
             na.rm = TRUE) +
  geom_point(aes(x = iteration, y = `Chain 5`, color = patho),
             size = .25,
             na.rm = TRUE) +
  geom_line(aes(x = iteration, y = `Chain 1`, color = patho), size = .25, na.rm = TRUE) +
  geom_line(aes(x = iteration, y = `Chain 2`, color = patho), size = .25, na.rm = TRUE) +
  geom_line(aes(x = iteration, y = `Chain 3`, color = patho), size = .25, na.rm = TRUE) +
  geom_line(aes(x = iteration, y = `Chain 4`, color = patho), size = .25, na.rm = TRUE) +
  geom_line(aes(x = iteration, y = `Chain 5`, color = patho), size = .25, na.rm = TRUE) +
  scale_colour_manual(values=paint2) +
  scale_x_continuous(breaks = 1:10) +
  xlab("Iteration") +
  ylab(bquote(theta)) +
  labs(color = "") + 
  theme(legend.position = "top")


# plot manual ac
ac_both <-
  ggplot(diagnostics) +
  # geom_line(aes(x = iteration, y = signif), color = "gray") + 
  geom_point(aes(x = iteration, y = acf.max, color = patho), size = .25, na.rm = TRUE) +
  geom_line(aes(x = iteration, y = acf.max, color = patho, linetype = "Default"), size = .25, na.rm = TRUE) +
  geom_point(aes(x = iteration, y = ac.max, color = patho), size = .25, na.rm = TRUE) +
  geom_line(aes(x = iteration, y = ac.max, color = patho, linetype = "Manual"), size = .25, na.rm = TRUE) +
  scale_colour_manual(values=paint2) +
  scale_linetype_manual("",values=c("Default"=2,"Manual"=1))+
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(limits = c(-0.5,1)) +
  xlab("Iteration") +
  ylab(bquote("AC of "~theta)) +
  theme(legend.position = c(0.75, 0.25))+
  guides(color = FALSE, linetype = guide_legend(order = 1)) 


# plot traditional rhat
old_rhat <-
  ggplot(diagnostics, aes(x = iteration, y = r.hat, color = patho)) +
  # geom_hline(yintercept = 1,
  #            color = "grey",
  #            lwd = 2) +
  geom_point(size = .25, na.rm = TRUE) +
  geom_line(size = .25, na.rm = TRUE) +
  scale_colour_manual(values=paint2) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(limits = c(0.98,1.86)) +
  xlab("Iteration") +
  ylab(bquote(widehat(R)~" of "~theta)) +
  theme(legend.position = "")


# plot rhat Vehtari et al. 2019
new_rhat <-
  ggplot(diagnostics, aes(x = iteration, y = r.hat.max, color = patho)) +
  # geom_hline(yintercept = 1,
  #            color = "grey",
  #            lwd = 2) +
  geom_point(size = .25, na.rm = TRUE) +
  geom_line(size = .25, na.rm = TRUE) +
  scale_colour_manual(values=paint2) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(limits = c(0.98,1.86)) +
  xlab("Iteration") +
  ylab(bquote(widehat(R)~" of "~theta)) +
  theme(legend.position = "")
