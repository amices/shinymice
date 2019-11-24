# create table to display results
# requires the packages "dplyr", "xtable
# and the object 'out' created with '1. Excecute'

# load package
library("dplyr")
library("xtable")

# transform object to tibble
out <- out %>% as.tbl()

# use xtable
tab <- xtable(out, caption = "Simulation and convergence diagnostics over 1000 MCMC simulations.", label = "results", digits = 3)

# adjust table
attr(tab, "align")[1] <- "l"
tab <- tab[c(1:10, 15, 20, 25, 50, 100),]
names(tab) <- c("Bias", "Emp. SE", "CI width", "Cov. rate",  "$R$", "$R$", "Auto-corr.", "Auto-corr.")

# # add footnote
# comment <- list(pos = list(0), command = NULL)
# comment$pos[[1]] <- c(nrow(tab))
# comment$command <- c(paste("\\hline",
#                            "{\\footnotesize{Note: * signifies that no convergence diagnostic could be computed..}\n", sep = ""))

# print table
print.xtable(
  tab,
  type = "latex",
  caption.placement = "top",
  NA.string = "NA" 
  # add.to.row = comment,
  # hline.after = c(-1,0)
)

# still try to include this:
# &\multicolumn{4}{c}{Simulation Diagnostics}& & \multicolumn{2}{c}{Convergence Diagn.} \\
# \cline{2-5} \cline{7-8}
# It. & Bias & Emp. SE & CI width & Cov. rate & & $\widehat{R}$ & Auto-corr. \\ 

