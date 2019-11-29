# create table to display results
# requires the packages "dplyr", "xtable
# and the object 'out' created with '1. Excecute'

# load package
library("dplyr")
library("xtable")

# create table with only specific rows
tab <- results %>% 
  as.tbl() %>% 
  .[c(1:10, 15, 20, 25, 50, 100),] %>% 
  xtable(caption = "Simulation and convergence diagnostics over 1000 MCMC simulations.", label = "results", digits = 3)

# adjust table attributes
attr(tab, "align")[1] <- "l"
names(tab) <- c("It.", "Bias", "CI width", "Cov. rate",  "$\\widehat{R}$", "$\\widehat{R}$", "Auto-corr.", "Auto-corr.")

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
  NA.string = "NA",
  include.rownames = FALSE,
  sanitize.text.function = function(x){x}
  # add.to.row = comment,
  # hline.after = c(-1,0)
)

# still try to include this:
# &\multicolumn{4}{c}{Simulation Diagnostics}& & \multicolumn{2}{c}{Convergence Diagn.} \\
# \cline{2-5} \cline{7-8}
# It. & Bias & Emp. SE & CI width & Cov. rate & & $\widehat{R}$ & Auto-corr. \\ 

