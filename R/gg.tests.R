# # file with tests to evaluate what to change
# # see: http://adv-r.had.co.nz/Functions.html,
# # https://bookdown.org/rdpeng/RProgDA/expressions-environments.html
# # and https://ggplot2-book.org/programming.html
# 
# # load packages (remove later, check dependencies), gebruik devtools::document()
# # library(dplyr)
# # library(ggplot2)
# # library(mice)
# # library(tidyr)
# 
# # load functions (remove later)
# # setwd("C:/Users/4216318/Documents/mice/R")
# # source("gg.densityplot.R")
# # source("gg.histogram.R")
# # source("gg.barplot.R")
# # source("gg.boxplot.R")
# # source("gg.stripplot.R")
# # source("gg.xyplot.R")
# 
# # create some test data
# mids <- mice(boys, printFlag = F, maxit = 20)
# 
# 
# 
# # assign values to test function
# # m = 5
# # x = "wgt"
# # y = "hgt"
# # dat = mids
# 
# # examples/tests
# gg.mids(x = "hgt",
#         dat = mids,
#         geom = "bwplot")
# gg.mids(x = "hgt",
#         dat = mids,
#         geom = "stripplot")
# gg.mids(x = "hgt",
#         dat = mids,
#         geom = "densityplot") #blauw weer dikker maken als argument?
# gg.mids(x = "hgt",
#         dat = mids,
#         geom = "histogram") #check stacking in density version
# gg.mids(x = "reg",
#         dat = mids,
#         geom = "histogram")
# gg.mids(mids,
#         x = "phb",
#         geom = "histogram",
#         scaling = T) #one plot per imp as option, with side by side as default upto 5-10 imp, and introduce a jitter if they are overlapping
# # figure 1.2, 1.3, 1.7 --> extend to multiple imps
# 
# gg.mids(x = "wgt",
#         y = "hgt",
#         dat = mids,
#         geom = "xyplot") #add option to show the imps side by side --> conditioned on .imp with |
# 
# gg.mids(x = ~ hgt,
#         dat = mids)
# 
# gg.mids(x = hgt ~ wgt,
#         dat = mids,
#         geom = "xyplot")
# 
# gg.mids(
#   x = "hgt",
#   dat = mids,
#   geom = "bwplot",
#   interactive = TRUE
# )
# 
# gg.mids(
#   x = "hgt",
#   dat = mids,
#   geom = "histogram",
#   binwidth = 50
# )
# 
# gg.mids(
#   x = "hgt",
#   dat = mids,
#   geom = "histogram",
#   scaling = TRUE
# )
# 
# gg.mids(x = "age", dat = mids, geom = "bwplot") #add empty columns for imps back in, add option to skip completely obs var
# # also, leave option to plot completely obs variable, because the data may have been imputed by someone else using mean imp! (see What Interpretable Machine Learning Can Tell Us About Missing Values
# # by Rich Caruana Jul 17, 2020 ) around min 27:00
# 
# gg.mids(x = "reg", dat = mids, geom = "bwplot")
# 
# gg.mids(x = "reg",
#         y = "age",
#         dat = mids,
#         geom = "xyplot") #plot NA values next to y axis? only for xyplots! add jitter for categorical vars
# 
# # test for all variables
# mids %>%
#   .$data %>%
#   names(.) %>%
#   setNames(., .) %>%
#   purrr::map(function(x) {
#     gg.mids(x, dat = mids) #plot sd, not chain var
#   })
# 
# gg.mids(x = "wgt", dat = mids) # if x=NULL, then plot all traceplots (except completely obs??)
# 
# #show comparison between histogram with skyline, solid and alpha bars
