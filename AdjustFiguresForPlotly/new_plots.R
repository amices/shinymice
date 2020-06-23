# convert lattice plots to ggplot to use with plotly
# kleuren in mice.theme()  mice:::mdc(1:2) where 1 = obs and 2 = mis
# plot them side by side, obs data on the left and imputed on the right
# potentially add options to choose which imp to show
# check use of x and data in mice vs lattice/ggplot
# ask to import purrr or use lapply
# get bwplot to work with plotly when all is obs!
# use hardhat package from tidymodels
# argument 'interactive = TRUE'
# make a generic plot

# load packages
library(dplyr) #version 0.8.5
library(ggplot2) #version 3.3.0
library(patchwork) #version 1.0.0
library(mice)
library(tidyr)
library(purrr)
library(plotly)


# set default graphing behavior
theme_update(
  # plot.title = element_text(hjust = 0.5),
  # plot.subtitle = element_text(hjust = 0.5),
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  axis.line = element_line(colour = "black")#,
  # legend.key = element_blank(),
  # legend.position = "bottom"
)


# get some test data
mids <- mice(boys, printFlag = FALSE)

vars <-
  mids$data %>% select_if(is.numeric) %>% names %>% set_names(., .) # ook select if voor NA


# stripplot of completed data (for small n)
mice::stripplot(mids)

mice_stripplot <- function(x, dat) {
  dat$imp[[x]] %>% tidyr::pivot_longer(everything(), names_to = ".imp", values_to = x) %>%
    ggplot(.) +
    geom_jitter(
      aes(x = .data$.imp, y = .data[[x]]),
      height = 0.1,
      width = 0.1,
      color = mice:::mdc(2)
    ) +
    geom_jitter(
      data = dat$data,
      mapping = aes(x = as.factor(0), y = .data[[x]]),
      height = 0.1,
      width = 0.1,
      na.rm = TRUE,
      color = mice:::mdc(1)
    ) +
    labs(y = x,
         x = "Imputation (0 = observed data)")
}

mice_stripplot(vars[1], mids) + mice_stripplot(vars[2], mids)

stripplots <- map(vars, ~ mice_stripplot(.x, mids))

ggplotly(stripplots[[2]], tooltip = "y")

# maybe change this to show both the value and the imp number label
# text = paste("Province:", NAME_1, "<br>", "Example III:", example1)

# box and whiskers of completed data (for large n)
mice::bwplot(mids)

mice_bwplot <- function(x, dat) {
  dat$imp[[x]] %>% tidyr::pivot_longer(everything(), names_to = ".imp", values_to = x) %>%
    ggplot(.) +
    stat_boxplot(
      aes(x = .data$.imp, y = .data[[x]]),
      width = 0.25,
      size = 1,
      geom = "errorbar",
      color = mice:::mdc(2)
    ) +
    geom_boxplot(aes(x = .data$.imp, y = .data[[x]]),
                 size = 1,
                 color = mice:::mdc(2)) +
    stat_boxplot(
      data = dat$data,
      mapping = aes(x = as.factor(0), y = .data[[x]]),
      na.rm = TRUE,
      color = mice:::mdc(1),
      geom = "errorbar",
      width = 0.25,
      size = 1
    ) +
    geom_boxplot(
      data = dat$data,
      mapping = aes(x = as.factor(0), y = .data[[x]]),
      na.rm = TRUE,
      size = 1,
      color = mice:::mdc(1)
    ) +
    labs(y = x,
         x = "Imputation (0 = observed data)")
}

mice_bwplot(vars[1], mids)

bwplots <- map(vars, ~ mice_bwplot(.x, mids))

ggplotly(bwplots[[2]])

# density plot of completed data
mice::densityplot(mids)

mice_densityplot <- function(x, dat, thicker = 2) {
  dat$imp[[x]] %>% tidyr::pivot_longer(everything(), names_to = ".imp", values_to = x) %>%
    ggplot(.) +
    geom_density(aes(x = .data[[x]], group = .data$.imp),
                 color = mice:::mdc(2),
                 na.rm = TRUE) +
    geom_density(
      data = dat$data,
      mapping = aes(x = .data[[x]]),
      na.rm = TRUE,
      size = thicker/2,
      color = mice:::mdc(1)
    ) +
    labs(x = x,
         y = "Density")
}

mice_densityplot(vars[2], mids)

densplots <- map(vars, ~ mice_densityplot(.x, mids)) #alpha lichter voor vakjes plotly

ggplotly(densplots[[2]])

# add histogram for discrete data
mice_histogram <- function(x, dat) {
  dat$imp[[x]] %>% tidyr::pivot_longer(everything(), names_to = ".imp", values_to = x) %>%
    ggplot(.) +
    geom_histogram(
      data = dat$data,
      mapping = aes(x = .data[[x]]),
      na.rm = TRUE,
      fill = mice:::mdc(1)    ) +
    geom_histogram(aes(x = .data[[x]], color = .data$.imp), position = "identity",
                   fill = mice:::mdc(2), alpha = 1,
                   na.rm = TRUE) +
    scale_color_manual(values = c(rep(mice:::mdc(2), dat$m))) +
    labs(x = x,
         y = "Count")
}

# # add histogram for discrete data
# mice_histogram <- function(x, dat) {
#   dat$imp[[x]] %>% tidyr::pivot_longer(everything(), names_to = ".imp", values_to = x) %>%
#     ggplot(.) +
#     geom_histogram(
#       data = dat$data,
#       mapping = aes(x = .data[[x]]),
#       na.rm = TRUE,
#       fill = mice:::mdc(1)    ) +
#     geom_histogram(aes(x = .data[[x]]),
#                    fill = mice:::mdc(2),
#                    na.rm = TRUE) + 
#     labs(x = x,
#          y = "Count") +
#     facet_grid(cols = vars(.imp), margins = TRUE) #does not yet work for completely observed variable #remove margins argument to plot without (all)
# }

# # add histogram for discrete data
# mice_histogram <- function(x, dat) {
#   dat$imp[[x]] %>% tidyr::pivot_longer(everything(), names_to = ".imp", values_to = x) %>%
#     ggplot(.) +
#     geom_histogram(
#       data = dat$data,
#       mapping = aes(x = .data[[x]]),
#       na.rm = TRUE,
#       color = mice:::mdc(1),
#       fill = "white", size = 1) +
#     geom_histogram(aes(x = .data[[x]]),
#                    color = mice:::mdc(2), fill = "white", size = 1.5,
#                    na.rm = TRUE) + 
#     labs(x = x,
#          y = "Count") 
# }

mice_histogram(vars[2], mids) 

histplots <- map(vars, ~ mice_histogram(.x, mids)) #alpha lichter voor vakjes plotly

ggplotly(histplots[[2]])

# hist matching density
mice_histogram_dens <- function(x, dat) {
  dat$imp[[x]] %>% tidyr::pivot_longer(everything(), names_to = ".imp", values_to = x) %>%
    ggplot(.) +
    geom_histogram(
      data = dat$data,
      mapping = aes(x = .data[[x]], y = ..density..),
      na.rm = TRUE,
      fill = mice:::mdc(1), alpha = 0.5, color = mice:::mdc(1)    ) +
    geom_histogram(aes(x = .data[[x]], y = ..density.., color = .data$.imp), position = "identity",
                   fill = mice:::mdc(2), alpha = 0.2,
                   na.rm = TRUE) +
    scale_color_manual(values = c(rep(mice:::mdc(2), dat$m))) +
    labs(x = x,
         y = "Density")
}

mice_histogram_dens(vars[2], mids) 
histdensplots <- map(vars, ~ mice_histogram_dens(.x, mids)) #alpha lichter voor vakjes plotly

ggplotly(histdensplots[[2]])


# scatterplot of completed data
mice::xyplot(mids, hgt ~ wgt)


mice_xyplot <- function(x, y, dat) {
  if (x==y){return(NULL)} else {
  # combine imputations and missingness indicator
  dat$data %>% 
      is.na() %>% 
      as.data.frame() %>% 
      mutate(.id = 1:dim(.)[1]) %>% 
      left_join(complete(dat, "long", include = FALSE), by = ".id", suffix = c("r", ""))  %>% 
      filter(.data[[paste0(x, "r")]] == T | .data[[paste0(y, "r")]] == T) %>% 
    ggplot() +
    geom_point(
      data = dat$data,
      aes(x = .data[[x]], y = .data[[y]]),
      color = mice:::mdc(1),
      na.rm = T
    ) +
    geom_point(aes(x = .data[[x]], y = .data[[y]]),
               color = mice:::mdc(2),
               na.rm = TRUE)}
}

mice_xyplot("wgt", "hgt", dat = mids)

xyplots <- map(vars, function(x){map(vars, function(y){mice_xyplot(x = x, y = y, dat = mids)})})
# create default coaand to create them all

ggplotly(xyplots[[3]][[2]])

# test with different data
mice_xyplot("age", "bmi", dat = mice(nhanes, printFlag = F))

# OPTIONAL: influx-outflux plot of incomplete data
mice::fluxplot(boys)
# outflux: hoeveel hangen andere var ervan af?
# influx hoeveel invloed is er van andere vars?
# liefst zo hoog mogelijke outflux

mice_fluxplot <- function(dat){
  # add step to check data type and subset if necessary
  dat %>% flux() %>% mutate(variable = rownames(.)) %>% ggplot() +
    geom_line(data = data.frame(x=0:1, y=1:0), aes(x=x, y=y), linetype = "dashed", color = "gray") + 
    geom_text(aes(x = influx, y = outflux, label=variable)) + 
    scale_x_continuous(limits = c(0,1)) + 
    scale_y_continuous(limits = c(0,1)) + 
    labs(x = "Influx", y = "Outflux")
  }
  
(fluxplot <- mice_fluxplot(mids$data))

ggplotly(fluxplot) #, tooltip = c("text", "influx", "outflux"))
