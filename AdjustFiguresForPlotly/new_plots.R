# convert lattice plots to ggplot to use with plotly
# kleuren in mice.theme()  mice:::mdc(1:2) where 1 = obs and 2 = mis
# plot them side by side, obs data on the left and imputed on the right
# potentially add options to choose which imp to show


# load packages
library(dplyr) #version 0.8.5
library(ggplot2) #version 3.3.0
library(patchwork) #version 1.0.0
library(mice)
library(tidyr)

# # test the colors
# obs <- grDevices::hcl(240, 100, 40, 0.7)
# mis <- grDevices::hcl(0, 100, 40, 0.7)
# dat <- data.frame(xx=1:10, yy=1:10)
# dat %>% ggplot(aes(x=xx, y=yy))+geom_jitter(color=mis)+geom_jitter(color=obs)
#
# # save the colors
# cols <- c(obs,mis)

# set default graphing behavior
theme_update(
  plot.title = element_text(hjust = 0.5),
  plot.subtitle = element_text(hjust = 0.5),
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  axis.line = element_line(colour = "black"),
  legend.key = element_blank(),
  legend.position = "bottom"
)


# get some test data
mids <- mice(boys, printFlag = FALSE)

vars <-
  mids$data %>% select_if(is.numeric) %>% names %>% set_names(., .) %>% .[-1]


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
         x = "Imputation")
}

mice_stripplot(vars[1], mids)

stripplots <- map(vars, ~ mice_stripplot(.x, mids))


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
         x = "Imputation")
}

mice_bwplot(vars[1], mids)

bwplots <- map(vars, ~ mice_bwplot(.x, mids))


# density plot of completed data
mice::densityplot(mids)

mice_densityplot <- function(x, dat) {
  dat$imp[[x]] %>% tidyr::pivot_longer(everything(), names_to = ".imp", values_to = x) %>%
    ggplot(.) +
    geom_density(aes(x = .data[[x]], group = .data$.imp),
                 color = mice:::mdc(2)) +
    geom_density(
      data = dat$data,
      mapping = aes(x = .data[[x]]),
      na.rm = TRUE,
      size = 1,
      color = mice:::mdc(1)
    ) +
    labs(x = x,
         y = "Density")
}

mice_densityplot(vars[1], mids)

densplots <- map(vars, ~ mice_densityplot(.x, mids))


# scatterplot of completed data
mice::xyplot(mids, hgt ~ wgt)

# this works!!
dat <-
  mids$data %>% is.na() %>% as.data.frame() %>% mutate(.id = 1:dim(.)[1]) %>% left_join(complete(mids, "long", include = FALSE),
                                                                                        by = ".id",
                                                                                        suffix = c("r", ""))
x = "hgt"
y = "wgt"

mice_densityplot <- function(x, y, dat) {
  dat %>% filter(.data[[paste0(x, "r")]] == T | .data[[paste0(y, "r")]] == T) %>% 
    ggplot() +
    geom_point(
      data = mids$data ,
      aes(x = .data[[x]], y = .data[[y]]),
      color = mice:::mdc(1),
      na.rm = T
    ) +
    geom_point(aes(x = .data[[x]], y = .data[[y]]),
               color = mice:::mdc(2),
               na.rm = TRUE) +
    labs(x = x,
         y = y)
}

mice_densityplot(vars[1], vars[2], dat)

densplots <- map(vars, function(x){map(vars, function(y){mice_densityplot(x = x, y = y, dat = dat)})})

# 
# dat %>% filter(.data[[paste0(x, "r")]] == T | .data[[paste0(y, "r")]] == T) %>% ggplot() +
#   geom_point(
#     data = mids$data ,
#     aes(x = .data[[x]], y = .data[[y]]),
#     color = mice:::mdc(1),
#     na.rm = T
#   ) +
#   geom_point(aes(x = .data[[x]], y = .data[[y]]),
#              color = mice:::mdc(2),
#              na.rm = TRUE)
# 
# 
# paste0(vars[1],"r")

# plot separately
# ggplot() +
#   geom_point(data = mids$data , aes(x=wgt, y=hgt), color = mice:::mdc(1), na.rm=T)
# cd %>%
#   ggplot() +
#   geom_point(aes(x=wgt, y=hgt), color = mice:::mdc(2), na.rm = TRUE)



# # OPTIONAL: influx-outflux plot of incomplete data
# mice::fluxplot(boys)
# # outflux: hoeveel hangen andere var ervan af?
# # influx hoeveel invloed is er van andere vars?
# # liefst zo hoog mogelijke outflux
