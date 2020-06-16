# convert lattice plots to ggplot to use with plotly
# kleuren in mice.theme()  mice:::mdc(1:2) where 1 = obs and 2 = mis
# plot them side by side, obs data on the left and imputed on the right
# potentially add options to choose which imp to show


# load packages
library(dplyr) #version 0.8.5
library(ggplot2) #version 3.3.0
library(patchwork) #version 1.0.0
library(mice)

# test the colors
obs <- grDevices::hcl(240, 100, 40, 0.7)
mis <- grDevices::hcl(0, 100, 40, 0.7) 
dat <- data.frame(xx=1:10, yy=1:10)
dat %>% ggplot(aes(x=xx, y=yy))+geom_jitter(color=mis)+geom_jitter(color=obs)

# save the colors
cols <- c(obs,mis)

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


# stripplot of completed data (for small n)
mice::stripplot(mids)

# # NEW STRIPPLOT
# mids$imp$hgt %>% 
#   setNames(., c("M1", "M2", "M3", "M4", "M5")) %>% 
#   ggplot() +
#   geom_jitter(aes(x=1, y=M1), height = 0.1, width = 0.1, color = mice:::mdc(2)) +
#   geom_jitter(aes(x=2, y=M2), height = 0.1, width = 0.1, color = mice:::mdc(2)) +
#   geom_jitter(aes(x=3, y=M3), height = 0.1, width = 0.1, color = mice:::mdc(2)) +
#   geom_jitter(aes(x=4, y=M4), height = 0.1, width = 0.1, color = mice:::mdc(2)) +
#   geom_jitter(aes(x=5, y=M5), height = 0.1, width = 0.1, color = mice:::mdc(2)) +
#   geom_jitter(data = mids$data, mapping = aes(x=0, y=hgt), height = 0.1, width = 0.1, na.rm = TRUE, color = mice:::mdc(1)) +
#   scale_x_continuous(breaks = c(0, 1:5)) +
#   xlab("Imputation number (0 = observed data)") + 
#   ylab("Height")

# NEW STRIPPLOT
mids$imp$hgt %>% tidyr::gather(., ".imp") %>% 
  ggplot() +
  geom_jitter(aes(x=.imp, y=value), height = 0.1, width = 0.1, color = mice:::mdc(2)) +
  geom_jitter(data = mids$data, mapping = aes(x=as.factor(0), y=hgt), height = 0.1, width = 0.1, na.rm = TRUE, color = mice:::mdc(1)) +
  xlab("Imputation number (0 = observed data)") + 
  ylab("Height")

# box and whiskers of completed data (for large n)
mice::bwplot(mids)

# NEW BWPLOT
mids$imp$hgt %>% tidyr::gather(., ".imp") %>% 
  ggplot() +
  stat_boxplot(aes(x=.imp, y=value), color = mice:::mdc(2), geom = "errorbar", width = 0.25, size = 1) +  
  stat_boxplot(data = mids$data, mapping = aes(x=as.factor(0), y=hgt), na.rm = TRUE, color = mice:::mdc(1), geom = "errorbar", width = 0.25, size = 1) +  
  geom_boxplot(aes(x=.imp, y=value), color = mice:::mdc(2), size = 1) +
  geom_boxplot(data = mids$data, mapping = aes(x=as.factor(0), y=hgt), na.rm = TRUE, color = mice:::mdc(1), size =1) +
  xlab("Imputation number (0 = observed data)") + 
  ylab("Height")

# density plot of completed data
mice::densityplot(mids)

# NEW DENSITYPLOT
mids$imp$hgt %>% tidyr::gather(., ".imp") %>% 
  ggplot() +
  geom_density(aes(x=value, group = .imp), color = mice:::mdc(2)) +
  geom_density(data = mids$data, aes(x=hgt), na.rm = TRUE, color = mice:::mdc(1), size = 1) +
  xlab("Height") + 
  ylab("Density")

# scatterplot of completed data
mice::xyplot(mids, hgt~wgt)

# NEW XYPLOT
# mids %>% 
#   complete("long") %>% 
#   ggplot() +
#   geom_point(data = mids$data, aes(x=wgt, y=hgt), na.rm = TRUE, color = mice:::mdc(1))+
#   geom_point(aes(x=wgt, y=hgt), color = mice:::mdc(2)) 
  # better to add imputed values to this instead?

# or make it more generic by adding the original data to the completed object?
# cd <- mids %>% 
#   complete("long", include = TRUE) 
r <- as.data.frame(is.na(mids$data)) %>% mutate(.id = 1:dim(.)[1])# dit als filter gebruiken!! als of hgt of wgt missing is plotten
cdf <- mids %>% 
  complete("long", include = FALSE) 

# this works!!
left_join(cdf, r, by = ".id", suffix=c("", "r")) %>% filter(hgtr == T | wgt == T) %>% ggplot() +
  geom_point(data = mids$data , aes(x=wgt, y=hgt), color = mice:::mdc(1), na.rm=T) +
  geom_point(aes(x=wgt, y=hgt), color = mice:::mdc(2), na.rm = TRUE) 


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
