# convert lattice plots to ggplot to use with plotly

# load packages
library(dplyr) #version 0.8.5
library(ggplot2) #version 3.3.0
library(patchwork) #version 1.0.0
library(mice)

# test the colors
obs <- grDevices::hcl(240, 100, 40, 0.7)
mis <- grDevices::hcl(0, 100, 40, 0.7) 
dat <- data.frame(xx=1:10, yy=1:10)
dat %>% ggplot(aes(x=xx, y=yy))+geom_point(color=mis)

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

# generate original mice plots
# box and whiskers of completed data (for large n)
mice::bwplot(mids)

# stripplot of completed data (for small n)
mice::stripplot(mids)

# density plot of completed data
mice::densityplot(mids)

# scatterplot of completed data
mice::xyplot(mids, hgt~wgt)

# # OPTIONAL: influx-outflux plot of incomplete data
# mice::fluxplot(boys)

# NEW STRIPPLOT
mids$imp$hgt %>% 
  setNames(., c("M1", "M2", "M3", "M4", "M5")) %>% 
  ggplot() +
  geom_jitter(aes(x=1, y=M1), height = 0, width = 0.1, color = cols[2]) +
  geom_jitter(aes(x=2, y=M2), height = 0, width = 0.1, color = cols[2]) +
  geom_jitter(aes(x=3, y=M3), height = 0, width = 0.1, color = cols[2]) +
  geom_jitter(aes(x=4, y=M4), height = 0, width = 0.1, color = cols[2]) +
  geom_jitter(aes(x=5, y=M5), height = 0, width = 0.1, color = cols[2]) +
  geom_jitter(data = mids$data, mapping = aes(x=0, y=hgt), height = 0, width = 0.1, na.rm = TRUE, color = cols[1]) +
  scale_x_continuous(breaks = c(0, 1:5)) +
  xlab("Imputation number (0 = observed data)") + 
  ylab("Height")

# NEW BWPLOT
mids$imp$hgt %>% 
  setNames(., c("M1", "M2", "M3", "M4", "M5")) %>% 
  ggplot() +
  geom_boxplot(aes(x=1, y=M1), color = cols[2]) +
  geom_boxplot(aes(x=2, y=M2), color = cols[2]) +
  geom_boxplot(aes(x=3, y=M3), color = cols[2]) +
  geom_boxplot(aes(x=4, y=M4), color = cols[2]) +
  geom_boxplot(aes(x=5, y=M5), color = cols[2]) +
  geom_boxplot(data = mids$data, mapping = aes(x=0, y=hgt), na.rm = TRUE, color = cols[1]) +
  scale_x_continuous(breaks = c(0, 1:5)) +
  xlab("Imputation number (0 = observed data)") + 
  ylab("Height")

# NEW DENSITYPLOT
mids$imp$hgt %>% 
  setNames(., c("M1", "M2", "M3", "M4", "M5")) %>% 
  ggplot() +
  geom_density(aes(x=M1), color = cols[2]) +
  geom_density(aes(x=M2), color = cols[2]) +
  geom_density(aes(x=M3), color = cols[2]) +
  geom_density(aes(x=M4), color = cols[2]) +
  geom_density(aes(x=M5), color = cols[2]) +
  geom_density(data = mids$data, aes(x=hgt), na.rm = TRUE, color = cols[1], size = 1) +
  xlab("Height") + 
  ylab("Density")

# NEW XYPLOT
mids %>% 
  complete("long") %>% 
  ggplot() +
  geom_point(aes(x=wgt, y=hgt), color = cols[2]) +
  geom_point(data = mids$data, aes(x=wgt, y=hgt), na.rm = TRUE, color = cols[1])

# better to add imputed values to this instead?
mids$data %>% 
  ggplot() +
  geom_point(aes(x=wgt, y=hgt), color = cols[1], na.rm=T) 

# or make it more generic by adding the original data to the completed object?
cd <- mids %>% 
  complete("long", include = TRUE) #%>% cbind((is.na(mids$data)))
r <- as.data.frame(is.na(mids$data))

cd %>%  
  ggplot() +
  geom_point(aes(x=wgt, y=hgt), color = cols[2], na.rm = TRUE) 
