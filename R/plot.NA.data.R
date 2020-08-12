# plot distributions before imputation

set.seed(123)
data <- mice::boys[sample.int(748, 100),]

v = "hc"

data %>% mutate(R = is.na(.data[[v]]), hc = factor(.data[[v]], exclude = NULL)) %>% 
  ggplot() +
  geom_point(aes(x = rownames(data), y = .data[[v]], color = R))  +
  scale_x_discrete(limits = c(levels(.data[[v]])))
