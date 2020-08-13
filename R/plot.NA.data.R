# plot distributions before imputation

set.seed(123)
data <- mice::boys[sample.int(748, 100),]

v = "phb"

# data %>% mutate(R = !is.na(.data[[v]]), hc = factor(.data[[v]], exclude = NULL)) %>% 
#   ggplot() +
#   geom_point(aes(x = rownames(data), y = .data[[v]], color = R))  +
#   scale_x_discrete(limits = c(levels(.data[[v]])))
plot_NA_margins <- function(data, x){
if (is.numeric(data[[v]])){
NA_level <- min(data[[v]], na.rm = TRUE) - sd(data[[v]], na.rm = TRUE)
data %>% mutate(R = ifelse(is.na(.data[[v]]), NA_level, NA), id = 1:nrow(data)) %>% 
  ggplot() +
  geom_point(aes(x = id, y = .data[[v]]), color = mice:::mdc(1), na.rm = TRUE) +
  geom_point(aes(x = id, y = R), color = mice:::mdc(2), na.rm = TRUE) + 
  scale_y_continuous(expand = expansion(mult = c(0.01, .05))) +
  theme_classic()
} else {
data %>% mutate(R = ifelse(is.na(.data[[v]]), " (NA)", NA), id = 1:nrow(data)) %>% 
  ggplot() +
  geom_point(aes(x = id, y = .data[[v]]), color = mice:::mdc(1), na.rm = TRUE) +
  geom_point(aes(x = id, y = R), color = mice:::mdc(2), na.rm = TRUE) + 
  scale_y_discrete(expand = expansion(mult = c(0.01, .05))) +
  theme_classic()
}
}