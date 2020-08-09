# md pattern plot, see https://stackoverflow.com/questions/32220446/how-to-plot-a-matrix-in-a-fixed-grid-pattern-in-r
# The columns A, B and C are either 0 (missing) or 1 (observed). The first column provides the frequency of each pattern. The last column lists the number of missing entries per pattern. The bottom row provides the number of missing entries per variable, and the total number of missing cells. 
# library(mice)
# library(tidyverse)
plot_md_pattern <- function(data){
  md <- md.pattern(boys, plot = FALSE)
  vars <- colnames(md)[-ncol(md)]
  mice_cols = c("1" = mice:::mdc(1), "0" = mice:::mdc(2))
  md[1:nrow(md)-1,1:ncol(md)-1] %>%
    cbind(id = 1:nrow(.)) %>% 
    as.data.frame() %>%
    tidyr::pivot_longer(cols = -id) %>% 
    ggplot(aes(x = name, y = rev(id), fill = factor(value))) + 
    geom_raster() +
    scale_x_discrete(limits = vars, expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    theme_classic() +
    scale_fill_manual(values = mice_cols) +
    theme(legend.position = "bottom")
}

#colnames(md) <- c(colnames(md)[-dim(md)[2]], "nmis") 
#md <- md %>% cbind(freq = as.numeric(row.names(.)))

