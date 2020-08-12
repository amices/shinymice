# md pattern plot, see https://stackoverflow.com/questions/32220446/how-to-plot-a-matrix-in-a-fixed-grid-pattern-in-r
# The columns A, B and C are either 0 (missing) or 1 (observed). The first column provides the frequency of each pattern. The last column lists the number of missing entries per pattern. The bottom row provides the number of missing entries per variable, and the total number of missing cells.
# library(mice)
# library(tidyverse)
plot_md_pattern <- function(data) {
  # parse inputs
  md <- mice::md.pattern(data, plot = FALSE)
  vars <- colnames(md)[-ncol(md)]
  mice_cols <- c("1" = mice:::mdc(1), "0" = mice:::mdc(2))
  second_x <- sec_axis(~.+0, breaks = seq_len(nrow(md)-1), labels = rev(as.character(md[-nrow(md),ncol(md)])), name = "Number of missing values per pattern")
  second_y <- as.character(md[nrow(md), -ncol(md)])
  # define function to add second discrete axis, see https://github.com/tidyverse/ggplot2/issues/3171
  # guide_axis_label_trans <- function(label_trans = identity, ...) {
  #   axis_guide <- guide_axis(...)
  #   axis_guide$label_trans <- rlang::as_function(label_trans)
  #   class(axis_guide) <- c("guide_axis_trans", class(axis_guide))
  #   axis_guide
  # }
  # 
  # guide_train.guide_axis_trans <- function(x, ...) {
  #   trained <- NextMethod()
  #   trained$key$.label <- x$label_trans(trained$key$.label)
  #   trained
  # }
  
  # make dataset with var names
  var_lab <- data.frame(
    name = vars, 
    label = vars,
    value = 1)

  # process and plot
  md[1:nrow(md) - 1, 1:ncol(md) - 1] %>%
    cbind(id = 1:nrow(.)) %>%
    as.data.frame() %>%
    tidyr::pivot_longer(cols = -id) %>%
    ggplot2::ggplot(ggplot2::aes(
      x = name,
      y = rev(id),
      fill = factor(value)
    )) +
    ggplot2::geom_tile(colour = "black") +
    ggplot2::scale_x_discrete(limits = vars, expand = c(0, 0), position = "bottom", labels = second_y) +
    ggplot2::scale_y_continuous(breaks = seq_len(nrow(md)-1), labels = rev(rownames(md)[-nrow(md)]), sec.axis = second_x, expand = expansion(mult = c(0, .05))) +
    ggplot2::theme_classic() +
    ggplot2::scale_fill_manual(values = mice_cols) +
    labs(
      x = paste0("Number of missing cells per variable (total = ",  md[nrow(md), ncol(md)], ")"), 
      y = "Number of rows with this pattern",
      title = " ") +
    geom_text(aes(x = name, y = Inf, label = label), data = var_lab, vjust = 1.5) +
    #guides(x.sec = guide_axis_label_trans()) +
    theme(legend.position = "none", 
          axis.title.y.right = element_text(vjust=2),
          axis.line.y = element_blank())
    #geom_text(aes(name, y = -Inf, label = label), data = data.frame(name = vars, label = as.character(md[nrow(md), -ncol(md)])), vjust = -0.5) 
}

#colnames(md) <- c(colnames(md)[-dim(md)[2]], "nmis")
#md <- md %>% cbind(freq = as.numeric(row.names(.)))

boys %>% plot_md_pattern()

boys %>% md.pattern()
