#' Helper function to create trace plot
#'
#' @param data A \code{mids} object, typically created by \code{mice()} or
#'\code{mice.mids()}.
#' @param x A string specifying the variable to plot.
#'
#' @return A list with two ggplot objects
traceplot_function <- function(data) {
  # preprocess data
  d <- data$chainMean %>%
    dplyr::na_if(., "NaN") %>%
    as.data.frame() %>%
    dplyr::mutate(var = row.names(.), theta = "Chain means") %>%
    rbind(
      .,
      data$chainVar %>%
        sqrt() %>%
        as.data.frame(.) %>%
        dplyr::mutate(var = row.names(.), theta = "Chain standard deviations")
    ) %>%
    tidyr::pivot_longer(-c(var, theta)) %>%
    cbind(.it = 1:data$iteration,
          .imp = rep(1:data$m, each = data$iteration))
  
  # plot data
  # p1 <- d %>% ggplot2::ggplot(ggplot2::aes(
  #   x = .it,
  #   y = value,
  #   group = .imp,
  #   color = as.factor(.imp)
  # )) +
  #   ggplot2::geom_line(na.rm = T) +
  #   ggplot2::labs(color = "Imputation",
  #                 x = "Iteration",
  #                 y = "") +
  #   ggplot2::theme_classic() +
  #   ggplot2::facet_wrap(
  #     ~ var + theta,
  #     scales = "free",
  #     ncol = 2)
  
  plotfun <- function(dt, v) {
    if(all(!is.na(dt$value[d$var==v]))) {
      geom_list <- NULL
      #p <- ggplot2::ggplot(data.frame(x = NA, y = NA), ggplot2::aes(x,y)) + ggplot2::geom_text(aes(label = c("Test"))) + ggplot2::theme_classic()
    }
    if(all(is.na(dt$value[d$var==v]))) {
      geom_list <- list(geom_text(aes(x = median(dt$.imp), y=0, label = "No \n imputations \n to show"), color = "grey")) #position = position_dodge(5)
    }
    # if(all(is.na(dt$value[d$var==v]))) {
    #   geom_list <- list(geom_text(aes(x = 1, y=1, label = "test")))
    # }
    
    p <- dt %>% dplyr::filter(var==v) %>% 
    ggplot2::ggplot(ggplot2::aes(
    x = .it,
    y = value,
    group = .imp,
    color = as.factor(.imp)
  )) +
    ggplot2::geom_line(na.rm = T) +
    ggplot2::labs(color = "Imputation",
                  x = "Iteration",
                  y = paste("Variable:", v)) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position="bottom") +
    ggplot2::facet_wrap(
      ~ theta,
      scales = "free",
      ncol = 1) +
      geom_list
    #}
    
    return(p)
  } 
  
  ps <- purrr::map(names(data$data) %>% setNames(., names(data$data)), function(x){plotfun(dt = d, v = x)})
  
  # ggsave('testplot.png', height = 22, width = 8.5)
  
  # # to remove a var, use:
  # p1$data <- p1$data %>% filter(vars != "age")
  
  # p2 <- d %>% ggplot2::ggplot(ggplot2::aes(
  #   x = .it,
  #   y = value,
  #   group = .imp,
  #   color = as.factor(.imp)
  # )) +
  #   ggplot2::geom_line(na.rm = T) +
  #   ggplot2::labs(
  #     color = "Imputation",
  #     x = "Iteration",
  #     y = "",
  #     title = "Chain standard deviations"
  #   ) +
  #   ggplot2::theme_classic() +
  #   facet_wrap( ~ vars, scales = "free")
  
  # Add text when no imps are shown
  #ps$age + list(geom_text(aes(x = 1, y=1, label = "test")))
  
  return(ps)
}



#mids <- mice::mice(boys, printFlag = FALSE)
#x <- "wgt"
#gg.traceplot(mids, "age")
# figure out how to show a message when variable is not imputed at all
#all(is.na(c$data$`mean(age)`))
#names(mids$data) %>% setNames(names(mids$data)) %>% purrr::map(function(x) {gg.traceplot(data = mids, x=x)})
