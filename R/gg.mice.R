#' Plot observed and imputed data
#' 
#' @aliases ggmice
#'
#' @param data A \code{mids} object, typically created by \code{mice()} or
#'\code{mice.mids()}.
#' @param x A string or formula that selects the data to be plotted.
#' @param y A string that selects an optional second variable to be plotted.
#' @param geom A string that selects the plot type.
#' @param interactive A logical indicator that specifies whether the plot should have \code{plotly} functionalities.
#' @param ... Optional additional ggplot2 arguments.
#'
#' @return A \code{ggplot} object.
#' @export
gg.mids <-
  function(data,
           x = names(data$data)[1],
           y = NULL,
           geom = c("bwplot", "densityplot", "fluxplot", "histogram", "stripplot", "traceplot", "xyplot"),
           interactive = FALSE,
           ...) {
    if (!is.mids(data)) stop("Argument 'data' must be a 'mids' object")
    
    # parse variable inputs
    if (is.null(y) &
        inherits(x, "formula") & length(all.vars(x)) == 1) {
      x <- all.vars(x)[1]
    }
    if (is.null(y) &
        inherits(x, "formula") & length(all.vars(x)) == 2) {
      y <- all.vars(x)[1]
      x <- all.vars(x)[2]
    }
    if (all(!is.na(data$data[[x]])) & is.null(y)) warning("Variable 'x' is completely observed, so no imputations can be shown")
    
    # parse plotting inputs
    if (missing(geom)) {
      geom <- "traceplot"
    }
    if(geom!="xyplot"){
      y <- NULL
    }
    if (geom == "stripplot") {
      geom_mice <- stripplot_function
      if (length(data$data[[x]])>500) warning("Variable 'x' has a lot of observations, `geom = 'bwplot'` might be better suited") #message may be better?
    }
    if (geom == "bwplot") {
      geom_mice <- boxplot_function
      if (is.factor(data$data[[x]])) warning("Variable 'x' is of class 'factor', `geom = 'stripplot'` might be better suited")
    }
    if (geom == "densityplot") {
      geom_mice <- densityplot_function
    }
    if (geom == "histogram" &
        is.numeric(data$data[[x]]) == TRUE) {
      geom_mice <- histogram_function
    }
    if (geom == "histogram" &
        is.numeric(data$data[[x]]) == FALSE) {
      geom_mice <- barplot_function
    }
    if (geom == "xyplot") {
      if(is.null(y)) warning("Please provide a second variable to plot.")
      geom_mice <- xyplot_function
    }
    if (geom == "traceplot"){
      p <- traceplot_function(data)
      if (interactive) {
        plist <- p %>% purrr::map(., ~plotly::ggplotly(.))
        return(plist)
      } else
        return(p)
    }
    if (geom == "fluxplot"){
      p <- fluxplot_function(data)
      if (interactive) {
        return(plotly::ggplotly(p))
      } else
        return(p)
    }
      
    # update theme (remove later, add to `mice.theme()`)
    # theme_mice <- ggplot2::theme_set(ggplot2::theme_classic)# + 
    #   ggplot2::theme(
    #     legend.position = "bottom" #to add, replace "" by e.g. "bottom"
    #   ) 
    #   ggplot2::scale_color_manual(values = mice::mdc(1:2)),
    # ggplot2::scale_fill_manual(values = mice::mdc(1:2))
    # mice_colors <- c("Observed" = mice::mdc(1), "Imputed" = mice::mdc(2))
    
    # preprocess imputations
    imps <- data$imp[[x]] %>%
      dplyr::mutate(.id = row.names(.)) %>% 
      tidyr::pivot_longer(
        cols = 1:data$m,
        names_to = ".imp",
        values_to = x,
        names_transform = list(.imp = as.numeric)
      )
    
    # preprocess incomplete data
    d <-
      data$data %>%
      dplyr::mutate(.id = rownames(.)) %>%
      .[c(x, y, ".id")]
    
    # combine incomplete and imputed data
    if (is.null(y)) {
      cd <- d %>%
        dplyr::mutate(.imp = 0) %>% 
        dplyr::bind_rows(., imps) %>%
        dplyr::mutate(imputed = ifelse(.imp > 0, 'Imputed','Observed'))
    }
    
    # optional additional steps for second variable
    if (!is.null(y)) {
      # preprocess imputations
      imps2 <- data$imp[[y]] %>%
        dplyr::mutate(.id = row.names(.)) %>%
        tidyr::pivot_longer(
          cols = 1:data$m,
          names_to = ".imp",
          values_to = y,
          names_transform = list(.imp = as.numeric)
          )  %>%
        dplyr::full_join(imps, by = c(".id", ".imp")) %>%
        dplyr::left_join(d, by = ".id", suffix = c("", ".obs"))
      
      # add observed values
      imps2[[x]][is.na(imps2[[x]])] <-
        imps2[[paste(x, "obs", sep = ".")]][is.na(imps2[[x]])]
      imps2[[y]][is.na(imps2[[y]])] <-
        imps2[[paste(y, "obs", sep = ".")]][is.na(imps2[[y]])]
      
      # combine incomplete and imputed data
      cd <- imps2 %>%
        .[c(".id", ".imp", x, y)] %>%
        dplyr::bind_rows(d %>% mutate(.imp = 0), .) %>% 
        dplyr::mutate(imputed = ifelse(.imp > 0, 'Imputed','Observed'))
    }
    
    # plot data
    p <- cd %>%
      ggplot2::ggplot() +
      geom_mice(x, y, cd, m = data$m, ...) +
      ggplot2::labs(color = "", fill = "", size = "") +
      theme_mice
      # ggplot2::theme_classic() +
      # ggplot2::scale_color_manual(values = mice_colors) +
      # ggplot2::scale_fill_manual(values = mice_colors) #+
      #ggplot2::guides(fill = FALSE, size = FALSE)
    
    # # add facets
    # p <- p + facet_wrap(~z)
    
    # generate static or interactive plot
    if (interactive) {
      return(plotly::ggplotly(p))
    } else
      return(p)
  }
