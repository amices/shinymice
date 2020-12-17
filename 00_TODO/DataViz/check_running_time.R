# test speed R code

start_time <- Sys.time()
old.plot.mids(
  x = "wgt",
  dat = mids, plottype = "histogram")
# insert function(s)
end_time <- Sys.time()
old<-end_time - start_time

system.time({ old.plot.mids(
  x = "wgt",
  dat = mids, plottype = "histogram") })

system.time({ plot.mids(
  x = "wgt",
  dat = mids, plottype = "histogram") })


############# old function

old.plot.mids <-
  function(x,
           y = NULL,
           dat,
           plottype = "stripplot",
           interactive = FALSE,
           ...) {
    
    # parse inputs
    if (is.null(y) &
        inherits(x, "character")) {
      y <- x
    } 
    if (is.null(y) &
        inherits(x, "formula") & length(all.vars(x)) == 1) {
      x <- y <- all.vars(x)[1]
    } 
    if (is.null(y) &
        inherits(x, "formula") & length(all.vars(x)) == 2) {
      y <- all.vars(x)[1]
      x <- all.vars(x)[2]
    }
    
    if (plottype == "stripplot") {
      geom_mice <- stripplot_function
    } 
    if (plottype == "bwplot") {
      geom_mice <- boxplot_function
    } 
    if (plottype == "densityplot") {
      geom_mice <- densityplot_function
    } 
    if (plottype == "histogram" &
        is.numeric(dat$data[[x]]) == TRUE) {
      geom_mice <- histogram_function
    } 
    if (plottype == "histogram" &
        is.numeric(dat$data[[x]]) == FALSE) {
      geom_mice <- barplot_function
    } 
    if (plottype == "xyplot") {
      geom_mice <- xyplot_function
    }
    
    # preprocess data
    imps <- dat$data %>%
      is.na() %>%
      as.data.frame() %>%
      mutate(.id = 1:dim(.)[1]) %>%
      left_join(
        mice::complete(dat, "long", include = FALSE), #kan efficienter!!
        by = ".id",
        suffix = c("r", "")
      )  %>% mutate(.imp = as.factor(.imp)) %>%
      filter(.data[[paste0(x, "r")]] == TRUE |
               .data[[paste0(y, "r")]] == TRUE) %>% select(c(x, y, ".id", ".imp"))
    
    cd <-
      dat$data %>% mutate(.id = 1:dim(.)[1], .imp = as.factor(0))  %>% select(c(x, y, ".id", ".imp")) %>% rbind(., imps)
    
    m <- dat$m
    
    # plot data
    p <- cd %>%
      ggplot() +
      geom_mice(x, y, cd, m, ...)
    
    # generate static or interactive plot
    if (interactive) {
      return(plotly::ggplotly(p))
    } else
      return(p)
  }
