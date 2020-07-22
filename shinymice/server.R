#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/


options(htmlwidgets.TOJSON_ARGS = list(na = 'string')) #to show NA values in dt, see https://github.com/rstudio/DT/issues/496

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    rv <- reactiveValues(data = NULL, mids = NULL, m = NULL, maxit = NULL, done = NULL)
    
    observe({
        if (is.null(input$upload)) {
            rv$data <- get(input$choice, "package:mice")
        } else {rv$data <- read.csv(input$upload$datapath, header = input$header)}
    })
    
    observeEvent(input$reset, {
        shinyjs::reset("sidebar")
        rv$data <- get(input$choice, "package:mice")
    })
    
    observe({
        if (is.null(input$m)) {
            rv$m <- NULL
        } else {rv$m <- input$m}
    })
    
    observe({
        if (is.null(input$maxit)) {
            rv$maxit <- NULL
        } else {rv$maxit <- input$maxit}
    })
    
    output$table <-
        renderDT({rv$data %>% 
                datatable(options = list(pageLength = 5)) %>% 
            formatStyle(
                names(rv$data ),
                target = "cell",
                color = styleInterval(9999, c("grey", "black"))
            )}, server = F)
    #display the actual data frame in a table
    # dataset <- get(input$dataset, "package:datasets") #redundant
    
    output$md_pattern <-
        renderPlot({
            md.pattern(rv$data)
        }#, height = function() {2*session$clientData$output_md_pattern_width}
    )
    
    output$summary <-
        renderPrint({
            #display a statistical summary of the data with fixed-width (verbatim) text
            # dataset <- get(input$dataset, "package:datasets") #redundant, after adding reactive and () after dataset
            summary(rv$data)
        })

    output$names <-
        renderPrint({
            #display a statistical summary of the data with fixed-width (verbatim) text
            # dataset <- get(input$dataset, "package:datasets") #redundant, after adding reactive and () after dataset
            names(rv$data)[1:5]
        })
    
    observeEvent(input$mice, {
        #rv$mids <- list(1)
        rv$mids <- mice(rv$data, m = input$m, maxit = rv$maxit, printFlag = FALSE)
    })
    
    observe({
        if (is.null(rv$mids)) {
            rv$done <- "not done yet..."} 
        #else if (rv$mids == 1) {rv$done <- "processing..."}
        else {rv$done <- "done!"}
    })
    
    output$done <- renderPrint(rv$done)
    
    observe({
        if (is.null(rv$mids)) {
            rv$trace <- ggplot(data.frame(x=NA, y=NA), aes(x, y))  + labs(x = "", y = "", title = "No imputations to evaluate.") + theme_classic()} 
        else {rv$trace <- gg.mids(rv$mids)}
    })
    
    output$traceplot <- renderPlot({
        rv$trace
    })
})