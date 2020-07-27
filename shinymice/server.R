# shinymice server

# set-up
options(htmlwidgets.TOJSON_ARGS = list(na = 'string')) #to show NA values in dt, see https://github.com/rstudio/DT/issues/496

shinyServer(function(input, output, session) {
    rv <-
        reactiveValues(
            data = NULL,
            mids = NULL,
            m = NULL,
            maxit = NULL,
            done = NULL
        )
    
    observe({
        if (is.null(input$upload)) {
            rv$data <- get(input$choice, "package:mice")
        } else if (grepl("\\.Rdata$", input$upload$datapath, ignore.case = TRUE)) {
            env <- attach(input$upload$datapath)
            nm <- ls(name = env)
            if(is.mids(env[[nm]])){
                rv$mids <- env[[nm]]
                rv$data <- env[[nm]][["data"]]
            } else {rv$data <- env[[nm]]}
        } else if (grepl("\\.csv$", input$upload$datapath, ignore.case = TRUE)) {
            rv$data <- read.csv(input$upload$datapath, header = input$header)
        }
    })
    
    observeEvent(input$reset, {
        shinyjs::reset("sidebar")
        rv$data <- get(input$choice, "package:mice")
    })
    
    # observe({
    #     if (is.null(input$m)) {
    #         rv$m <- NULL
    #     } else {
    #         rv$m <- input$m
    #     }
    # })
    # 
    # observe({
    #     if (is.null(input$maxit)) {
    #         rv$maxit <- NULL
    #     } else {
    #         rv$maxit <- input$maxit
    #     }
    # })
    
    output$table <-
        renderDT({
            rv$data %>%  dplyr::mutate_if(is.numeric, round, 2) %>% 
                datatable(options = list(pageLength = 5)) %>%
                formatStyle(
                    names(rv$data),
                    target = "cell",
                    color = styleEqual("NA", "#B61A51"),
                    fontWeight = styleEqual("NA", "bold")
                )
            }, server = F)
    
    output$md_pattern <-
        renderPlot({
            md.pattern(rv$data)
        })
    
    output$names <-
        renderPrint({
            names(rv$data)[1:5]
        })
    
    observeEvent(input$mice, {
        rv$mids <-
            mice(
                rv$data,
                m = input$m,
                maxit = input$maxit,
                printFlag = FALSE
            )
    })
    
    #output$micecall <- renderPrint(rv$mids$call)
    
    observe({
        if (is.null(rv$mids)) {
            rv$done <- "Not done yet..."
        }
        else {
            rv$done <- "Done!"
        }
    })
    
    output$done <- renderPrint(rv$done)
    
    observe({
        if (is.null(rv$mids)) {
            rv$trace <-
                ggplot(data.frame(x = NA, y = NA), aes(x, y))  + labs(x = "",
                                                                      y = "",
                                                                      title = "No imputations to evaluate.") + theme_classic()
        }
        else {
            rv$trace <- gg.mids(rv$mids)
        }
    })
    
    observe(
        updateSelectInput(session, "varnr",
                     choices = names(rv$data))
    )
    
    observe(
        updateSelectInput(session, "histvar1",
                          choices = names(rv$data)),
    )
    
    observe(
            updateSelectInput(session, "histvar2",
                          choices = names(rv$data))
    )
    
    output$traceplot <- renderPlot({
        rv$trace[[input$varnr]]
    })
    
    output$hist <- renderPlot({
        if(is.numeric(rv$data[[input$histvar1]])){
            geom <- list(geom_histogram())} else {
                geom <- list(geom_bar())}
            
        rv$data %>% 
            dplyr::mutate(R = is.na(!!input$histvar2)) %>%  #factor(is.na(!!input$histvar2), levels = c("Observed", "Missing"))) %>% 
            ggplot(aes(x = !!input$histvar1, fill = is.na(!!input$histvar2))) +
            geom +
            theme_classic() +
            scale_fill_manual(values = mice:::mdc(1:2)) + 
            facet_wrap( ~ R, 
                        ncol = 1, 
                        labeller = labeller(R = c("Missing", "Observed") %>% setNames(c("TRUE", "FALSE"))))
    })
    
    output$savecsv <- downloadHandler(
        filename = function() {
            paste("dataset", ".csv", sep = "")
        },
        content = function(file) {
            write.csv(rv$data, file, row.names = FALSE)
        }
    )
    
    output$saverdata <- downloadHandler(
        filename = function() {
            paste("dataset.Rdata")
        },
        content = function(file) {
            dataset <- rv$data
            save(dataset, file = file)
        }
    )
    
    output$savemids <- downloadHandler(
        filename = function() {
            paste("mids.Rdata")
        },
        content = function(file) {
            mids <- rv$mids
            save(mids, file = file)
        }
    )
})