# shinymice server

# set-up
# options(htmlwidgets.TOJSON_ARGS = list(na = 'string')) #to show NA values in dt, see https://github.com/rstudio/DT/issues/496
options("DT.TOJSON_ARGS" = list(na = "string"))
        
shinyServer(function(input, output, session) {
    data <- reactive({
        if (is.null(input$upload)) {
            mice::boys[sample.int(748, 100), ]
        }#vroom::vroom("data/mockdata.csv", delim = ",")}#get("boys", "package:mice")}
        
        else{
            ext <- tools::file_ext(input$upload$name)
            switch(
                ext,
                csv = vroom::vroom(input$upload$datapath, delim = ","),
                tsv = vroom::vroom(input$upload$datapath, delim = "\t"),
                Rdata = get_rdata_file(path = input$upload$datapath),
                validate(
                    "Invalid file; Please upload a .csv, .tsv or .Rdata file"
                )
            )
        }
    })
    
    datname <-
        
        # define reactives
        vars <- reactive(names(data()))
    # update variable choices automatically
    varsUpdate <-
        function(UI_name) {
            updateSelectInput(session, UI_name, choices = vars())
        }
    
    ## Data tab
    # choose data
    # observe({
    #     if (is.null(input$upload)) {
    #         data() <- get(input$choice, "package:mice")
    #         rv$nm <- input$choice
    #     } else if (grepl("\\.Rdata$", input$upload$datapath, ignore.case = TRUE)) { # or use tools::file_ext(), see https://mastering-shiny.org/action-transfer.html
    #         env <- attach(input$upload$datapath)
    #         rv$nm <- ls(name = env)
    #         if (is.mids(env[[rv$nm]])) {
    #             mids() <- env[[rv$nm]]
    #             data() <- env[[rv$nm]][["data"]]
    #         } else {
    #             data() <- env[[rv$nm]]
    #         }
    #     } else if (grepl("\\.csv$", input$upload$datapath, ignore.case = TRUE)) {
    #         data() <- read.csv(input$upload$datapath, header = input$header)
    #         rv$nm <- tools::file_path_sans_ext(input$upload$name)
    #     }
    # })
    # reset data
    # observeEvent(input$reset, {
    #     shinyjs::reset("sidebar")
    #     data() <- get(input$choice, "package:mice")
    #     rv$nm <- input$choice
    # })
    
    # tablutate data
    output$table <-
        renderDT({
            DT_NA_highlight(data(), vars())
        }, server = F)
    
    ## Explore tab
    # plot pattern
    output$md_pattern <-
        renderPlot(md.pattern(data()), res = 96)
    
    # show correct variables
    observe(varsUpdate("histvar1"))
    observe(varsUpdate("histvar2"))
    
    histscale <-
        reactive(ifelse(input$scalehist, "fixed", "free_y"))
    histbin <-
        reactive({
            if (input$binwidth == 0) {
                NULL
            } else {
                input$binwidth
            }
        })
    # plot distributions
    output$hist <- renderPlotly({
        conditional_hist(
            dat = data(),
            x = input$histvar1,
            y = input$histvar2,
            scaler = histscale(),
            binner = histbin()
        ) #%>% plotly::ggplotly()
    })#, res = 96)
    
    ## Impute tab
    # show names data or name of df with input$file$name, see https://mastering-shiny.org/action-transfer.html
    # print table of data
    output$datname <-
        renderText(paste(
            "Data:",
            ifelse(
                is.null(input$upload),
                "test dataset (sample of mice::boys)",
                tools::file_path_sans_ext(input$upload$name)
            )
        ))
    
    # print call
    output$micecall <- renderText({
        paste0("mice(data, m = ",
               input$m,
               ", maxit = ",
               input$maxit,
               ", printFlag = FALSE)")
    })
    # impute
    mids <- eventReactive(input$mice, {
        # for spinner, see: https://shiny.john-coene.com/waiter/
        waiter::waiter_show(html = waiter::spin_throbber(),
                            color = waiter::transparent(.5))
        on.exit(waiter::waiter_hide())
        
        if (input$mice) {
            mice(
                data(),
                m = input$m,
                maxit = input$maxit,
                printFlag = FALSE
            )
        }
        # if (input$mids){
        #     mice.mids(mids(),
        #               maxit = input$midsmaxit,
        #               printFlag = FALSE)
        # }
    })
    
    # indicate that data is imputed
    output$done <- renderPrint(#input$mice[[1]])
        {
            if (is.mids(mids())) {
                "Done!"
            }
        })
    
    ## Evaluate tab
    ## Fluxplot subtab
    output$fluxplot <-
        renderPlotly({
            if (is.mids(mids())) {
                gg.mids(mids(), geom = "fluxplot", interactive = T)
            }
        })#, res = 96)
    
    ## Traceplot subtab
    # show correct variables
    observe(varsUpdate("varnr"))
    # select traceplot variable
    trace <- reactive({
        shinyFeedback::feedbackWarning(
            "varnr",
            all(!is.na(mids()$data[[input$varnr]])),
            "No imputations to visualize. Impute the missing data first and/or choose a different variable."
        )
        req(!is.null(mids()))
        # plot
        gg.mids(mids())
    })
    # plot traceplot
    output$traceplot <- renderPlotly(trace()[[input$varnr]])#, res = 96)
    
    #mids <- eventReactive(input$mids, mice.mids(mids()))
    # add iterations
    # observeEvent(input$mids, {
    #     waiter::waiter_show(html = waiter::spin_throbber(),
    #                         #spin_fading_circles(),
    #                         color = waiter::transparent(.5))
    #     shinyFeedback::feedbackWarning(
    #         "midsmaxit",
    #         is.null(mids()),
    #         "Please run some initial iterations in the 'Impute' tab"
    #     )
    #     if (!is.null(mids())) {
    #         mids() <-
    #             mice.mids(mids(),
    #                       maxit = input$midsmaxit,
    #                       printFlag = FALSE)
    #     }
    #     waiter::waiter_hide()
    # })
    
    ## Imputations subtab
    # show correct variables
    observe(varsUpdate("midsvar1"))
    observe(varsUpdate("midsvar2"))
    # select plotting variable
    observe({
        shinyFeedback::feedbackWarning(
            "midsvar1",
            all(!is.na(mids()$data[[input$midsvar1]])),
            "No imputations to visualize. Impute the missing data first and/or choose a different variable."
        )
        req(!is.null(mids()))
        shinyFeedback::feedbackWarning(
            "midsvar2",
            input$midsvar1 == input$midsvar2 &
                input$plottype == "xyplot",
            "The two variables should be different"
        )
        if (input$plottype == "xyplot") {
            req(input$midsvar1 != input$midsvar2)
        }
    })
    # plot imputations
    output$impplot <- renderPlotly(gg.mids(
        mids(),
        x = as.character(input$midsvar1),
        y = as.character(input$midsvar2),
        geom = input$plottype,
        interactive = TRUE
    ))#,
    #res = 96)
    
    ## Save tab
    # as csv
    output$savecsv <- downloadHandler(
        filename = function() {
            paste("dataset", ".csv", sep = "")
        },
        content = function(file) {
            write.csv(data(), file, row.names = FALSE)
        }
    )
    # as rdata
    output$saverdata <- downloadHandler(
        filename = function() {
            paste("dataset.Rdata")
        },
        content = function(file) {
            dataset <- data()
            save(dataset, file = file)
        }
    )
    # save imps
    output$savemids <- downloadHandler(
        filename = function() {
            paste("mids.Rdata")
        },
        content = function(file) {
            mids <- mids()
            save(mids, file = file)
        }
    )
})