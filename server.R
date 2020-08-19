# shinymice server

# show NA values in dt, see https://github.com/rstudio/DT/issues/496
options("DT.TOJSON_ARGS" = list(na = "string"))

shinyServer(function(input, output, session) {
    # reactives
    data <- reactive({
        if (is.null(input$upload)) {
            set.seed(123)
            d <- mice::boys[sample.int(748, 100), ]
        }
        else{
            ext <- tools::file_ext(input$upload$name)
            d <- switch(
                ext,
                csv = vroom::vroom(input$upload$datapath, delim = ","),
                tsv = vroom::vroom(input$upload$datapath, delim = "\t"),
                Rdata = get_rdata_file(path = input$upload$datapath),
                validate(
                    "Invalid file; Please upload a .csv, .tsv or .Rdata file"
                )
            )
        }
        if (is.mids(d)) {
            #rv$imp <- isolate(d)
            d <- d$data
            # add message: "Please use the upload below to load a `mids` object, and not just the data"
        }
        return(d)
    })
    
    rv <- reactiveValues(imp = NULL)
    
    output$test <- renderPrint({if ( is.null(input$upload_mids)) return(NULL)
        d <- get_rdata_file(path = isolate(input$upload_mids)$datapath)
        names(d$data)})
    
    # observe({if(is.null(input$upload_mids)){
    #     rv$imp <- NULL} else {
    #     rv$imp <- get_rdata_file(path = isolate(input$upload_mids)$datapath)}})
    
    vars <- reactive(names(data()))
    
    # update variable choices automatically
    varsUpdate <-
        function(UI_name) {
            updateSelectInput(session, UI_name, choices = vars())
        }
    
    ## Banner
    output$banner <- renderText({
        paste0(
            "Data: ",
            ifelse(
                is.null(input$upload),
                "test dataset (sample of mice::boys)",
                tools::file_path_sans_ext(input$upload$name)
            ),
            "\n",
            "Imputation: ",
            ifelse(
                input$mice < 1 & is.null(input$midsupload),
                "no imputations (yet)",
                input$impname
            )
        )
    })
    
    # tablutate data
    output$table <-
        renderDT({
            DT_NA_highlight(data(), vars())
        }, server = F)
    
    ## Explore tab
    # plot pattern
    # make it interactive with two axes? see https://stackoverflow.com/questions/52833214/adding-second-y-axis-on-ggplotly
    output$md_pattern <-
        renderPlot({
            #md_plot <- 
                plot_md_pattern(data = data())
            
            #interactive_md_plot(md_plot)
            }, res = 72)

    # show correct variables
    observe(varsUpdate("NA_var1"))
    observe(varsUpdate("NA_var2"))
    
    # show best predictors
    output$relations <-
        renderText(test_NA_y(data(), x = input$NA_var1)$top3)
    output$NA_relations <-
        renderText(test_predictors(data(), x = input$NA_var1))
    # plot distributions
    # make plots the correct heigth every time, see https://stackoverflow.com/questions/34792998/shiny-variable-height-of-renderplot
    output$NA_hist <- renderPlotly({
        conditional_hist(
                dat = data(),
                x = input$NA_var1,
                y = input$NA_var2,
                scaler = input$scalehist,
                binner = input$bins
            )})
    output$NA_scat <- renderPlotly({
            plot_NA_margins(
                data = data(),
                x = input$NA_var2,
                y = input$NA_var1
            )
    })
    
    ## Impute tab
    # show names data or name of df with input$file$name, see https://mastering-shiny.org/action-transfer.html
    # print call
    output$micecall <- renderText({
        paste0(
            input$impname,
            " <- mice(data, m = ",
            input$m,
            ", maxit = ",
            input$maxit,
            ", seed = ",
            input$seed,
            ", printFlag = FALSE)"
        )
    })
    
    observeEvent(input$mice, {
        # for spinner, see: https://shiny.john-coene.com/waiter/
        waiter::waiter_show(html = waiter::spin_throbber(),
                            color = waiter::transparent(.5))
        on.exit(waiter::waiter_hide())
        
        if (is.null(rv$imp)) {
            rv$imp <-
                list(mice(data(),
                          m = input$m,
                          maxit = input$maxit,
                          seed = as.numeric(input$seed)))
        }  else {
            rv$imp <-
                c(rv$imp,
                  list(mice(
                      data(),
                      m = input$m,
                      maxit = input$maxit
                  )))
        }
    })
    
    observeEvent(input$iterate, {
        waiter::waiter_show(html = waiter::spin_throbber(),
                            color = waiter::transparent(.5))
        on.exit(waiter::waiter_hide())
        req(!is.null(rv$imp))
        rv$imp[[input$mice]] <-
            mice.mids(rv$imp[[input$mice]], maxit = input$midsmaxit)
    })
    
    # indicate that data is imputed
    output$done <- renderPrint({
        if (is.mids(rv$imp[[input$mice]])) {
            "Done!"
        }
    })
    
    ## Evaluate tab
    ## Fluxplot subtab
    output$fluxplot <-
        renderPlotly({
            if (is.mids(rv$imp[[input$mice]])) {
                gg.mids(rv$imp[[input$mice]], geom = "fluxplot", interactive = T)
            }
        })
    
    ## Traceplot subtab
    # show correct variables
    #observe(varsUpdate("varnr"))
    # select traceplot variable
    trace <- reactive({
        shinyFeedback::feedbackWarning(
            "varnr",
            all(!is.na(rv$imp[[input$mice]]$data[[input$midsvar1]])),
            "No imputations to visualize. Impute the missing data first and/or choose a different variable."
        )
        req(!is.null(rv$imp[[input$mice]]))
        # plot
        gg.mids(rv$imp[[input$mice]])
    })
    # plot traceplot
    output$traceplot <-
        renderPlotly(trace()[[input$midsvar1]])#, res = 96)
    
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
            all(!is.na(rv$imp[[input$mice]]$data[[input$midsvar1]])),
            "No imputations to visualize. Impute the missing data first and/or choose a different variable."
        )
        req(!is.null(rv$imp[[input$mice]]))
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
    output$impplot <- renderPlotly(
        gg.mids(
            rv$imp[[input$mice]],
            x = as.character(input$midsvar1),
            y = as.character(input$midsvar2),
            geom = input$plottype,
            interactive = TRUE
        )
    )
    
    ## Save tab
    output$save <- downloadHandler(
        filename = function() {
            paste0(
                ifelse(
                    input$mids_or_data == "Just the data",
                    "dataset",
                    input$impname
                ),
                input$rdata_or_csv
            )
        },
        content = function(file) {
            if (input$rdata_or_csv == ".Rdata") {
                dataset <-
                    data() #add if()/switch() statement to add rv$imp object instead
                save(dataset, file = file)
            }
            if (input$rdata_or_csv == ".csv") {
                write.csv(data(), file, row.names = FALSE)
            }
        }
    )
})