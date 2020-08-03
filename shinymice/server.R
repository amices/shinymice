# shinymice server

# set-up
options(htmlwidgets.TOJSON_ARGS = list(na = 'string')) #to show NA values in dt, see https://github.com/rstudio/DT/issues/496

shinyServer(
    function(input, output, session) {
        # initial state reactive values
        rv <-
            reactiveValues(
                data = NULL,
                imp = NULL
                )
        
        vars <- reactive(names(rv$data))
        
        ## Data tab
        # choose data
        observe({
            if (is.null(input$upload)) {
                rv$data <- get(input$choice, "package:mice")
            } else if (grepl("\\.Rdata$", input$upload$datapath, ignore.case = TRUE)) { # or use tools::file_ext(), see https://mastering-shiny.org/action-transfer.html
                env <- attach(input$upload$datapath)
                nm <- ls(name = env)
                if (is.mids(env[[nm]])) {
                    rv$imp <- env[[nm]]
                    rv$data <- env[[nm]][["data"]]
                } else {
                    rv$data <- env[[nm]]
                }
            } else if (grepl("\\.csv$", input$upload$datapath, ignore.case = TRUE)) {
                rv$data <- read.csv(input$upload$datapath, header = input$header)
            }
            # shinyFeedback::feedbackWarning(
            #     "upload",
            #     !is.data.frame(rv$data),
            #     "No imputations to visualize. Impute the missing data first and/or choose a different variable."
            # )
            # req(is.data.frame(rv$data))
        })
        # reset data
        observeEvent(input$reset, {
            shinyjs::reset("sidebar")
            rv$data <- get(input$choice, "package:mice")
        })
        # print table of data
        output$table <-
            renderDT({
                rv$data %>%  dplyr::mutate_if(is.numeric, round, 2) %>%
                    datatable(options = list(pageLength = 5)) %>%
                    formatStyle(
                        vars(),
                        target = "cell",
                        color = styleEqual("NA", "#B61A51"),
                        fontWeight = styleEqual("NA", "bold")
                    )
            }, server = F)
        
        ## Explore tab
        # plot pattern
        output$md_pattern <-
            renderPlot(
                md.pattern(rv$data), res = 96
            )
        # show correct variables
        observe(updateSelectInput(session, "histvar1",
                                  choices = vars()))
        # show correct variables
        observe(updateSelectInput(session, "histvar2",
                                  choices = vars()))
        observe({if(input$scalehist){rv$scalehist <- "fixed"} else {rv$scalehist <- "free_y"}})
        # plot distributions
        output$hist <- renderPlot({
          conditional_hist(dat = rv$data, x = input$histvar1, y = input$histvar2, scaler = rv$scalehist)
        }, res = 96)  
        
        ## Impute tab
        # show names data or name of df with input$file$name, see https://mastering-shiny.org/action-transfer.html
        output$names <-
            renderPrint({
                vars() %>% .[1:5]
            })
        # print call
        output$micecall <- renderText({
            paste0(
                "mice(data, m = ",
                input$m,
                ", maxit = ",
                input$maxit,
                ", printFlag = FALSE)"
            )
        })
        # impute
        observeEvent(input$mice, {
            # for spinner, see: https://shiny.john-coene.com/waiter/
            waiter::waiter_show(html = waiter::spin_throbber(),
                                color = waiter::transparent(.5))
            rv$imp <-
                mice(
                    rv$data,
                    m = input$m,
                    maxit = input$maxit,
                    # method = input$micemeth,
                    # predictorMatrix = input$micepred,
                    # blocks = input$micebloc,
                    # visitSequence = input$micevisi,
                    # formulas = input$miceform,
                    # seed = input$miceseed,
                    printFlag = FALSE
                )
            waiter::waiter_hide()
        })
        
        # indicate that data is imputed
        output$done <- renderPrint({
            if (is.null(rv$imp)) {
            "Not done yet..."
        }
        else {
            "Done!"}
        })
        
        ## Evaluate tab
        ## Fluxplot subtab
        output$fluxplot <-
            renderPlot({if(is.mids(rv$imp)){gg.mids(rv$imp, geom = "fluxplot")}}, res = 96)  
        
        ## Traceplot subtab
        # show correct variables
        observe(updateSelectInput(session, "varnr",
                                  choices = vars()))
        # select traceplot variable
        observe({
            shinyFeedback::feedbackWarning(
                "varnr",
                all(!is.na(rv$imp$data[[input$varnr]])),
                "No imputations to visualize. Impute the missing data first and/or choose a different variable."
            )
            req(!is.null(rv$imp))
            # plot
            rv$trace <- gg.mids(rv$imp)
        })
        # plot traceplot
        output$traceplot <- renderPlot(
            rv$trace[[input$varnr]], res = 96)
        # add iterations
        observeEvent(input$mids, {
            waiter::waiter_show(html = waiter::spin_throbber(),
                                #spin_fading_circles(),
                                color = waiter::transparent(.5))
            shinyFeedback::feedbackWarning(
                "midsmaxit",
                is.null(rv$imp),
                "Please run some initial iterations in the 'Impute' tab"
            )
            if (!is.null(rv$imp)) {
                rv$imp <-
                    mice.mids(rv$imp,
                              maxit = input$midsmaxit,
                              printFlag = FALSE)
            }
            waiter::waiter_hide()
        })
        
        ## Imputations subtab
        # show correct variables
        observe(updateSelectInput(session, "midsvar1",
                                  choices = vars()))
        observe(updateSelectInput(session, "midsvar2",
                                  choices = vars()))
        # select plotting variable
        observe({
            shinyFeedback::feedbackWarning(
                "midsvar1",
                all(!is.na(rv$imp$data[[input$midsvar1]])),
                "No imputations to visualize. Impute the missing data first and/or choose a different variable."
            )
            req(!is.null(rv$imp))
            shinyFeedback::feedbackWarning(
                "midsvar2",
                input$midsvar1==input$midsvar2 & input$plottype == "xyplot",
                "The two variables should be different"
            )
            if(input$plottype == "xyplot"){
            req(input$midsvar1!=input$midsvar2)}
            # plot
            # rv$geom <-
            #     purrr::map(vars() %>% setNames(., vars()), function(x) {
            #         gg.mids(rv$imp, x = x, y = input$midsvar2, geom = input$plottype)
            #     })
            
        })
        # plot imputations
        output$impplot <- renderPlot(
            gg.mids(rv$imp, x = as.character(input$midsvar1), y = as.character(input$midsvar2), geom = input$plottype),
            res = 96
        )
        
    ## Save tab
    # as csv
    output$savecsv <- downloadHandler(
        filename = function() {
            paste("dataset", ".csv", sep = "")
        },
        content = function(file) {
            write.csv(rv$data, file, row.names = FALSE)
        }
    )
    # as rdata
    output$saverdata <- downloadHandler(
        filename = function() {
            paste("dataset.Rdata")
        },
        content = function(file) {
            dataset <- rv$data
            save(dataset, file = file)
        }
    )
    # save imps
    output$savemids <- downloadHandler(
        filename = function() {
            paste("mids.Rdata")
        },
        content = function(file) {
            mids <- rv$imp
            save(mids, file = file)
        }
    )
    })