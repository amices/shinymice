# shinymice server

# set-up
options(htmlwidgets.TOJSON_ARGS = list(na = 'string')) #to show NA values in dt, see https://github.com/rstudio/DT/issues/496

shinyServer(
    function(input, output, session) {
        # initial state reactive values
        rv <-
            reactiveValues(
                data = NULL,
                mids = NULL,
                m = NULL,
                maxit = NULL,
                done = NULL
            )
        
        ## Data tab
        # choose data
        observe({
            if (is.null(input$upload)) {
                rv$data <- get(input$choice, "package:mice")
            } else if (grepl("\\.Rdata$", input$upload$datapath, ignore.case = TRUE)) {
                env <- attach(input$upload$datapath)
                nm <- ls(name = env)
                if (is.mids(env[[nm]])) {
                    rv$mids <- env[[nm]]
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
                        names(rv$data),
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
                                  choices = names(rv$data)))
        # show correct variables
        observe(updateSelectInput(session, "histvar2",
                                  choices = names(rv$data)))
        # plot distributions
        output$hist <- renderPlot({
            # choose hist or bar depending of variable type
            if (is.numeric(rv$data[[input$histvar1]])) {
                geom <- list(geom_histogram())
            } else {
                geom <- list(geom_bar())
            }
            # plot
            rv$data %>%
                dplyr::mutate(R = factor(
                    is.na(!!input$histvar2),
                    levels = c(FALSE, TRUE),
                    labels = c("Observed", "Imputed")
                )) %>%  #factor(is.na(!!input$histvar2), levels = c("Observed", "Missing"))) %>%
                ggplot(aes(x = !!input$histvar1, fill = R)) +
                geom +
                mice:::theme_mice +
                theme(legend.position = "none") +
                facet_wrap(~ R,
                           ncol = 1,
                           labeller = labeller(R = c("Missing", "Observed") %>% setNames(c(
                               "Imputed", "Observed"
                           ))))
        }, res = 96)  
        
        ## Impute tab
        # show names data
        output$names <-
            renderPrint({
                names(rv$data)[1:5]
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
            rv$mids <-
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
            if (is.null(rv$mids)) {
            "Not done yet..."
        }
        else {
            "Done!"}
        })
        
        ## Evaluate tab
        ## Fluxplot subtab
        output$fluxplot <-
            renderPlot({if(is.mids(rv$mids)){gg.mids(rv$mids, geom = "fluxplot")}}, res = 96)  
        
        ## Traceplot subtab
        # show correct variables
        observe(updateSelectInput(session, "varnr",
                                  choices = names(rv$data)))
        # select traceplot variable
        observe({
            shinyFeedback::feedbackWarning(
                "varnr",
                all(!is.na(rv$mids$data[[input$varnr]])),
                "No imputations to visualize. Impute the missing data first and/or choose a different variable."
            )
            req(!is.null(rv$mids))
            # plot
            rv$trace <- gg.mids(rv$mids)
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
                is.null(rv$mids),
                "Please run some initial iterations in the 'Impute' tab"
            )
            if (!is.null(rv$mids)) {
                rv$mids <-
                    mice.mids(rv$mids,
                              maxit = input$midsmaxit,
                              printFlag = FALSE)
            }
            waiter::waiter_hide()
        })
        
        ## Imputations subtab
        # show correct variables
        observe(updateSelectInput(session, "plotvar",
                                  choices = names(rv$data)))
        # select plotting variable
        observe({
            shinyFeedback::feedbackWarning(
                "plotvar",
                all(!is.na(rv$mids$data[[input$plotvar]])),
                "No imputations to visualize. Impute the missing data first and/or choose a different variable."
            )
            req(!is.null(rv$mids))
            # plot
            rv$geom <-
                purrr::map(names(rv$data) %>% setNames(., names(rv$data)), function(x) {
                    gg.mids(rv$mids, x = x, geom = input$plottype)
                })
        })
        # plot imputations
        output$impplot <- renderPlot(
            rv$geom[[input$plotvar]], res = 96
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
            mids <- rv$mids
            save(mids, file = file)
        }
    )
    })