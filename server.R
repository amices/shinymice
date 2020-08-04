# shinymice server

# set-up
options(htmlwidgets.TOJSON_ARGS = list(na = 'string')) #to show NA values in dt, see https://github.com/rstudio/DT/issues/496

shinyServer(
    function(input, output, session) {
        
        data <- reactive({
            req(input$upload)
            
            ext <- tools::file_ext(input$upload$name)
            switch(ext,
                   csv = vroom::vroom(input$upload$datapath, delim = ","),
                   tsv = vroom::vroom(input$upload$datapath, delim = "\t"),
                   Rdata = get_rdata_file(path = input$upload$datapath),
                   validate("Invalid file; Please upload a .csv, .tsv or .Rdata file")
            )
        })
        
        # initial state reactive values
        rv <-
            reactiveValues(
                data = NULL,
                imp = NULL
                )
        # define reactives
        vars <- reactive(names(data()))
        
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
        #             rv$imp <- env[[rv$nm]]
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
            renderPlot(
                md.pattern(data()), res = 96
            )
        
        # update variable choices automatically
        varsUpdate <- function(UI_name){updateSelectInput(session, UI_name, choices = vars())}
        
        # show correct variables
        observe(varsUpdate("histvar1"))
        observe(varsUpdate("histvar2"))
        
        observe({if(input$scalehist){rv$scalehist <- "fixed"} else {rv$scalehist <- "free_y"}})
        # plot distributions
        output$hist <- renderPlot({
          conditional_hist(dat = data(), x = input$histvar1, y = input$histvar2, scaler = rv$scalehist)
        }, res = 96)  
        
        ## Impute tab
        # show names data or name of df with input$file$name, see https://mastering-shiny.org/action-transfer.html
        # print table of data
        output$datname <- renderPrint(rv$nm)
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
                    data(),
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
        observe(varsUpdate("varnr"))
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
        observe(varsUpdate("midsvar1"))
        observe(varsUpdate("midsvar2"))
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
            mids <- rv$imp
            save(mids, file = file)
        }
    )
    }
)