# shinymice server

# show NA values in dt, see https://github.com/rstudio/DT/issues/496
options("DT.TOJSON_ARGS" = list(na = "string"))

shinyServer(function(input, output, session) {
    # reactives
    rv <- reactiveValues(mids = NULL)
    
    observe({
        if (!is.null(input$upload_mids) & is.null(rv$mids)) {
            imp <-
                get_rdata_file(path = input$upload_mids$datapath)
            
            rv$mids <- list(imp)
            names(rv$mids) <-
                tools::file_path_sans_ext(input$upload_mids$name)
        }
    })
    
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
            d <- d$data
            # add message: "Please use the upload below to load a `mids` object, and not just the data"
        }
        if (!is.null(input$upload_mids)) {
            d <- get_rdata_file(path = input$upload_mids$datapath) %>% .$data
        }
        return(d)
    })
    
    vars <- reactive(names(data()))
    
    plotServer("hist1", dat = data())
    
    # update variable choices automatically
    varsUpdate <-
        function(UI_name) {
            updateSelectInput(session, UI_name, choices = vars())
        }
    
    observe(updateSelectInput(session, "banner2", choices = names(rv$mids)))
    
    # ## Banner
    # output$banner <- renderText({
    #     paste0(
    #         "Data: ",
    #         ifelse(
    #             is.null(input$upload),
    #             "test dataset (sample of mice::boys)",
    #             tools::file_path_sans_ext(input$upload$name)
    #         ),
    #         "\n",
    #         "Imputation: ",
    #         ifelse(
    #             input$mice < 1 & is.null(input$upload_mids),
    #             "no imputations (yet)",
    #             paste(names(rv$mids))#input$impname
    #         )
    #     )
    # })
    
    
    # tablutate data
    dataServer("table", dat = data())
    # output$table <-
    #     renderDT({
    #         DT_NA_highlight(data(), vars())
    #     }, server = F)
    # 
    # #output$n <- renderText(paste0("Descriptive statistics (n = ", nrow(data()), ")"))
    # output$descr <- renderDT(
    #     descr(data()) %>% 
    #         dplyr::mutate_if(is.numeric, round, 2) %>%
    #         DT::datatable(options = list(pageLength = 5)) %>%
    #         DT::formatStyle(
    #             "n",
    #             target = "cell",
    #             color = DT::styleInterval(c(nrow(data())-1), c("#B61A51", "black")),
    #             fontWeight = DT::styleInterval(c(nrow(data())-1), c("bold", "")),
    #         )
    #     
    # )
    # 
    # output$categ <- renderDT(least_perc(data()))
    
    ## Explore tab
    # plot pattern
    # make it interactive with two axes? see https://stackoverflow.com/questions/52833214/adding-second-y-axis-on-ggplotly
    output$md_pattern <-
        renderPlot({
            #md_plot <-
            plot_md_pattern(data = data())
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
        )
    })
    output$NA_scat <- renderPlotly({
        plot_NA_margins(data = data(),
                        x = input$NA_var2,
                        y = input$NA_var1)
    })
    
    ## Impute tab
    imputeServer("impute", dat = data(), r = rv)
    # # show names data or name of df with input$file$name, see https://mastering-shiny.org/action-transfer.html
    # # print call
    # output$micecall <- renderText({
    #     paste0(
    #         input$impname,
    #         " <- mice(data, m = ",
    #         input$m,
    #         ", maxit = ",
    #         input$maxit,
    #         ", seed = ",
    #         input$seed,
    #         ", ...)"
    #     )
    # })
    # 
    # observeEvent(input$mice, {
    #     # for spinner, see: https://shiny.john-coene.com/waiter/
    #     waiter::waiter_show(html = waiter::spin_throbber(),
    #                         color = waiter::transparent(.5))
    #     on.exit(waiter::waiter_hide())
    #     
    #     runmice <- paste0("mice(data(), m = ",
    #                       input$m,
    #                       ", maxit = ",
    #                       input$maxit,
    #                       ", seed = ",
    #                       input$seed)
    #     
    #     runmice <-
    #         ifelse(
    #             is.null(input$args),
    #             paste0(runmice, ")"),
    #             paste0(runmice, ",", input$args, ")")
    #         )
    #     
    #     if (is.null(rv$mids)) {
    #         rv$mids <-
    #             list(eval(parse(text = runmice)))
    #         names(rv$mids) <- input$impname
    #     }  else {
    #         rv$mids[[length(rv$mids) + 1]] <-
    #             eval(parse(text = runmice))
    #         names(rv$mids) <-
    #             c(names(rv$mids)[-length(rv$mids)], input$impname)
    #     }
    # })
    # 
    # observeEvent(input$iterate, {
    #     waiter::waiter_show(html = waiter::spin_throbber(),
    #                         color = waiter::transparent(.5))
    #     on.exit(waiter::waiter_hide())
    #     req(!is.null(rv$mids))
    #     rv$mids[[input$banner2]] <-
    #         mice.mids(rv$mids[[input$banner2]], maxit = input$midsmaxit)
    # })
    # 
    # indicate that data is imputed
    # replace with loader and checkmark combo, see https://github.com/daattali/advanced-shiny/tree/master/busy-indicator
    # output$done <- renderPrint({
    #     if (is.mids(rv$mids[[1]])) {
    #         "Done!"
    #     }
    # })
    
    ## Evaluate tab
    ## Fluxplot subtab
    output$fluxplot <-
        renderPlotly({
            req(!is.null(rv$mids[[input$banner2]]))
            gg.mids(rv$mids[[input$banner2]], geom = "fluxplot")
        })
    
    # plot traceplot
    output$traceplot <-
        renderPlotly({
            req(!is.null(rv$mids[[input$banner2]]))
            p <- gg.mids(rv$mids[[input$banner2]])
            p[[input$midsvar1]]
        })
    
    ## Imputations subtab
    # show correct variables
    observe(varsUpdate("midsvar1"))
    observe(varsUpdate("midsvar2"))
    # select plotting variable
    observe({
        shinyFeedback::feedbackWarning(
            "midsvar1",
            all(!is.na(rv$mids[[input$banner2]]$data[[input$midsvar1]])),
            "No imputations to visualize. Impute the missing data first and/or choose a different variable."
        )
        req(!is.null(rv$mids[[input$banner2]]))
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
    output$impplot <- renderPlotly({
        req(!is.null(rv$mids[[input$banner2]]))
        
        gg.mids(
            rv$mids[[input$banner2]],
            x = as.character(input$midsvar1),
            y = as.character(input$midsvar2),
            geom = input$plottype,
            interactive = TRUE
        )
    })
    
    ## Save tab
    output$save <- downloadHandler(
        filename = function() {
            paste0(
                ifelse(
                    input$mids_or_data == "Just the data",
                    "dataset",
                    input$banner2
                ),
                input$rdata_or_csv
            )
        },
        content = function(file) {
            if (input$rdata_or_csv == ".Rdata") {
                dataset <- ifelse(input$mids_or_data == "Just the data",
                                  data(),
                                  rv$mids[[input$banner2]])
                save(dataset, file = file)
            }
            if (input$rdata_or_csv == ".csv") {
                write.csv(data(), file, row.names = FALSE)
            }
        }
    )
})