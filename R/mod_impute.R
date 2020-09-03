imputeUI <- function(id) {
  fluidRow(
    column(
      4,
      h2("Impute missingness"),
      numericInput(
        NS(id,"m"),
        label = "Number of imputations",
        value = 5,
        min = 1,
        step = 1
      ),
      numericInput(
        NS(id,"maxit"),
        label = "Number of iterations",
        value = 5,
        min = 1,
        step = 5
      ),
      textInput(NS(id,"seed"), "Set random number generator seed", value = "123"),
      textInput(NS(id,"impname"), "Name the imputation object", value = "imp"),
      textInput("args", "Additional arguments (optional)", value = NULL)
    ),
    column(8,
           tabsetPanel(
             tabPanel(
               "Mice call",
               br(),
               verbatimTextOutput(NS(id,"micecall")),
               actionButton(NS(id,"mice"), "Impute", icon = icon("hourglass-start")),
               helpText("This may take a minute.")
             )
           ))
  )
}

imputeServer <- function(id, dat, r) {
  moduleServer(id, function(input, output, session, data = dat, rv = r) {
    
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
        ", ...)"
      )
    })
    
    observeEvent(input$mice, {
      # for spinner, see: https://shiny.john-coene.com/waiter/
      waiter::waiter_show(html = waiter::spin_throbber(),
                          color = waiter::transparent(.5))
      on.exit(waiter::waiter_hide())
      
      runmice <- paste0("mice(mice::boys, m = ",
                        input$m,
                        ", maxit = ",
                        input$maxit,
                        ", seed = ",
                        input$seed)
      
      runmice <-
        ifelse(
          is.null(input$args),
          paste0(runmice, ")"),
          paste0(runmice, ",", input$args, ")")
        )
      
      if (is.null(rv$mids)) {
        rv$mids <-
          list(eval(parse(text = runmice)))
        names(rv$mids) <- input$impname
      }  else {
        rv$mids[[length(rv$mids) + 1]] <-
          eval(parse(text = runmice))
        names(rv$mids) <-
          c(names(rv$mids)[-length(rv$mids)], input$impname)
      }
    })
    
    observeEvent(input$iterate, {
      waiter::waiter_show(html = waiter::spin_throbber(),
                          color = waiter::transparent(.5))
      on.exit(waiter::waiter_hide())
      req(!is.null(rv$mids))
      rv$mids[[input$banner2]] <-
        mice.mids(rv$mids[[input$banner2]], maxit = input$midsmaxit)
    })
    
    return(rv)
    
  })
}

# # test the module
# testApp <- function() {
#   ui <- fluidPage(
#     imputeUI("test1")
#   )
#   server <- function(input, output, session) {
#     imputeServer("test1", dat = mice::boys, r = rv)
#   }
#   shinyApp(ui, server)  
# }
