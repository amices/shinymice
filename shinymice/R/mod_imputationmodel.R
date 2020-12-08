#' imputationmodel UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_imputationmodel_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(
        4,
        tags$b("Define the imputation model"),
        br(),
        br(),
        "1. Look at the missing data pattern for an overview of the missingness.",
        br(),
        "2. Evaluate the in and outflux of the model.",
        br(),
        "3. Check (and modify) the predictor matrix.",
        br(),
        with_red_star(
          "4. Make the imputations reproducible by specifying an initial random value (seed)"
        ),
        set_number(ns("seed"), val = 123),
        no_br(),
        with_red_star("5. Determine the number of imputations (m)"),
        set_number(ns("m"), val = 5),
        no_br(),
        with_red_star("6. And the number of iterations (maxit)"),
        set_number(ns("maxit"), val = 2),
        no_br(),
        "7. Add optional arguments to the imputation model (e.g., `method = 'pmm'`)",
        textInput(ns("add_args"), label = NULL, value = NULL),
        no_br(),
        "8. Run `mice()`.",
        br(),
        actionButton(ns("run_mice"), label = "Impute", width = 100),
        br(),
        "9. Monitor potential non-convergence through visual inspection.",
        verbatimTextOutput(ns("micecall"))
      ),
      column(8,
             tabsetPanel(
               tabPanel(
                 "Missingness pattern",
                 plotOutput(ns("md_plot")),
                 br(),
                 tags$b("Interpretation:"),
                 "The missing data pattern influences the amount of information that can be transferred between variables. Imputation can be more precise if other variables are non-missing for those cases that are to be imputed. The reverse is also true. Predictors are potentially more powerful if they have are non-missing in rows that are vastly incomplete."
               ),
               tabPanel(
                 "Fluxplot",
                 plotly::plotlyOutput(ns("flux_plot")),
                 br(),
                 tags$b("Interpretation:"),
                 "Influx and outflux are summaries of the missing data pattern intended to aid in the construction of imputation models. The influx of a variable quantifies how well its missing data connect to the observed data on other variables. The outflux of a variable quantifies how well its observed data connect to the missing data on other variables. Keeping everything else constant, variables with high influx and outflux are preferred."
               ),
               tabPanel(
                 "Predictor matrix",
                 actionButton(ns("quickpred"), "Best guess"),
                 plotOutput(ns("pred_plot")),
                 br(),
                 tags$b("Interpretation:"),
                 "Each row in the predictor matrix identifies which predictors are to be used for the variable in the row name."
               ),
               tabPanel("Traceplot",
                        #               h6("Please make sure to impute the incomplete data first (see left-hand side)"),
                        select_var(ns("var1")),
                        plotOutput(ns("trace_plot"))
                        # "Convergence diagnostic",
                        # plotOutput(ns("rhat_plot")))))
               )))))
}

#' imputationmodel Server Functions
#'
#' @noRd
mod_imputationmodel_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    stopifnot(is.reactive(data))
    updateSelectInput(session, "var1", choices = names(isolate(data())))
    output$md_plot <- renderPlot(plot_md_pattern(data()))
    output$flux_plot <-
      plotly::renderPlotly({
        plot_flux(data())
      })
    output$pred_plot <-
      renderPlot({
        if (input$quickpred == 0) {
          plot_pred_matrix(data())
        } else {
          plot_pred_matrix(mice::quickpred(data()))
        }
      })
    imp <-
      eventReactive(input$run_mice, {
        waiter::waiter_show(html = waiter::spin_throbber(),
                            color = waiter::transparent(.5))
        on.exit(waiter::waiter_hide())
        eval(parse(
          text = paste(
            "mice::mice(data(), seed = input$seed, m = input$m, maxit = input$maxit, ",
            input$add_args,
            ")")
        ))
      }) #, ignoreNULL = FALSE, ignoreInit = TRUE
    # TODO: validate additional arguments with formals(mice::mice)
    chains <- reactive(preprocess_thetas(imp()))
    output$trace_plot <- renderPlot({
      validate(need(input$run_mice, message = "Please impute the incomplete data first."))
      trace_one_variable(chains(), x = input$var1)
    })
    output$rhat_plot <- renderPlot(plot_rhat(imp(), x = input$var1))
    return(reactive(imp()))
  })
}

## To be copied in the UI
# mod_imputationmodel_ui("imputationmodel_ui_1")

## To be copied in the server
# mod_imputationmodel_server("imputationmodel_ui_1")
