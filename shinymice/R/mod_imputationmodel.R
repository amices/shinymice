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
  fluidPage(fluidRow(
    column(
      3,
      tags$b("Define the imputation model"),
      br(),
      br(),
      "1. Make the imputations reproducible by specifying an initial random value (seed)",
      set_number("seed", val = 123),
      no_br(),
      "2. Determine the number of imputations (m)",
      set_number("m", val = 5),
      no_br(),
      "3. And the number of iterations (maxit)",
      set_number("maxit", val = 10),
      no_br(),
      "4. Evaluate the in and outflux of the model.",
      br(),
      "5. Check (and modify) the predictor matrix.",
      br(),
      "6. Run `mice()`.",
      br(),
      actionButton(ns("run_mice"), label = "Impute", width = 100),
      br(),
      "7. Monitor potential non-convergence through visual inspection.",
      verbatimTextOutput(ns("micecall"))
    ),
    column(9,
           tabsetPanel(
             tabPanel(
               "Fluxplot",
               plotly::plotlyOutput(#  plotOutput(
                 ns("flux_plot")),
               br(),
               tags$b("Interpretation:"),
               "Influx and outflux are summaries of the missing data pattern intended to aid in the construction of imputation models. The influx of a variable quantifies how well its missing data connect to the observed data on other variables. The outflux of a variable quantifies how well its observed data connect to the missing data on other variables. Keeping everything else constant, variables with high influx and outflux are preferred."
             ),
             tabPanel("Predictor matrix",
                      plotOutput(ns("pred_plot")),
                      "Each row in the predictor matrix identifies which predictors are to be used for the variable in the row name."),
             tabPanel("Traceplot",
                      select_var(ns("var1")),
                      plotOutput(ns("trace_plot")))
           ))
  ))
}

#' imputationmodel Server Functions
#'
#' @noRd
mod_imputationmodel_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    p <- dummy_plot()
    #mids <- mice::mice(mice::boys, maxit = 1)
    output$pred_plot <- renderPlot(p)
    output$flux_plot <- #renderPlot({
      plotly::renderPlotly({
        plot_flux(mice::boys)
        #return(plotly::ggplotly())
      })
    output$trace_plot <- renderPlot(p)
  })
}

## To be copied in the UI
# mod_imputationmodel_ui("imputationmodel_ui_1")

## To be copied in the server
# mod_imputationmodel_server("imputationmodel_ui_1")
