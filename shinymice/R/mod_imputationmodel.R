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
      "4. Check (and modify??) the predictor matrix.",
      br(),
      "5. Run `mice()`.",
      br(),
      actionButton(ns("run_mice"), label = "Impute", width = 100),
      br(),
      "6. Evaluate the in and outflux of the model.",
      br(),
      "6. Monitor potential non-convergence through visual inspection.",
      verbatimTextOutput(ns("micecall"))
    ),
    column(9,
           tabsetPanel(
             tabPanel("Predictor matrix",
                      plotOutput(ns("pred_plot"))),
             tabPanel("Fluxplot",
                      plotOutput(ns("flux_plot"))),
             tabPanel(
               "Traceplot",
               select_var(ns("var1")),
               plotOutput(ns("trace_plot"))
             )
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
    output$pred_plot <- renderPlot(p)
    output$flux_plot <- renderPlot(p)
    output$trace_plot <- renderPlot(p)
  })
}

## To be copied in the UI
# mod_imputationmodel_ui("imputationmodel_ui_1")

## To be copied in the server
# mod_imputationmodel_server("imputationmodel_ui_1")
