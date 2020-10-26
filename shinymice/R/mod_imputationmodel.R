#' imputationmodel UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_imputationmodel_ui <- function(id){
  ns <- NS(id)
  fluidPage(fluidRow(
      column(3,
             tags$b("Define the imputation model"),
             br(),
             br(),
             "1. Make the imputations reproducible by specifying a",
             numericInput("seed", "random number seed", value = NULL, min = 1),
             "2. Determine the ",
             numericInput("m", "number of imputations (m)", value = 5, min = 1),
             "and the ",
             numericInput("maxit", "number of iterations (maxit)", value = 5, min = 1),
             "3. Check (and modify??) the predictor matrix.",
             br(),
             "4. Run `mice()`.",
             br(),
             "5. Evaluate the in and outflux of the model.",
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
               tabPanel("Traceplot",
                        selectInput("var", "Select a variable", choices = names(mice::boys)),
                        plotOutput(ns("trace_plot")))
             ))
  ))
}
    
#' imputationmodel Server Functions
#'
#' @noRd 
mod_imputationmodel_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    dummy_plot <- ggplot2::ggplot(data = data.frame(x = c("dummy", "plot"), y = c(0,0))) + ggplot2::geom_point(ggplot2::aes(x=x,y=y))
    output$pred_plot <- renderPlot(dummy_plot)
    output$flux_plot <- renderPlot(dummy_plot)
    output$trace_plot <- renderPlot(dummy_plot)
  })
}
    
## To be copied in the UI
# mod_imputationmodel_ui("imputationmodel_ui_1")
    
## To be copied in the server
# mod_imputationmodel_server("imputationmodel_ui_1")
