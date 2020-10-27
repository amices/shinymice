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
             "1. Make the imputations reproducible by specifying an initial random value (seed)",
             div(numericInput("seed", NULL, value = 123, min = 1, width = 100), style = "margin-bottom: -15px"),
             "2. Determine the number of imputations (m)",
             div(numericInput("m", NULL, value = 5, min = 1, width = 100), style = "margin-bottom: -15px"),
             "3. And the number of iterations (maxit)",
            div(numericInput("maxit", NULL, value = 5, min = 1, width = 100), style = "margin-bottom: -15px"),
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
               tabPanel("Traceplot",
                        div(style="display:inline-block; margin-left: 15px", selectInput("var", label = NULL, choices = c("Select a variable", names(mice::boys)), width = 200)),
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
