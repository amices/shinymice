#' 03_convergence UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_03_convergence_ui <- function(id){
  ns <- NS(id)
  fluidPage(fluidRow(
    column(
      4,
      tags$b("Evaluate algorithmic convergence"),
      br(),
      br(),
      "1. Monitor potential non-convergence through visual inspection.",
      br(),
      "2. Check out the convergence diagnostics.",
      br()
    ),
    column(8,
           tabsetPanel(
             tabPanel("Traceplot",
                      select_var(ns("var1")),
                      plotOutput(ns("trace_plot")),
                      tags$b("Interpretation:"),
                      "This traceplot displays the 'imputation chains' or 'streams' of the algorithm against the iteration number. On convergence, the different streams should be freely intermingled with one another, without showing any definite trends."),
             tabPanel("Convergence",
                      select_var(ns("var2")),
                      plotOutput(ns("rhat_plot")),
                      tags$b("Interpretation:"),
                      "This figure shows the potential scale reduction factor (R hat) conform Vehtari et al. (2019). If the potential scale reduction is high, then we have reason to believe that proceeding with further iterations may improve our inference.")
           ))
  ))
}
    
#' 03_convergence Server Functions
#'
#' @noRd 
mod_03_convergence_server <- function(id, data, imp){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    stopifnot(is.reactive(imp))
    observe(purrr::map(paste0("var", 1:2), function(x) {
      updateSelectInput(session, x, choices = names(data()))
    }))
    chains <- reactive(preprocess_thetas(imp()))
    output$trace_plot <- renderPlot({
      plot_trace(chains(), x = input$var1) #get x tick labels from imp()$maxit
    })
    output$rhat_plot <- renderPlot(plot_rhat(imp(), x = input$var2))
  })
}
    
## To be copied in the UI
# mod_03_convergence_ui("03_convergence_ui_1")
    
## To be copied in the server
# mod_03_convergence_server("03_convergence_ui_1", data, imp)
