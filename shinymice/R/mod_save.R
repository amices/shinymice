#' save UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_save_ui <- function(id){
  ns <- NS(id)
  fluidPage(fluidRow(
    column(3,
           tags$b("Save the data/results"),
           br(),
           br(),
           "[This tab is under construction.]"
    ),
    column(9,
           tabsetPanel(
             tabPanel("Some plot?",
                      plotOutput(ns("save_plot")))
           ))
  ))
}
    
#' save Server Functions
#'
#' @noRd 
mod_save_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    dummy_plot <- ggplot2::ggplot(data = data.frame(x = c("dummy", "plot"), y = c(0,0))) + ggplot2::geom_point(ggplot2::aes(x=x,y=y))
    output$save_plot <- renderPlot(dummy_plot)
  })
}
    
## To be copied in the UI
# mod_save_ui("save_ui_1")
    
## To be copied in the server
# mod_save_server("save_ui_1")
