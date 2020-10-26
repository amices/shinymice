#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_home_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(
      div(img(src = "www/favicon.png", width = 300), align = "center"),#h2("[shinymice logo here]", align = "center"),
      br(),
      h2("[navigation buttons here]", align = "center"),
      h5("[link to github repo for issues]", align = "center"),
      br()
      # br(),
      # h2("[upload data here]", align = "center"),
      # br(),
      # verbatimTextOutput(ns("name"))
    )
    )
}
    
#' home Server Functions
#'
#' @noRd 
mod_home_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    #output$name <- renderText("[print name of dataset here?]")
  })
}
    
## To be copied in the UI
# mod_home_ui("home_ui_1")
    
## To be copied in the server
# mod_home_server("home_ui_1")
