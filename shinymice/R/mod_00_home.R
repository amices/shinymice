#' 00_home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_00_home_ui <- function(id){
  ns <- NS(id)
  fluidPage(fluidRow(
    div(img(src = "www/favicon.png", width = 300), align = "center"),
    br(),
    br(),
    div(tags$b("Dear reader,"), align = "center"),
    div(tags$b("This app is still under development."),
        align = "center"),
    div(
      tags$b("Please feel free to explore it anyway, but beware of bugs."),
      align = "center"
    ),
    div(tags$b("-Hanne"), align = "center"),
    br(),
    br(),
    div(
      "For the most recent version of this app or to report an issue, see ",
      tags$a(href = "https://github.com/amices/shinyMice", "github.com/amices/shinyMice"),
      ".",
      align = "center"
    ),
    br()
  ))
}
    
#' 00_home Server Functions
#'
#' @noRd 
mod_00_home_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_00_home_ui("00_home_ui_1")
    
## To be copied in the server
# mod_00_home_server("00_home_ui_1")
