#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_home_ui <- function(id) {
  ns <- NS(id)
  fluidPage(fluidRow(
    div(img(src = "www/favicon.png", width = 300), align = "center"),
    #h2("[shinymice logo here]", align = "center"),
    br(),
    br(),
    #h2("[navigation buttons here]", align = "center"),
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

#' home Server Functions
#'
#' @noRd
mod_home_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_home_ui("home_ui_1")

## To be copied in the server
# mod_home_server("home_ui_1")
