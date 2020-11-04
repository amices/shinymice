#' save UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_save_ui <- function(id) {
  ns <- NS(id)
  fluidPage(fluidRow(
    column(
      3,
      tags$b("Save the data/results"),
      br(),
      br(),
      "1. Select the file type to save the imputations.",
      div(
        radioButtons(
          "file_type",
          label = NULL,
          choices = c(".Rdata", ".spv"),
          inline = TRUE
        ),
        style = "margin-bottom: -15px"
      ),
      "2. Save the imputations.",
      downloadButton("save_file", label = NULL)
    ),
    column(9,
           tabsetPanel(
             tabPanel("Some plot/image?",
                      plotOutput(ns("save_plot"))),
             tabPanel("About",
                      "[Some info about the `amices` universe, e.g. link to Github again and FIMD: https://stefvanbuuren.name/fimd/.]")
           ))
  ))
}

#' save Server Functions
#'
#' @noRd
mod_save_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$save_plot <- renderPlot(dummy_plot())
  })
}

## To be copied in the UI
# mod_save_ui("save_ui_1")

## To be copied in the server
# mod_save_server("save_ui_1")
