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
      4,
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
    column(8,
           tabsetPanel(
             tabPanel("About",
                      br(),
                      div(img(src = "www/logo.png", width = 300), align = "center"),
                      br(),
                      "This app is part of the `amices` universe ('a home for the growing MICE family'). Amices is a place for people interested in solving missing data problems. Find us online on ",
                      tags$a(href = "https://github.com/amices", "GitHub"),
                      " or our ",
                      tags$a(href = "https://amices.org", "website"),
                      ". For an introduction to missing data and imputation methods, see the open access book 'Flexible Imputation of Missing Data'",
                      tags$a(href = "https://stefvanbuuren.name/fimd/", "(van Buuren, 2018)"),
                      ".")
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
