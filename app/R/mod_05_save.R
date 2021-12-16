#' 05_save UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_05_save_ui <- function(id){
  ns <- NS(id)
  fluidPage(fluidRow(
    column(
      4,
      tags$b("Save the data/results"),
      br(),
      br(),
      "1. Please make sure to impute the incomplete data first (see 'Imputation model').",
      br(),
      "2. Select the file type to save the imputations [this feature is currently disabeled].",
      div(radioButtons(
        ns("file_type"),
        label = NULL,
        choices = c(".RData", ".sav"),
        inline = TRUE
      ),
      style = "margin-bottom: -15px"),
      "3. Save the imputations (button disabled until data is imputed).",
      br(),
      shinyjs::disabled(downloadButton(ns("save_file"))),
      br()
    ),
    column(8,
           tabsetPanel(
             tabPanel(
               "About",
               br(),
               div(img(src = "www/logo.png", width = 300), align = "center"),
               br(),
               "This app is part of the `amices` universe ('a home for the growing MICE family'). Amices is a place for people interested in solving missing data problems. Find us online on ",
               tags$a(href = "https://github.com/amices", "GitHub"),
               " or our ",
               tags$a(href = "https://amices.org", "website"),
               ". For an introduction to missing data and imputation methods, see the open access book 'Flexible Imputation of Missing Data'",
               tags$a(href = "https://stefvanbuuren.name/fimd/", "(van Buuren, 2018)"),
               "."
             )
           ))
  ))
}
    
#' 05_save Server Functions
#'
#' @noRd 
mod_05_save_server <- function(id, imp){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    stopifnot(is.reactive(imp))
    observe(if (is.list(imp())) {
      shinyjs::enable("save_file")
    })
    output$save_file <- downloadHandler(
      filename = function() {
        paste0("imp", isolate(input$file_type))
      },
      content = function(file) {
        mids_object <- imp()
        if (input$file_type == ".RData") {
          save(mids_object, file = file)
        } else {
          mids2spss(imp = mids_object, file = file)
        }
      }
    )
  })
}
    
## To be copied in the UI
# mod_05_save_ui("05_save_ui_1")
    
## To be copied in the server
# mod_05_save_server("05_save_ui_1", imp)
