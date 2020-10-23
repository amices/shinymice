#' missingness UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_missingness_ui <- function(id){
  ns <- NS(id)
  #tagList(
  fluidPage(
    fluidRow(
      column(3, 
             "Select variable(s)",
             selectInput("var", "Variable", choices = names(mice::boys))
             ),
      column(9,
             tabsetPanel(
               tabPanel("Dataset",
                        DT::DTOutput(ns("na_tab"))),
               tabPanel("Descriptives",
                        tableOutput(ns("na_desc"))),
               tabPanel("Pattern",
             plotOutput(ns("md_pat"))))
             )
      )
    )
}
    
#' missingness Server Function
#'
#' @noRd 
mod_missingness_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$na_tab <- DT::renderDT(mice::boys)
    output$na_desc <- renderTable(summary(mice::boys))
    output$md_pat <- renderPlot(mice::md.pattern(mice::boys))
  })
}  

## To be copied in the UI
# mod_missingness_ui("missingness_ui_1")
    
## To be copied in the server
# callModule(mod_missingness_server, "missingness_ui_1")
 
