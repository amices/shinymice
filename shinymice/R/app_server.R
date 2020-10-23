#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # List the first level callModules here
  #output$md_pat <- renderPlot(mice::md.pattern(mice::boys))
  #callModule(mod_missingness_server, "missingness_ui_1")
  mod_missingness_server("missingness_ui_1")
  mod_home_server("home_ui_1")
}
