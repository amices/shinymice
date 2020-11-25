#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # List the first level callModules here
  mod_home_server("home_ui_1")
  data <- mod_missingness_server("missingness_ui_1")
  imp <- mod_imputationmodel_server("imputationmodel_ui_1", data)
  mod_imputeddata_server("imputeddata_ui_1", data, imp)
  #mod_scientificmodel_server("scientificmodel_ui_1")
  mod_save_server("save_ui_1", imp)
}
