#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # switch_tabs <- function(to_tab, ...){updateNavbarPage(session, inputId = "testid", selected = to_tab)}
  # observeEvent(input$test2, switch_tabs("01"))
  mod_00_home_server("00_home_ui_1", parent_session = session)
  data <- mod_01_data_server("01_data_ui_1")
  imp <- mod_02_model_server("02_model_ui_1", data)
  mod_03_convergence_server("03_convergence_ui_1", data, imp)
  mod_04_imputations_server("04_imputations_ui_1", data, imp)
  mod_05_save_server("05_save_ui_1", imp)
  }
