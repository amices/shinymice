#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(# Leave this function for adding external resources
    golem_add_external_resources(),
    fluidPage(
      navbarPage(
        windowTitle = "shinymice",
        title = div(img(src = "www/logo.png", width = 120)),
        collapsible = TRUE,
        header = div(br(), br()),
        tabPanel(
          "Home",
          icon = icon("home"),
          mod_00_home_ui("home_00_ui_1")),
        tabPanel(
          "Incomplete data",
          icon = icon("file-upload"),
          mod_01_data_ui("01_data_ui_1")
        ),
        tabPanel(
          "Imputation model",
          icon = icon("sliders-h"),
          mod_02_model_ui("02_model_ui_1")
        ),
        tabPanel(
          "Algorithmic convergence",
          icon = icon("chart-line"),
          mod_03_convergence_ui("03_convergence_ui_1")
          ),
        tabPanel(
          "Imputed data",
          icon = icon("chart-area"),
          mod_04_imputations_ui("04_imputations_ui_1")
        ),
        tabPanel("Save",
                 icon = icon("file-download"),
                 mod_05_save_ui("05_save_ui_1")
        )
    ))
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path('www', app_sys('app/www'))
  tags$head(favicon(ext = 'png'),
            bundle_resources(path = app_sys('app/www'),
                             app_title = 'shinymice'),
            # Add here other external resources
            # for example, you can add shinyalert::useShinyalert() 
            shinyjs::useShinyjs(),
            waiter::use_waiter()
            # shinyFeedback::useShinyFeedback(),
            )
            }
