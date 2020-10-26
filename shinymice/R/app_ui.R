#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(# Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    fluidPage(navbarPage(
      title = "shinymice",
      tabPanel("Home",
               mod_home_ui("home_ui_1")),
      tabPanel("Missing data",
               mod_missingness_ui("missingness_ui_1")),
      tabPanel("Imputation model",
               mod_imputationmodel_ui("imputationmodel_ui_1")),
      tabPanel("Imputed data",
               mod_imputeddata_ui("imputeddata_ui_1")),
      tabPanel("Analyze the imputed data",
               mod_scientificmodel_ui("scientificmodel_ui_1")),
      tabPanel("Exit",
               mod_save_ui("save_ui_1"))
    )))
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  add_resource_path(
    'www', app_sys('app/www')
  )
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'shinymice'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

