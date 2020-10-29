#' imputeddata UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_imputeddata_ui <- function(id) {
  ns <- NS(id)
  fluidPage(fluidRow(
    column(
      3,
      tags$b("Inspect the imputations"),
      br(),
      br(),
      "1. Check the descriptive statistics of the imputed data.",
      br(),
      "2. Inspect the distribution of the imputed data per variable.",
      br(),
      "3. Evaluate the bivariate relations post-imputation."
    ),
    column(
      9,
      tabsetPanel(
        tabPanel("Descriptives",
                 tableOutput(ns("imp_desc"))),
        tabPanel(
          "Stripplot",
          select_var(ns("var1")),
          plotOutput(ns("strip_plot"))
        ),
        tabPanel(
          "Boxplot",
          select_var(ns("var1")),
          plotOutput(ns("bw_plot"))
        ),
        tabPanel(
          "Densityplot",
           select_var(ns("var1")),
           plotOutput(ns("dens_plot"))
        ),
        tabPanel(
          "Scatterplot",
          select_var(ns("var1")),
          select_var(ns("var2")),
          plotOutput(ns("xy_plot"))
        )
      )
    )
  ))
}

#' imputeddata Server Functions
#'
#' @noRd
mod_imputeddata_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    p <- dummy_plot()
    output$imp_desc <- renderTable(imp_descr(mice::mice(mice::boys, maxit = 2)))
    output$strip_plot <- renderPlot(p)
    output$bw_plot <- renderPlot(p)
    output$dens_plot <- renderPlot(p)
    output$xy_plot <- renderPlot(p)
  })
}

## To be copied in the UI
# mod_imputeddata_ui("imputeddata_ui_1")

## To be copied in the server
# mod_imputeddata_server("imputeddata_ui_1")
