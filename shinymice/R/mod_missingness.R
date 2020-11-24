#' missingness UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_missingness_ui <- function(id) {
  ns <- NS(id)
  fluidPage(fluidRow(
    column(
      4,
      tags$b("Explore the missingness"),
      br(),
      br(),
      "1. Load the incomplete data",
      fileInput(
        ns("dat"),
        label = NULL,
        buttonLabel = icon("search"),
        placeholder = ".csv/.Rdata/.txt",
        accept = c(".rdata", ".csv", "text", ".txt")
      ),
      no_br(),
      "2. Check how much missingness there is in each variable.",
      br(),
      "3. Browse the dataset to view missing data points.",
      br(),
      "4. Evaluate the bivariate relations in the incomplete data.",
      br(),
      "5. Evaluate the distribution of variables conditional on missingness in another variable.",
      br()
    ),
    column(8,
           tabsetPanel(
             tabPanel("Descriptives",
                      DT::DTOutput(ns("na_desc"))),
             tabPanel("Browse",
                      DT::DTOutput(ns("na_tab"))),
             tabPanel(
               "Scatter plot",
               select_var(ns("var1")),
               select_var(ns("var2")),
               plotOutput(ns("na_plot"))
             ),
             tabPanel(
               "Conditional distribution",
               select_var(ns("var3")),
               select_var(ns("var4")),
               plotOutput(ns("cond_plot"))
             )
           ))
  ))
}

#' missingness Server Function
#'
#' @noRd
mod_missingness_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$na_desc <- DT::renderDT(descr_NA(mice::boys))
    output$na_tab <- DT::renderDT(tab_NA(mice::boys))
    output$na_plot <-
      renderPlot({
        plot_NA_scatter(mice::boys, x = input$var1, y = input$var2)
      })
    output$cond_plot <-
      renderPlot({
        plot_NA_cond(mice::boys, x = input$var3, z = input$var4)
      })
  })
}

## To be copied in the UI
# mod_missingness_ui("missingness_ui_1")

## To be copied in the server
# callModule(mod_missingness_server, "missingness_ui_1")
