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
      3,
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
      "4. Look at the missing data pattern for an overview of the missingness.",
      br(),
      "5. Evaluate the bivariate relations in the incomplete data.",
      br()
    ),
    column(9,
           tabsetPanel(
             tabPanel("Descriptives",
                      tableOutput(ns("na_desc"))),
             tabPanel("Browse",
                      DT::DTOutput(ns("na_tab"))),
             tabPanel("Missingness pattern",
                      plotOutput(ns("md_pat"))),
             tabPanel(
               "Distributions",
               select_var(ns("var1")),
               select_var(ns("var2")),
               plotOutput(ns("na_plot"))
             )
           ))
  ))
}

#' missingness Server Function
#'
#' @noRd
mod_missingness_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    p <- dummy_plot()
    output$na_tab <- DT::renderDT(cbind(mice::boys, mice::boys))
    output$na_desc <- renderTable(mis_descr(mice::boys))
    output$md_pat <- renderPlot(p)
    output$na_plot <- renderPlot(p +
                                   list(ggplot2::labs(
                                     title = paste0(
                                       "We'll eventually plot variables '",
                                       input$var1,
                                       "' and '",
                                       input$var2,
                                       "'"
                                     )
                                   )))
  })
}

## To be copied in the UI
# mod_missingness_ui("missingness_ui_1")

## To be copied in the server
# callModule(mod_missingness_server, "missingness_ui_1")
