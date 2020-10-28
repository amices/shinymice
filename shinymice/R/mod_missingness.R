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
  #tagList(
  fluidPage(fluidRow(
    column(
      3,
      tags$b("Explore the missingness"),
      br(),
      br(),
      "1. Load the incomplete data",
      div(
        fileInput(
          ns("dat"),
          label = NULL,
          buttonLabel = icon("search"),
          placeholder = ".csv/.Rdata/.txt",
          accept = c(".rdata", ".csv", "text", ".txt")
        ),
        style = "margin-bottom: -15px"
      ),
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
             tabPanel("Pattern",
                      plotOutput(ns("md_pat"))),
             tabPanel(
               "Distribution",
               div(
                 style = "display:inline-block; margin-left: 20px",
                 selectInput(
                   "var1",
                   label = NULL,
                   choices = c("Select a variable", names(mice::boys)),
                   width = 200
                 )
               ),
               div(
                 style = "display:inline-block; margin-left: 20px",
                 selectInput(
                   "var2",
                   label = NULL,
                   choices = c("Select a second variable", names(mice::boys)),
                   width = 200
                 )
               ),
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
    dummy_plot <-
      ggplot2::ggplot(data = data.frame(x = c("dummy", "plot"), y = c(0, 0))) +
      ggplot2::geom_point(ggplot2::aes(x = x, y = y))
    output$na_tab <- DT::renderDT(mice::boys)
    output$na_desc <- renderTable(descr(mice::boys), rownames = TRUE)
    output$md_pat <- renderPlot(dummy_plot)
    output$na_plot <- renderPlot(dummy_plot)
  })
}

## To be copied in the UI
# mod_missingness_ui("missingness_ui_1")

## To be copied in the server
# callModule(mod_missingness_server, "missingness_ui_1")
