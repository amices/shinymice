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
      "4. Evaluate the distribution of variables conditional on missingness in another variable.",
      br(),
      "5. Evaluate the bivariate relations in the incomplete data.",
      br(),
      "6. Look at the missing data pattern for an overview of the missingness.",
      br()
    ),
    column(
      9,
      tabsetPanel(
        tabPanel("Descriptives",
                 tableOutput(ns("na_desc"))),
        tabPanel("Browse",
                 DT::DTOutput(ns("na_tab"))),
        
        tabPanel(
          "Conditional distributions",
          select_var(ns("var1")),
          select_var(ns("var2")),
          plotOutput(ns("cond_plot"))
        ),
        tabPanel(
          "Bivariate distributions",
          select_var(ns("var1")),
          select_var(ns("var2")),
          plotOutput(ns("na_plot"))
        ),
        tabPanel("Missingness pattern",
                 plotOutput(ns("md_pat")))
      )
    )
  ))
}

#' missingness Server Function
#'
#' @noRd
mod_missingness_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    p <- dummy_plot()
    output$na_tab <- DT::renderDT(mice::boys)
    output$na_desc <- renderTable(mis_descr(mice::boys))
    output$md_pat <- renderPlot(p)
    x = "age"
    y = "hc"
    z = "hc"
    output$cond_plot <-
      renderPlot({
        plot_conditional(mice::boys, x , z) + ggplot2::ggtitle(
          paste0(
            "Distribution of '",
            x,
            "' conditional on missingness in '",
            z,
            "' (select above)"
          )
        )
      })
    output$na_plot <-
      renderPlot({
        plot_bivariate(mice::boys, x, y) +
          list(ggplot2::labs(
            title = paste0("Scatter plot of '",
                           y,
                           "' against '",
                           x,
                           "' (select above)")
          ))
      })
  })
}

## To be copied in the UI
# mod_missingness_ui("missingness_ui_1")

## To be copied in the server
# callModule(mod_missingness_server, "missingness_ui_1")
