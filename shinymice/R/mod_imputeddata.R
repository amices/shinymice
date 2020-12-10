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
      4,
      tags$b("Inspect the imputations"),
      br(),
      br(),
      "1. Please make sure you have imputed the incomplete data.",
      br(),
      "2. Check the descriptive statistics of the imputed data.",
      br(),
      "3. Inspect the distribution of the imputed data per variable.",
      br(),
      "4. Evaluate the bivariate relations post-imputation."
    ),
    column(
      8,
      tabsetPanel(
        tabPanel("Descriptives",
                 DT::DTOutput(ns("imp_desc"))),
        tabPanel("Data points",
                 select_var(ns("var1")),
                 plotOutput(ns("strip_plot"))),
        tabPanel("Distribution",
                 select_var(ns("var2")),
                 plotOutput(ns("bw_plot"))),
        tabPanel("Density",
                 select_var(ns("var3")),
                 plotOutput(ns("dens_plot"))),
        tabPanel(
          "Scatter plot",
          select_var(ns("var4")),
          select_var(ns("var5")),
          plotOutput(ns("xy_plot"))
        )
      )
    )
  ))
}

#' imputeddata Server Functions
#'
#' @noRd
mod_imputeddata_server <- function(id, data, imp) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    stopifnot(is.reactive(imp))
    observe(purrr::map(paste0("var", 1:5), function(x) {
      updateSelectInput(session, x, choices = names(data()))
    }))
    # observe(showModal(modalDialog(
    #   title = "Important message",
    #   "This is an important message!"
    # )))
    output$imp_desc <- DT::renderDT({
      if(is.null(imp())){return(data.frame(x=0, y=0))} else{
      imp_descr(imp())}
    })
    output$strip_plot <-
      renderPlot(plot_strip(imp(), x = input$var1))
    output$bw_plot <- renderPlot(plot_bw(imp(), x = input$var2))
    output$dens_plot <- renderPlot(plot_dens(imp(), x = input$var3))
    output$xy_plot <-
      renderPlot(plot_xy(imp(), x = input$var4, y = input$var5))
  })
}

## To be copied in the UI
# mod_imputeddata_ui("imputeddata_ui_1")

## To be copied in the server
# mod_imputeddata_server("imputeddata_ui_1")
