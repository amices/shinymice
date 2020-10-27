#' imputeddata UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_imputeddata_ui <- function(id){
  ns <- NS(id)
  fluidPage(fluidRow(
    column(3,
           tags$b("Inspect the imputations"),
           br(),
           br(),
           "1. Check the descriptive statistics of the imputed data.",
           br(),
           "2. Inspect the distribution of the imputed data per variable.",
           br(),
           "3. Evaluate the bivariate relations post-imputation."
    ),
    column(9,
           tabsetPanel(
             tabPanel("Descriptives",
                      verbatimTextOutput(ns("imp_desc"))),
             tabPanel("Stripplot",
                      div(style="display:inline-block; margin-left: 15px", selectInput("var", label = NULL, choices = c("Select a variable", names(mice::boys)), width = 200)),
                      plotOutput(ns("strip_plot"))),
             tabPanel("Boxplot",
                      div(style="display:inline-block; margin-left: 15px", selectInput("var", label = NULL, choices = c("Select a variable", names(mice::boys)), width = 200)),
                      plotOutput(ns("bw_plot"))),
             tabPanel("Densityplot",
                      div(style="display:inline-block; margin-left: 15px", selectInput("var", label = NULL, choices = c("Select a variable", names(mice::boys)), width = 200)),
                      plotOutput(ns("dens_plot"))),
             tabPanel("Scatterplot",
                      div(style="display:inline-block; margin-left: 15px", selectInput("var1", label = NULL, choices = c("Select a variable", names(mice::boys)), width = 200)),
                      div(style="display:inline-block; margin-left: 15px", selectInput("var2", label = NULL, choices = c("Select a second variable", names(mice::boys)), width = 200)),
                      plotOutput(ns("xy_plot")))
           ))
  ))
}
    
#' imputeddata Server Functions
#'
#' @noRd 
mod_imputeddata_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    dummy_plot <- ggplot2::ggplot(data = data.frame(x = c("dummy", "plot"), y = c(0,0))) + ggplot2::geom_point(ggplot2::aes(x=x,y=y))
    output$imp_desc <- renderPrint(summary(mice::boys))
    output$strip_plot <- renderPlot(dummy_plot)
    output$bw_plot <- renderPlot(dummy_plot)
    output$dens_plot <- renderPlot(dummy_plot)
    output$xy_plot <- renderPlot(dummy_plot)
  })
}
    
## To be copied in the UI
# mod_imputeddata_ui("imputeddata_ui_1")
    
## To be copied in the server
# mod_imputeddata_server("imputeddata_ui_1")
