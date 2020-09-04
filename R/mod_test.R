plotUI <- function(id, dat) {
  fluidRow(column(3,
    varSelectInput(NS(id, "var1"), "Variable", data = dat),
    varSelectInput(NS(id, "var2"), "Variable", data = dat)),
  column(9,
    plotlyOutput(NS(id, "plot")))
  )
}

plotServer <- function(id, dat) {
  moduleServer(id, function(input, output, session, data = dat, vars = names(dat), plottype = "NAplot") {
    varsUpdate <-
      function(UI_name) {
        updateSelectInput(session, UI_name, choices = vars)
      }
    observe(varsUpdate("var1"))
    observe(varsUpdate("var2"))
    
    
    #data <- reactive(boys[[input$var1]])
    output$plot <- renderPlotly({
      if(plottype == "NAplot") {
        p <- plot_NA_margins(
          data = data,
          x = input$var2,
          y = input$var1)
      } 
      p
    })
  })
}

# # test the module
# source("R/fnc_plot.NA.data.R")
# testApp <- function() {
#   ui <- fluidPage(
#     plotUI("hist1", dat = mice::boys)
#   )
#   server <- function(input, output, session) {
#     plotServer("hist1", dat = mice::boys)
#   }
#   shinyApp(ui, server)  
# }
