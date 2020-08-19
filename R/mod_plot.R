plotUI <- function(id) {
  fluidRow(column(3,
    varSelectInput(NS(id, "var1"), "Variable", data = mice::boys),
    varSelectInput(NS(id, "var2"), "Variable", data = mice::boys)),
  column(9,
    plotlyOutput(NS(id, "plot")))
  )
}

plotServer <- function(id) {
  moduleServer(id, function(input, output, session, data = boys, vars = names(boys), plottype = "NAplot") {
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
      #ggplot(data) + geom_histogram(aes(x = .data[[input$var1]]))
    })
  })
}

histogramApp <- function() {
  ui <- fluidPage(
    plotUI("hist1")
  )
  server <- function(input, output, session) {
    plotServer("hist1")
  }
  shinyApp(ui, server)  
}
