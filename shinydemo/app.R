#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(navbarPage(
  "Shiny demo",
  tabPanel("1.",
           h2("1. The default app"),
           # Sidebar with a slider input for number of bins
           sidebarLayout(sidebarPanel(
             sliderInput(
               "bins",
               "Number of bins:",
               min = 1,
               max = 50,
               value = 30
             )
           ),
           
           # Show a plot of the generated distribution
           mainPanel(plotOutput("distPlot")))
                           ),
  tabPanel("2.",
           h2("2. The basic mechanism"),
           #uiOutput("pdf")
           #img(src = "test.png"),
           tags$iframe(style="height:600px; width:100%; scrolling=yes",
                       src = "shiny_logic.pdf")),
  tabPanel("3.",
           h2("3. Building blocks"),
           fluidRow(
             column(6,
             tags$b("UI"),
             br(),
             br(),
             tags$code("sliderInput('bins', 'Number of bins:', min = 1, max = 50, value = 30)"),
             img(src = "slider.png", style = "width:100%"),
             tags$code("plotOutput('distPlot')"),
             img(src = "histogram.png", style = "width:100%")
             ),
             column(6, 
             tags$b("Server"),
             br(),
             br(),
             tags$code("output$distPlot <- renderPlot({"), br(),
             tags$code("x    <- faithful[, 2]"), br(),
             tags$code("bins <- seq(min(x), max(x), length.out = input$bins + 1)"), br(),
             tags$code("hist(x,
                    breaks = bins,
                    col = 'darkgray',
                    border = 'white')"), br(),
             tags$code("})")
             )
  ))
  ))

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # draw the histogram with the specified number of bins
    hist(x,
         breaks = bins,
         col = 'darkgray',
         border = 'white')
  })
  #output$pdf <- renderUI()
}

# Run the application
shinyApp(ui = ui, server = server)
