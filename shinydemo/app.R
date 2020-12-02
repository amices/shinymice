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
  "SHINY101",
  tabPanel("1.",
           h2("1. The default app"),
           "The result of navigating to 'File' >> 'New File' >> 'Shiny Web App' in RStudio.", br(), br(),
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
           "From the 'Introduction to Shiny'",
           tags$a("webinar.", href = "https://rstudio.com/resources/webinars/introduction-to-shiny/"), br(), br(),
           #uiOutput("pdf")
           #img(src = "test.png"),
           tags$iframe(style="height:600px; width:100%; scrolling=yes",
                       src = "shiny_logic.pdf")),
  tabPanel("3.",
           h2("3. The building blocks"),
           "See also the",
           tags$a("Shiny cheatsheet.", href= "https://rstudio.com/resources/cheatsheets/"), br(), br(),
           fluidRow(
             column(5,
             tags$b("Shiny UI"),
             br(),
             br(),
             tags$code("sliderInput('bins', 'Number of bins:', min = 1, max = 50, value = 30)"),
             img(src = "slider.png", style = "width:100%"), br(), br(),
             tags$code("plotOutput('distPlot')"),
             img(src = "histogram.png", style = "width:100%")
             ),
             column(1, " "),
             column(5, 
             tags$b("Shiny server"),
             br(),
             br(),
             tags$code("output$distPlot <- renderPlot({"), br(), br(),
             tags$code("x    <- faithful[, 2]"), br(), br(),
             tags$code("bins <- seq(min(x), max(x), length.out = input$bins + 1)"), br(), br(),
             tags$code("hist(x,
                    breaks = bins,
                    col = 'darkgray',
                    border = 'white')"), br(), br(),
             tags$code("})")
             )
  )),
  tabPanel("4.", 
           h2("4. Some examples"),
           "For an overview, see the ",
           tags$a("Shiny gallery", href = "https://shiny.rstudio.com/gallery/"), 
           "and the",
           tags$a("Shiny contest winners.", href = "https://blog.rstudio.com/2020/07/13/winners-of-the-2nd-shiny-contest/"), br(),
           br(), br(),
           img(style = "width:50%", src = "normal.PNG"),
           "Let's start very simple with a",
           tags$a("normal distribution app.", href = "http://teaching.sociology.ul.ie:3838/apps/normsd/"),
           br(), br(),
           img(style = "width:50%", src = "outlier.PNG"),
           "Or an app to investigate the effect of",
           tags$a("outliers.", href = "http://teaching.sociology.ul.ie:3838/influence/"),
           br(), br(),
           img(style = "width:50%", src = "hex.PNG"),
           "Now, something more interesting, a ",
           tags$a("hex sticker designer.", href = "https://connect.thinkr.fr/hexmake/")
           ),
  tabPanel("5.", 
           h2("5. How to"),
           "According to",
           tags$a("Engineering Shiny", href = "https://engineering-shiny.org/"),
           "and yours truly.", br(), br(),
           tabsetPanel(
           tabPanel(tags$b("STEP 1: DESIGN"),
           br(), 
           # "Unless you are just coding for the joy of coding, there will always be one or more end users. And if these people cannot use the application because it is too hard to use, too hard to understand, because it is too slow or there is no inherent logic in how the user experience is designed, then it is inappropriate to call the app a success.",
           "Don't rush into coding when you should be thinking.", br(), br(),
           "You should KISS:",
           tags$div(
             tags$ul(
               tags$li("Avoid unnecessary complexity and 'feature creep'."),
               tags$li("UI first approach."),
               tags$li("Rmd first approach.")
               # "Modules aim at three things: simplifying "id" namespacing, split the code base into a series of functions, and allow UI/Server parts of your app to be reused",
             )
           ), 
           # "Most systems work best if they are kept simple rather than made complicated; therefore, simplicity should be a key goal in design, and unnecessary complexity should be avoided",
           "Ask yourself:",
           tags$div(
             tags$ul(
               tags$li("Who are the end users of your app?"),
               tags$li("Are they tech-literate?"),
               tags$li("In which context will they be using your app?"),
               tags$li("On what machines? E.g., will they be using the app on their office desktop PC, on their phone while driving a tractor, etc.?")
             )
           )), 
           tabPanel(tags$b("STEP 2: PROTOTYPE"), br(), 
           "Build the front-end and the back-end, but separately.", br(), br(),
           "Front-end:",
           tags$div(
             tags$ul(
               tags$li("Work on the general appearance, e.g. tabs, inputs, outputs; anything that does not rely on computation."),
               tags$li("Use mock data and/or text (build an 'ipsum-app')."),
               tags$li("Make the app self-evident; if possible, the main usage of the application does not require reading the manual.")
             )),
           "Back-end:",
           tags$div(
             tags$ul(
               tags$li("Extract your core 'non-reactive' functions."),
               tags$li("Split your app into modules."),
               tags$li("Reactivity vs. speed trade-off.")
             ))),
           tabPanel(tags$b("STEP 3: BUILD"), br(),
           "Something about building, like documentation.", br(), br(), br()),
           tabPanel(tags$b("STEP 4: STRENGTHEN"), br(), 
           "Something about testing and dependencies.", br(), br(), br()),
           tabPanel(tags$b("STEP 5: DEPLOY"), br(), 
           "Something about making it a package, using shinyapps.io, etc.",  br(), br()
  )))
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
