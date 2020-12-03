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
ui <- fluidPage(
  navbarPage(
    "SHINY101",
    tabPanel("0.",
             img(src = "SHINY101.png", style = "width:800")),
    tabPanel(
      "1.",
      h2("1. The default app"),
      "The result of navigating to 'File' >> 'New File' >> 'Shiny Web App' in RStudio.",
      br(),
      br(),
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
    #segway: should you choose 1 or 2 file app??
    tabPanel(
      "2.",
      h2("2. The basic mechanism"),
      "From the 'Introduction to Shiny'",
      tags$a("webinar.", href = "https://rstudio.com/resources/webinars/introduction-to-shiny/"),
      br(),
      br(),
      #uiOutput("pdf")
      #img(src = "test.png"),
      tags$iframe(style = "height:600px; width:100%; scrolling=yes",
                  src = "shiny_logic.pdf")
    ),
    tabPanel(
      "3.",
      h2("3. The building blocks"),
      "See also the",
      tags$a("Shiny cheatsheet.", href = "https://rstudio.com/resources/cheatsheets/"),
      br(),
      br(),
      fluidRow(
        column(
          5,
          tags$b("Shiny UI"),
          br(),
          br(),
          tags$code("ui <- fluidPage("),
          br(),
          br(),
          tags$code(
            "sliderInput('bins', 'Number of bins:', min = 1, max = 50, value = 30)"
          ),
          img(src = "slider.png", style = "width:100%"),
          br(),
          br(),
          tags$code("plotOutput('distPlot')"),
          img(src = "histogram.png", style = "width:100%"),
          tags$code(")")
        ),
        column(1, " "),
        column(
          5,
          tags$b("Shiny server"),
          br(),
          br(),
          tags$code("server <- function(input, output) {"),
          br(),
          br(),
          tags$code("output$distPlot <- renderPlot({"),
          br(),
          br(),
          tags$code("x    <- faithful[, 2]"),
          br(),
          br(),
          tags$code("bins <- seq(min(x), max(x), length.out = input$bins + 1)"),
          br(),
          br(),
          tags$code("hist(x,
                    breaks = bins,
                    col = 'darkgray',
                    border = 'white')"),
          br(),
          br(),
          tags$code("})"),
          br(),
          br(),
          tags$code("}")
        )
      )
    ),
    tabPanel(
      "4.",
      h2("4. Some examples"),
      "For an overview, see the ",
      tags$a("Shiny gallery", href = "https://shiny.rstudio.com/gallery/"),
      "and the",
      tags$a("Shiny contest winners.", href = "https://blog.rstudio.com/2020/07/13/winners-of-the-2nd-shiny-contest/"),
      br(),
      br(),
      br(),
      img(style = "width:50%", src = "normal.PNG"),
      "Let's start very simple with a",
      tags$a("normal distribution app.", href = "http://teaching.sociology.ul.ie:3838/apps/normsd/"),
      br(),
      br(),
      img(style = "width:50%", src = "outlier.PNG"),
      "Or an app to investigate the effect of",
      tags$a("outliers.", href = "http://teaching.sociology.ul.ie:3838/influence/"),
      br(),
      br(),
      img(style = "width:50%", src = "hex.PNG"),
      "Now, something more interesting, a ",
      tags$a("hex sticker designer.", href = "https://connect.thinkr.fr/hexmake/")
    ),
    tabPanel(
      "5.",
      h2("5. How to"),
      "According to",
      tags$a("Engineering Shiny", href = "https://engineering-shiny.org/"),
      "and yours truly.",
      br(),
      br(),
      tabsetPanel(
        tabPanel(
          tags$b("STEP 1: DESIGN"),
          br(),
          # "Unless you are just coding for the joy of coding, there will always be one or more end users. And if these people cannot use the application because it is too hard to use, too hard to understand, because it is too slow or there is no inherent logic in how the user experience is designed, then it is inappropriate to call the app a success.",
          "Don't rush into coding when you should be thinking.",
          br(),
          br(),
          "You should KISS (i.e., Keep It Simple, Stupid):",
          tags$div(tags$ul(
            tags$li("Avoid unnecessary complexity and 'feature creep'."),
            tags$li("UI first approach."),
            tags$li("Rmd first approach.")
          )),
          # "Most systems work best if they are kept simple rather than made complicated; therefore, simplicity should be a key goal in design, and unnecessary complexity should be avoided",
          "Ask yourself:",
          tags$div(tags$ul(
            tags$li("Who are the end users of your app?"),
            tags$li("Are they tech-literate?"),
            tags$li("In which context will they be using your app?"),
            tags$li(
              "On what machines? E.g., will they be using the app on their office desktop PC, on their phone while driving a tractor, etc.?"
            )
          ))
        ),
        tabPanel(
          tags$b("STEP 2: PROTOTYPE"),
          br(),
          "Build the front-end and the back-end, but separately.",
          br(),
          br(),
          "Front-end:",
          tags$div(tags$ul(
            tags$li(
              "Work on the general appearance, e.g. tabs, inputs, outputs; anything that does not rely on computation."
            ),
            tags$li("Use mock data and/or text (build an 'ipsum-app')."),
            tags$li(
              "Make the app self-evident; the main usage of the app should not require reading the manual."
            )
          )),
          "Back-end:",
          tags$div(tags$ul(
            tags$li(
              "Think about what could to be 'hard coded', because of the reactivity vs. speed trade-off."
            ),
            tags$li(
              "Extract your core 'non-reactive' functions and put them in separate files."
            ),
            tags$li(
              "Use sensible non-reactive defaults while develioping (e.g., `data <- mtcars` instead of `data <- reactive(...).`"
            ),
            tags$li("OPTIONAL: Split your app into modules.")
          ))
        ),
        tabPanel(
          "DETOUR: MODULES",
          br(),
          "Something about modules.",
          br(),
          br(),
          "What are modules? Definition from",
          tags$a("RStudio:", href = "https://shiny.rstudio.com/articles/modules.html"),
          br(),
          br(),
          img(src = "module.PNG", style = "width:100%"),
          br(),
          "Modules aim at three things: simplifying 'id' namespacing, split the code base into a series of functions, and allow UI/Server parts of your app to be reused.",
          "If you want to learn more, watch ",
          tags$a("this presentation.", href = "https://www.youtube.com/watch?v=ylLLVo2VL50"),
          br()
        ),
        tabPanel(
          tags$b("STEP 3: BUILD"),
          br(),
          "Now that you have a rough sketch of the app, and the 'dumb' (i.e., non-reactive) functions to perform data processing and visualization, you need to link the front-end and back-end together.",
          br(),
          br(),
          "Things to think about:",
          tags$div(tags$ul(
            tags$li(
              "Package dependencies (e.g., using `library()` vs. making your app a package)."
            ),
            tags$li(
              "Documentation (e.g., of functions and UX elements; maybe even write a vignette)."
            ),
            tags$li(
              "Combine server-side requirements with user feedback (such as input validation, pop-up messages, or loading icons)."
            ),
            tags$li(
              "If you're not sure if a reactive is working, just print the output (i.e., `renderPrint(...)`)."
            )
          ))
        ),
        tabPanel(
          tags$b("STEP 4: STRENGTHEN"),
          br(),
          "If you want your Shiny app to last, you should make it robust.",
          br(),
          br(),
          "At least:",
          tags$div(tags$ul(
            tags$li(
              "Run the app in the viewer panel, a separate window, and your browser."
            ),
            tags$li("Monkey test it (i.e., click EVERYTHING)."),
            tags$li(
              "Provide the wrong inputs (e.g., a corrupt data file, a file with the 'wrong' extension, an 'impossible' numeric input, etc.)"
            )
          )),
          "If you feel like being fancy, also:",
          tags$div(tags$ul(
            tags$li("Add 'real' tests with the {thatthat} and {shinytest} packages."),
            tags$li(
              "Optimize certain computations that are flagged by the {shinyloadtest} package."
            ),
            tags$li(
              "Make the app future-proof with a reproducible environment like {renv} or a Docker container."
            )
          ))
        ),
        tabPanel(
          tags$b("STEP 5: DEPLOY"),
          br(),
          "If you want your app to 'live' on a webpage, you should deploy it. For example on",
          tags$a("shinyapps.io.", href = "https://www.shinyapps.io/"),
          br(),
          br(),
          "Note that:",
          tags$div(tags$ul(
            tags$li("With a free account, your app will be public."),
            tags$li(
              "If your app is too popular, you will need to pay (for an upgrade that is, not automatically)."
            ),
            tags$li(
              "You can tweak your app to cache certain outputs, or have several users in one session (like Google Drive documents)."
            ),
            tags$li(
              "You could also host your app on your own website. Or don't deploy it at all (e.g., for privacy reasons)."
            )
          ))
        )
      )
    ),
    tabPanel(
      "6.",
      h2("6. Take", HTML("<strike>home</strike>"), "away"),
      "So, what should you remember from all this?",
      "First of all:",
      tags$div(tags$ul(
        tags$li("Google is your friend*."),
        tags$li("Don't forget to add 'r' to your 'shiny ...' searches."),
        tags$li(
          "Keep your `id`s simple and sensible. If you use modules, add the namespace function to each `id`."
        ),
        tags$li("If you copy something just ONCE, make it a function/module."),
        tags$li(
          "You will now be able to create silly apps for friends/loved ones in a matter of minutes, (ab)use that power!"
        )
      )),
      "Furthermore, you could use the {golem} framework to guide you through these steps, but decide to do this up front otherwise you'll end up with 'Frankencode'.",
      br(), "I wouldn't use it for this course, because there is quite some terminology/setting up to get through and I don't think it's worth it.",
      br(), "If you choose to invest some time in a new package next to {shiny}, I'd advice {datatable} or {plotly}.",
      br(), br(), br(),
      h6("*Debatable.")
    )
  )
)

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
