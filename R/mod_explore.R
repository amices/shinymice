exploreUI <- function(id, dat) {
  fluidRow(
    column(
      4,
      h2("Explore missingness"),
      br(),
      varSelectInput(NS(id, "NA_var1"), "Distribution of", data = mice::boys),
      varSelectInput(NS(id, "NA_var2"), "given (missingness in)", data = mice::boys),
      helpText("Hint: the strongest relations are with observed data in"),
      div(textOutput(NS(id, "relations")), style = "margin-top:-1em;"),
      helpText("Hint: the strongest relations are with missingness in"),
      div(textOutput(NS(id, "NA_relations")), style = "margin-top:-1em;"),
      
    ),
    column(
      8,
      tabsetPanel(
        tabPanel("Pattern",
                 plotOutput(NS(id, "md_pattern"), height = "520px")),
        tabPanel("Scatterplot",
                 plotlyOutput(NS(id, "NA_scat"), height = "520px")),
        tabPanel("Histogram",
                 plotlyOutput(NS(id, "NA_hist"), height = "520px"))
      ),
      br(),
      tags$b("Additional options for histograms"),
      splitLayout(
        cellWidths = c("30%", "70%"),
        numericInput(
          NS(id, "bins"),
          "",
          min = 0,
          step = 0.5,
          value = 0,
          width = 70
        ),
        div(br(), br(), "Binwidth (default when 0)")
      ),
      checkboxInput(NS(id, "scalehist"), "Fixed heigth y-axis", value = TRUE)
    )
  )
}

exploreServer <- function(id, dat) {
  moduleServer(id, function(input, output, session, data = dat, vars = names(dat), plottype = "NAplot") {
    # plot pattern
    # make it interactive with two axes? see https://stackoverflow.com/questions/52833214/adding-second-y-axis-on-ggplotly
    data <- reactive(data)
    output$md_pattern <-
      renderPlot({
        #md_plot <-
        plot_md_pattern(data = data())
      }, res = 72)
    
    varsUpdate <-
      function(UI_name) {
        updateSelectInput(session, UI_name, choices = vars())
      }
    
    # show correct variables
    observe(varsUpdate("NA_var1"))
    observe(varsUpdate("NA_var2"))
    
    # show best predictors
    output$relations <-
      renderText(test_NA_y(data(), x = input$NA_var1)$top3)
    output$NA_relations <-
      renderText(test_predictors(data(), x = input$NA_var1))
    # plot distributions
    # make plots the correct heigth every time, see https://stackoverflow.com/questions/34792998/shiny-variable-height-of-renderplot
    output$NA_hist <- renderPlotly({
      conditional_hist(
        dat = data(),
        x = input$NA_var1,
        y = input$NA_var2,
        scaler = input$scalehist,
        binner = input$bins
      )
    })
    output$NA_scat <- renderPlotly({
      plot_NA_margins(data = data(),
                      x = input$NA_var2,
                      y = input$NA_var1)
    })
  })
}

# test the module
source("R/fnc_plot.md.pattern.R")
source("R/fnc_test.relations.R")
testApp <- function() {
  ui <- fluidPage(
    exploreUI("hist1", dat = mice::boys)
  )
  server <- function(input, output, session) {
    exploreServer("hist1", dat = mice::boys)
  }
  shinyApp(ui, server)
}
