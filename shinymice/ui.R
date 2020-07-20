#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(navbarPage(
    "shinymice",
    #sets basic virtual structure (layout function)
    tabPanel("Data",
             sidebarLayout(
                 sidebarPanel(
                     shinyjs::useShinyjs(),
                     id = "sidebar",
                     fileInput("upload", label = h3("Upload CSV file..."), 
                               accept = c(
                                   "text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv")
                     ),
                     checkboxInput("header", h6("Un-check this box if the CSV file does not have headers"), TRUE),
                     selectInput(
                         #is the input of the app that the user interacts with
                         "choice",
                         label = h3("...or use `mice` data"),
                         choices = data(package = "mice")$results[-c(5,7, 17, 18), "Item"]
                     ),
                     actionButton("reset", "Reset"),
                 ),
                 mainPanel(
                     h2("Tabulated dataset"),
                     helpText("Sort columns descending to view missing values."),
                     DTOutput("table"))
             )),
    tabPanel(
        "Explore missingness",
        h2("Observed missingness pattern per variable"),
        helpText("Observed data is blue, missing data is red."),
        plotOutput("md_pattern", height = "100%")
    ),
    tabPanel(
        "Impute missingness",
        h2("Impute missing data using `mice`"),
        helpText("This may take a while."),
        actionButton("mice", "Impute"),
        plotOutput("traceplot")
    ),
    navbarMenu(
        "More",
        tabPanel(
            "Summary", 
            h2("Descriptive statistics per variable"),
            h5("NB. Particularly the number of NA values is of interest."),
            verbatimTextOutput(#where to place the output code
                "summary")),
        tabPanel("About",
                 h2("About this app"),
                 fluidRow(
                     column(6,
                            includeMarkdown("about.Rmd")),
                     column(
                         3,
                         img(
                             class = "img-polaroid",
                             src = paste0(
                                 "https://raw.githubusercontent.com/gerkovink/shinyMice/edits/ICML/shinymicehex.png"
                             ),
                             style = paste0("width:110%;")
                         ),
                         tags$small(
                             "Impression of the hex sticker ",
                             "for the interactive evaulation ",
                             "suite for multiple imputation 'ShinyMICE'",
                             a(href = "https://github.com/gerkovink/shinyMice")
                         )
                     )
                 ))
    )
))
