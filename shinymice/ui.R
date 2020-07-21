#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# Icons: https://fontawesome.com/icons?d=gallery&m=free

library("shiny")
library("mice")
library("DT")
library("data.table")
library("naniar")
library("rmarkdown")

# Define UI for application that draws a histogram
shinyUI(navbarPage(
    title = "shinymice", theme = shinythemes::shinytheme("flatly"),
    #sets basic virtual structure (layout function)
    tabPanel("Data", icon = icon("file-upload"),
             sidebarLayout(
                 sidebarPanel(
                     shinyjs::useShinyjs(),
                     id = "sidebar",
                     h2("Select a dataset"),
                     fileInput("upload", label = h4("Upload a CSV file..."), 
                               accept = c(
                                   "text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv")
                     ),
                     checkboxInput("header", label = "CSV file contains variable names", value = TRUE),
                     selectInput(
                         #is the input of the app that the user interacts with
                         "choice",
                         label = h4("...or use `mice` data"),
                         choices = data(package = "mice")$results[-c(5,7, 17, 18), "Item"]
                     ),
                     actionButton("reset", "Reset", icon = icon("redo")),
                 ),
                 mainPanel(
                     h2("Tabulated dataset"),
                     helpText("Sort variables descending to view missing values."),
                     DT::DTOutput("table"))
             )),
    tabPanel(
        "Explore", icon = icon("table"), #icon("bar-chart-o")
        h2("Observed missingness pattern per variable"),
        helpText("Observed data is blue, missing data is red."),
        plotOutput("md_pattern", height = "150%", width = "100%"), 
        style = 'width:100%;height:85vh;overflow-y: scroll;'
    ),
    tabPanel(
        "Impute", icon = icon("chart-line"), #icon("list-alt")
        h2("Impute missing data using `mice`"),
        tags$b("Dataset to impute"),
        verbatimTextOutput("names"),
        helpText("Showing only the first 5 variable names to check what data is used."),
        numericInput("m", label = "Number of imputations", value = 5, min = 1, step = 1),
        numericInput("maxit", label = "Number of iterations", value = 5, min = 1, step = 1),
        actionButton("mice", "Impute", icon = icon("hourglass-start")),
        helpText("This may take a minute."),
        plotOutput("traceplot")
    ),
    navbarMenu(
        "More", icon = icon("ellipsis-h"),
        # tabPanel(
        #     "Summary", 
        #     h2("Descriptive statistics per variable"),
        #     helpText("Check the number of NAs."),
        #     verbatimTextOutput(#where to place the output code
        #         "summary")),
        tabPanel("About", icon = icon("info-circle"),
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
                             style = paste0("width:100%;")
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
