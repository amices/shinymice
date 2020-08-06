# shinymice UI

# set-up
library(warn.conflicts = FALSE, "shiny")
library(warn.conflicts = FALSE, "shinythemes")
library(warn.conflicts = FALSE, "mice")
library(warn.conflicts = FALSE, "DT")
library(warn.conflicts = FALSE, "data.table")
library(warn.conflicts = FALSE, "rmarkdown")
library(warn.conflicts = FALSE, "ggplot2")
library(warn.conflicts = FALSE, "plotly")
# library(warn.conflicts = FALSE, "thematic") # for ggplot background that converts with the shiny theme, use thematic::thematic_on(font = "auto")

shinyUI(
    fluidPage(
        title = "shinymice",
        # for logo/name as browser icon, see https://stackoverflow.com/questions/51688463/shiny-page-title-and-image
        list(tags$head(
            HTML(
                '<link rel="icon" href="www/logo_square.png"
                type="image/png" />'
            )
        )),
        # load functions
        shinyjs::useShinyjs(),
        shinyFeedback::useShinyFeedback(),
        waiter::use_waiter(),
        includeCSS(path = "www/right_align_nav.CSS"),
        theme = shinythemes::shinytheme("flatly"),
        navbarPage(
            title = div(
                img(src = "logo_wide.png", style = "width:155px;position:fixed;left:30px;")
            ),
            #to make 'hamburger' menu on small screens
            collapsible = TRUE,
            selected = "Data",
            # add theme toggle in header, for themes, see below, for placement see https://stackoverflow.com/questions/56873774/change-css-properties-of-shiny-checkbox
            includeScript(path = "www/theme_switch.JS"),
            header = div(
                checkboxInput(
                    inputId = "themeToggle",
                    label = icon("moon"),
                    value = FALSE,
                    width = "150%"
                ),
                style = "text-align:right;"
            ), div(
                verbatimTextOutput("datname"),
                style = "text-align:right;width:102%;" 
                ),
            
            tabPanel(
                title = "Data",
                icon = icon("file-upload"),
                sidebarLayout(
                    sidebarPanel(
                        id = "sidebar",
                        h2("Select data"),
                        fileInput(
                            "upload",
                            tags$b("Choose a dataset with missing values"),
                            accept = c(
                                "text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv",
                                ".Rdata",
                                ".tsv"
                            )
                        ),
                        div(
                            style = "margin-top:-1em;",
                            helpText("Accepts `.Rdata`, `.csv`, `.tsv`, and `.txt` files.")
                        ),
                        fileInput(
                            "midsobject",
                            tags$b("Or choose a multiply imputed dataset (`mids` object)"),
                            accept = ".Rdata"
                        ),
                        div(style = "margin-top:-2em;", helpText("Does not do anything anymore."))
                        
                        #,
                        # div(
                        #     style = "margin-top:-1em;",
                        #     checkboxInput("header", label = "CSV file contains variable names", value = TRUE)
                        #),
                        # selectInput(
                        #     "choice",
                        #     label = tags$b("...or use `mice` data"),
                        #     choices = data(package = "mice")$results[-c(5, 7, 17, 18), "Item"]
                        # ),
                        #actionButton("reset", "Reset", icon = icon("redo")),
                    ),
                    mainPanel(
                        h2("Tabulated dataset"),
                        helpText("Sort variables descending to view missing values."),
                        DT::DTOutput("table")
                    )
                )
            ),
            
            navbarMenu(
                "Explore",
                icon = icon("search"),
                
                tabPanel(
                    "Pattern",
                    icon = icon("th"),
                    h2("Observed missingness pattern per variable"),
                    helpText("Observed data is blue, missing data is red."),
                    plotOutput("md_pattern", height = "150%", width = "100%"),
                    style = 'width:100%;height:85vh;overflow-y: scroll;'
                ),
                
                tabPanel(
                    "Distributions",
                    icon = icon("chart-bar"),
                    #icon("list-alt")
                    h2("Inspect relations before imputation"),
                    br(),
                    fluidRow(
                        column(
                            3,
                            varSelectInput("histvar1", "Choose a variable to plot:", data = mice::boys),
                            varSelectInput("histvar2", "Conditional on missingness in:", data = mice::boys),
                            checkboxInput("scalehist", "Fixed heigth y-axis", value = TRUE),
                            numericInput(
                                "binwidth",
                                "Binwidth (optional)",
                                min = 0,
                                step = 0.5,
                                value = 0
                            )
                        ),
                        column(9,
                               plotlyOutput(
                                   "hist", width = "auto", height = "520px"
                               ))
                    )
                )
            ),
            
            
            tabPanel(
                "Impute",
                icon = icon("calculator"),
                h2("Impute missing data using `mice`"),
                tags$b("Dataset to impute"),
                #verbatimTextOutput("datname"),
                numericInput(
                    "m",
                    label = "Number of imputations",
                    value = 5,
                    min = 1,
                    step = 1
                ),
                numericInput(
                    "maxit",
                    label = "Number of iterations",
                    value = 5,
                    min = 1,
                    step = 1
                ),
                tags$b("Mice call"),
                verbatimTextOutput("micecall"),
                # textInput("miceargs", label = "Add arguments", value = NULL),
                waiter::use_waiter(),
                actionButton("mice", "Impute", icon = icon("hourglass-start")),
                helpText("This may take a minute."),
                br(),
                verbatimTextOutput("done")
                #plotOutput("traceplot")
            ),
            
            navbarMenu(
                "Evaluate",
                icon = icon("check-square"),
                tabPanel(
                    "Model",
                    icon = icon("balance-scale-right"),
                    h2("Imputation model influx-outflux"),
                    helpText(
                        "Variables that have more observed information ... Variables that ..."
                    ),
                    plotlyOutput("fluxplot")
                ),
                tabPanel(
                    "Convergence",
                    icon = icon("chart-line"),
                    h2("Evaluate convergence"),
                    br(),
                    fluidRow(
                        column(
                            3,
                            varSelectInput("varnr", "Choose a variable:", data = mice::boys),
                            numericInput(
                                "midsmaxit",
                                label = "Continue iterating (optional)",
                                value = 5,
                                min = 1,
                                step = 1
                            ),
                            actionButton("mids", "Iterate", icon = icon("hourglass-start")),
                            helpText("Does not work anymore.")
                        ),
                        column(9,
                               plotlyOutput("traceplot"))
                    )
                ),
                tabPanel(
                    "Imputations",
                    icon = icon("chart-area"),
                    h2("Visualize imputations to inspect"),
                    br(),
                    fluidRow(
                        column(
                            3,
                            selectInput(
                                "plottype",
                                "Choose a visualization:",
                                c("bwplot", "densityplot", "histogram", "stripplot", "xyplot"),
                                selected = "stripplot"
                            ),
                            varSelectInput("midsvar1", "Choose a variable:", data = mice::boys),
                            varSelectInput(
                                "midsvar2",
                                "Choose a second variable (for `xyplot` only):",
                                data = mice::boys
                            ),
                            
                        ),
                        column(9,
                               plotlyOutput("impplot"))
                    )
                )
            ),
            
            tabPanel(
                "Save",
                #add sav download
                icon = icon("file-download"),
                h2("Download the dataset or imputations"),
                br(),
                tags$b("Download the dataset:"),
                fluidRow(column(
                    6,
                    downloadButton("savecsv", "Download dataset as CSV file"),
                ), column(
                    6,
                    downloadButton("saverdata", "Download dataset as RData file"),
                )),
                br(),
                tags$b("Download the imputations:"),
                fluidRow(column(
                    6,
                    downloadButton("savemids", "Download imputations as RData file")
                ))
            ),
            
            navbarMenu(
                "More",
                icon = icon("ellipsis-h"),
                tabPanel(
                    "About",
                    icon = icon("info-circle"),
                    h2("About this app"),
                    fluidRow(column(8,
                                    includeMarkdown("www/about.Rmd")),
                             column(
                                 4,
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
                                     "suite for multiple imputation `shinymice`",
                                     a(href = "https://github.com/gerkovink/shinyMice")
                                 )
                             ))
                )
            )
        )
    )
)
