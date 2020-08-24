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
library(warn.conflicts = FALSE, "dplyr")
# library(warn.conflicts = FALSE, "thematic") # for ggplot background that converts with the shiny theme, use thematic::thematic_on(font = "auto")

shinyUI(
    fluidPage(
        title = "shinymice",
        # for logo/name as browser icon, see https://stackoverflow.com/questions/51688463/shiny-page-title-and-image
        list(tags$head(
            HTML('<link rel="icon" href="logo_square.png"
                type="image/png" />')
        )),
        # load functions
        shinyjs::useShinyjs(),
        shinyFeedback::useShinyFeedback(),
        waiter::use_waiter(),
        includeCSS(path = "www/right_align_nav.CSS"),
        includeScript(path = "www/link_to_tab.JS"),
        theme = shinythemes::shinytheme("flatly"),
        #"./theme/css/flatly.min.css", #
        navbarPage(
            title = div(
                img(src = "logo_wide.png", style = "width:155px;position:fixed;left:30px;")
            ),
            #to make 'hamburger' menu on small screens
            collapsible = TRUE,
            selected = "shinymice",
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
            ),
            div(verbatimTextOutput("banner"),
                style = "text-align:right;width:102%;"),
            
            
            ## Landing page
            tabPanel(
                title = "shinymice",
                "Overview of the app (not finished yet!!)",
                # link to tabs, see https://davidruvolo51.github.io/shinytutorials/tutorials/shiny-link/
                tags$p(
                    "You are currently on the home page. Go to the",
                    shinyLink(to = "data-tab", label = "Data page"),
                    " to select some data. Then explore the missingness on the ",
                    shinyLink(to = "expl-tab", label = "Explore page"),
                    ", impute the missingness on the ",
                    shinyLink(to = "impu-tab", label = "Impute page"),
                    ", and evaluate the imputations on the ",
                    shinyLink(to = "eval-tab", label = "Evaluate page"),
                    
                    ". Finally, save the imputations on the ",
                    shinyLink(to = "save-tab", label = "Save page"),
                    
                    "."
                )
            ),
            
            ## Data tab
            tabPanel(
                title = "Data",
                value = "data-tab",
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
                            helpText("Accepts `.Rdata`, `.csv`, `.tsv`, and `.txt` files.") #add option to read from link, see https://cran.r-project.org/web/packages/vroom/vignettes/vroom.html
                        ),
                        fileInput("upload_mids",
                                  div(
                                      HTML(
                                          "<strong> Or choose a multiply imputed dataset (<code>mids</code> object) </strong>"
                                      )
                                  )),#,
                                  #accept = ".Rdata"),
                        div(style = "margin-top:-2em;", helpText("Does not do anything anymore."))
                    ),
                    mainPanel(
                        textOutput("test"),
                        h2("Tabulated dataset"),
                        helpText("Sort variables descending to view missing values."),
                        DT::DTOutput("table")
                    )
                )
            ),
            tabPanel("Explore",
                     icon = icon("search"),
                     value = "expl-tab",
                     fluidRow(
                         column(
                             3,
                             h2("Explore missingness"),
                             br(),
                             varSelectInput("NA_var1", "Distribution of", data = mice::boys),
                             varSelectInput("NA_var2", "given (missingness in)", data = mice::boys),
                             helpText("Hint: the strongest relations are with observed data in"),
                             div(textOutput("relations"), style = "margin-top:-1em;"),
                             helpText("Hint: the strongest relations are with missingness in"),
                             div(textOutput("NA_relations"), style = "margin-top:-1em;"),
                             
                         ),
                         column(
                             9,
                             tabsetPanel(
                                 tabPanel("Pattern",
                                          plotOutput("md_pattern", height = "520px")),
                                 tabPanel("Scatterplot",
                                          plotlyOutput("NA_scat", height = "520px")),
                                 tabPanel("Histogram",
                                          plotlyOutput("NA_hist", height = "520px"))
                             ),
                             br(),
                             tags$b("Additional options for histograms"),
                             splitLayout(
                                 cellWidths = c("30%", "70%"),
                                 numericInput(
                                     "bins",
                                     "",
                                     min = 0,
                                     step = 0.5,
                                     value = 0,
                                     width = 70
                                 ),
                                 div(br(), br(), "Binwidth (default when 0)")
                             ),
                             checkboxInput("scalehist", "Fixed heigth y-axis", value = TRUE)
                         )
                     )),
            
            
            tabPanel(
                "Impute",
                value = "impu-tab",
                icon = icon("calculator"),
                div(HTML(
                    "<h2> Impute missing data using <code>mice</code></h2>"
                )),
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
                    step = 5
                ),
                textInput("seed", "Set random number generator seed", value = "123"),
                textInput("impname", "Name the imputation object", value = "imp"),
                tags$b("Mice call"),
                verbatimTextOutput("micecall"),
                actionButton("mice", "Impute", icon = icon("hourglass-start")),
                helpText("This may take a minute."),
                br(),
                verbatimTextOutput("done")
            ),
            
            
        tabPanel("Evaluate",
                     value = "eval-tab",
                     icon = icon("check-square"),
                     fluidRow(
                         column(
                             3,
                             h2("Evaluate imputations"),
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
                             )
                             
                         ),
                         column(9,
                                tabsetPanel(
                                    tabPanel("Model",
                                             plotlyOutput("fluxplot")),
                                    # h2("Imputation model influx-outflux"),
                                    #         "The influx of a variable quantifies how well its missing data connect to the observed data on other variables. The outflux of a variable quantifies how well its observed data connect to the missing data on other variables. In general, higher influx and outflux values are preferred.",
                                    #         tags$a(href = "https://stefvanbuuren.name/fimd/missing-data-pattern.html#sec:flux", "See van Buuren (2018)."),
                                    #         plotlyOutput("fluxplot")
                                    tabPanel("Convergence",
                                             plotlyOutput("traceplot")),
                                    tabPanel("Imputations", plotlyOutput("impplot"))
                                ),
                                h3("Add iterations"),
                                numericInput(
                                    "midsmaxit",
                                    label = "Continue iterating (optional)",
                                    value = 5,
                                    min = 1,
                                    step = 5
                                ),
                                actionButton("iterate", "Iterate", icon = icon("hourglass-start")),
                                helpText("This may take a minute.")
                         
                         
                     )
                     )),
            
            
            tabPanel(
                "Save",
                #add sav download
                icon = icon("file-download"),
                value = "save-tab",
                h2("Download the dataset or imputations"),
                br(),
                selectInput(
                    "mids_or_data",
                    "Save the imputations, or just the data",
                    choices = c("Imputations (incl. the data)", "Just the data")
                ),
                selectInput("rdata_or_csv", "Save as ...", choices = c(".Rdata", ".csv")),
                downloadButton("save", "Save"),
                helpText(
                    "Add warning/disable downloading mids as csv! Add downloas as .sav option."
                )
                
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
