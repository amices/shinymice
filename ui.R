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
                img(src = "logo_wide.png", style = "width:155px;position:absolute;left:30px;")
            ),
            #to make 'hamburger' menu on small screens
            collapsible = TRUE,
            selected = "Home",
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
            # div(verbatimTextOutput("banner"),
            #     style = "text-align:right;width:102%;"),
            div(
                selectInput("banner2", "", width = 200, choices = "Imputations (none yet)"),
                style = "margin-top:-2em;margin-bottom:-2em;margin-right:-1em;float:right;"
            ),
            
            
            ## Landing page
            tabPanel(
                title = "Home",
                icon = icon("home"),
                fluidRow(
                    column(
                        12,
                        align = "center",
                        img(class = "img-polaroid",
                            src = "logo_wide.png",
                            style = "width:50%;"),
                        br(),
                        br(),
                        br(),
                        # link to tabs, see https://davidruvolo51.github.io/shinytutorials/tutorials/shiny-link/
                        tags$p(
                            "You are currently on the home page. Go to the",
                            tags$a(
                                actionButton("next-home", "Data page"),
                                href = "data-tab",
                                class = "shiny__link"
                            ),
                            " to select some data. Then explore the missingness on the ",
                            tags$a(
                                actionButton("next-home", "Explore page"),
                                href = "expl-tab",
                                class = "shiny__link"
                            ),#shinyLink(to = "expl-tab", label = "Explore page"),
                            ", impute the missingness on the ",
                            tags$a(
                                actionButton("next-home", "Impute page"),
                                href = "impu-tab",
                                class = "shiny__link"
                            ),#shinyLink(to = "impu-tab", label = "Impute page"),
                            ", and evaluate the imputations on the ",
                            tags$a(
                                actionButton("next-home", "Evaluate page"),
                                href = "eval-tab",
                                class = "shiny__link"
                            ),#shinyLink(to = "eval-tab", label = "Evaluate page"),
                            
                            ". Finally, save the imputations on the ",
                            tags$a(
                                actionButton("save-home", "Save page"),
                                href = "data-tab",
                                class = "shiny__link"
                            ),#shinyLink(to = "save-tab", label = "Save page"),
                            
                            "."
                        ),
                        "For the most recent version of this app or to report an issue, see ",
                        tags$a(href = "https://github.com/amices/shinyMice", "github.com/amices/shinyMice")
                    )
                ),
                #,
                #actionButton("Next", "Next", onclick = 'location.href=shinyLink(to = "save-tab", label = "Save page");')
                br(),
                br(),
                div(
                    tags$a(
                        actionButton("next-home", "Next"),
                        href = "data-tab",
                        class = "shiny__link"
                    ),
                    style = "text-align:right;"
                )
            ),
            
            ## Data tab
            tabPanel(
                title = "Data",
                value = "data-tab",
                icon = icon("file-upload"),
                fluidRow(
                    column(
                        4,
                        #id = "sidebar",
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
                            style = "margin-top:-2em;",
                            helpText("Accepts `.Rdata`, `.csv`, `.tsv`, and `.txt` files.") #add option to read from link, see https://cran.r-project.org/web/packages/vroom/vignettes/vroom.html
                        ),
                        fileInput("upload_mids",
                                  div(
                                      HTML(
                                          "<strong> Or choose a multiply imputed dataset (<code>mids</code> object) </strong>"
                                      )
                                  )),
                        div(style = "margin-top:-2em;",
                            helpText("Only accepts `.Rdata` files.")) #add option to read from link, see https://cran.r-project.org/web/packages/vroom/vignettes/vroom.html
                        #,
                        #,
                        #accept = ".Rdata"),
                    ),
                    column(8,
                           tabsetPanel(
                               tabPanel(
                                   "Table",
                                   br(),
                                   # add that it's a subset
                                   helpText("Hint: Sort variables descending to view missing values."),
                                   DT::DTOutput("table")
                               ),
                               tabPanel(
                                   "Descriptives",
                                   br(),
                                   #tags$b(textOutput("n")),
                                   helpText("Categorical variables denoted with '*'"),
                                   br(),
                                   DT::DTOutput("descr"),
                                   DT::DTOutput("categ")
                               )
                           ))
                ),
                br(),
                br(),
                div(
                    tags$a(
                        actionButton("next-data", "Next"),
                        href = "expl-tab",
                        class = "shiny__link"
                    ),
                    style = "text-align:right;"
                )
            ),
            tabPanel(
                "Explore",
                icon = icon("search"),
                value = "expl-tab",
                fluidRow(
                    column(
                        4,
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
                        8,
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
                ),
                br(),
                br(),
                div(
                    tags$a(
                        actionButton("next-expl", "Next"),
                        href = "impu-tab",
                        class = "shiny__link"
                    ),
                    style = "text-align:right;"
                )
            ),
            
            
            tabPanel(
                "Impute",
                value = "impu-tab",
                icon = icon("calculator"),
                fluidRow(
                    column(
                        4,
                        #        div(HTML(
                        # "<h2> Impute missing data using <code>mice</code></h2>"
                        #)),
                        h2("Impute missingness"),
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
                        #tags$b("Mice call"),
                        textInput("args", "Additional arguments (optional)", value = NULL)
                    ),
                    column(8,
                           tabsetPanel(
                               tabPanel(
                                   "Mice call",
                                   br(),
                                   verbatimTextOutput("micecall"),
                                   actionButton("mice", "Impute", icon = icon("hourglass-start")),
                                   helpText("This may take a minute.")
                               )
                           ))
                ),
                #br(),
                #verbatimTextOutput("done"),
                br(),
                br(),
                div(
                    tags$a(
                        actionButton("next-impu", "Next"),
                        href = "eval-tab",
                        class = "shiny__link"
                    ),
                    style = "text-align:right;"
                )
            ),
            
            
            tabPanel(
                "Evaluate",
                value = "eval-tab",
                icon = icon("check-square"),
                fluidRow(
                    column(
                        4,
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
                    column(
                        8,
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
                ),
                br(),
                br(),
                div(
                    tags$a(
                        actionButton("next-eval", "Next"),
                        href = "save-tab",
                        class = "shiny__link"
                    ),
                    style = "text-align:right;"
                )
            ),
            
            
            tabPanel(
                "Save",
                #add sav download
                icon = icon("file-download"),
                value = "save-tab",
                fluidRow(
                    column(
                        4,
                        h2("Save results"),
                        br(),
                        selectInput(
                            "mids_or_data",
                            "Save the imputations, or just the data",
                            choices = c("Imputations (incl. the data)", "Just the data")
                        ),
                        selectInput("rdata_or_csv", "Save as ...", choices = c(".Rdata", ".csv")),
                        downloadButton("save", "Save"),
                        # helpText(
                        #     "Add warning/disable downloading mids as csv! Add downloas as .sav option."
                        # )
                    )
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
                                     src = #paste0(
                                         "logo_square.png"#"https://raw.githubusercontent.com/gerkovink/shinyMice/edits/ICML/shinymicehex.png"
                                     ,
                                     style = "width:100%;"
                                 ),
                                 tags$small(
                                     "Impression of the hex sticker ",
                                     "for the interactive evaulation ",
                                     "suite for multiple imputation `shinymice`",
                                     a(href = "https://github.com/amices/shinyMice")
                                 )
                             ))
                )
            )
        )
    )
)
