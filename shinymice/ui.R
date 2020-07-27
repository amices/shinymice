# shinymice UI

# TO DO:
# menu bar naar dropdown
# data of csv of rdata file --> met verschillende mids objecten om te evalueren
# nieuwe kleuren: blauw #006CC2, rood #B61A51
# lighter blue #66a6da, lighter red #d37596
# icons: "bar-chart-o", "table", "list-alt

# set-up
library("shiny")
library("shinythemes")
library("mice")
library("DT")
library("data.table")
#library("naniar")
library("rmarkdown")
library("ggplot2")
#library("shinycssloaders")

shinyUI(
    fluidPage(
        title = "shinymice",
        theme = shinythemes::shinytheme("flatly"),
        # for logo/name as browser icon, see https://stackoverflow.com/questions/51688463/shiny-page-title-and-image
        list(tags$head(
            HTML('<link rel="icon" href="logo_square.png"
                type="image/png" />')
        )),
        shinyjs::useShinyjs(),
        # for right aligned navbar, see https://stackoverflow.com/questions/35584644/r-shiny-navbarpage-right-aligned-tabs
        tags$head(
            tags$style(HTML("
          .navbar .navbar-nav {float: right}
          .navbar .navbar-header {float: right}
        "))
        ),
        tags$head(tags$style(
            HTML(
                "#themeToggle, .visually-hidden {
                position: absolute;
                width: 1px;
                height: 1px;
                clip: rect(0 0 0 0);
                clip: rect(0, 0, 0, 0);
                overflow: hidden;
                };
    "
            )
        )),
        
        navbarPage(
            title = div(
                img(src = "logo_wide.png", style = "width:155px;position:fixed;left:30px;")
            ),
            #to make 'hamburger' menu on small screens
            collapsible = TRUE,
            selected = "Data",
            # add theme toggle in header, for themes, see below, for placement see https://stackoverflow.com/questions/56873774/change-css-properties-of-shiny-checkbox
            header = div(
                checkboxInput(
                    inputId = "themeToggle",
                    label = icon("moon"),
                    value = FALSE,
                    width = "150%"
                ),
                style = "text-align:right;"
            ),
            
            tabPanel(
                title = "Data",
                icon = icon("file-upload"),
                sidebarLayout(
                    sidebarPanel(
                        id = "sidebar",
                        h2("Select a dataset"),
                        fileInput(
                            "upload",
                            label = tags$b("Upload a CSV or RData file..."),
                            accept = c("text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv", 
                                       ".Rdata")
                        ),
                        div(style = "margin-top:-1em;", checkboxInput("header", label = "CSV file contains variable names", value = TRUE)),
                        selectInput(
                            "choice",
                            label = tags$b("...or use `mice` data"),
                            choices = data(package = "mice")$results[-c(5, 7, 17, 18), "Item"]
                        ),
                        actionButton("reset", "Reset", icon = icon("redo")),
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
                icon = icon("chart-area"),
                #icon("list-alt")
                h2("Inspect relations before imputation"),
                varSelectInput("histvar1", "Choose a variable to plot:", data = mice::boys),
                varSelectInput("histvar2", "Conditional on missingness in:", data = mice::boys),
                plotOutput("hist", width = "auto", height = "720px")
            )
            ),
            
            
            tabPanel(
                "Impute",
                icon = icon("calculator"),
                h2("Impute missing data using `mice`"),
                tags$b("Dataset to impute"),
                verbatimTextOutput("names"),
                helpText("Showing only the first 5 variable names to check what data is used."),
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
                actionButton("mice", "Impute", icon = icon("hourglass-start")),
                #verbatimTextOutput("micecall"),
                helpText("This may take a minute."),
                verbatimTextOutput("done") #%>% withSpinner(color="#0dc5c1")
                #plotOutput("traceplot")
            ),
            
            tabPanel(
                "Evaluate",
                icon = icon("chart-line"),
                #icon("list-alt")
                h2("Evaluate convergence"),
                varSelectInput("varnr", "Choose a variable:", data = mice::boys),
                plotOutput("traceplot")
            ),
            
            tabPanel(
                "Save", #add sav download
                icon = icon("file-download"),
                h2("Download the dataset or imputations"),
                tags$b("Download the dataset:"),
                br(),
                br(),
                downloadButton("savecsv", "Download dataset as CSV file"),
                br(),
                br(),
                downloadButton("saverdata", "Download dataset as RData file"),
                br(),
                br(),
                tags$b("Download the imputations:"),
                br(),
                br(),
                downloadButton("savemids", "Download imputations as RData file")
            ),
            
            navbarMenu(
                "More",
                icon = icon("ellipsis-h"),
                # make options appear on hover, see https://stackoverflow.com/questions/34597421/dropdown-bootstrap-menu-with-unfold-effect
                tags$script(
                    HTML(
                        "$('.navbar .dropdown').hover(function() {
                $(this).find('.dropdown-menu').first().stop(true, true).slideDown();
            }, function() {
                $(this).find('.dropdown-menu').first().stop(true, true).slideUp()
            });"
                    )
                ),
                
                tabPanel(
                    "About",
                    icon = icon("info-circle"),
                    h2("About this app"),
                    fluidRow(column(6,
                                    includeMarkdown("www/about.Rmd")),
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
                             ))
                ),
                
                tags$script(
                    #for light/dark theme, see https://stackoverflow.com/questions/61632272/r-shiny-light-dark-mode-switch
                    "
        // define css theme filepaths
        const themes = {
            dark: 'shinythemes/css/darkly.min.css',
            light: 'shinythemes/css/flatly.min.css'
        }

        // function that creates a new link element
        function newLink(theme) {
            let el = document.createElement('link');
            el.setAttribute('rel', 'stylesheet');
            el.setAttribute('text', 'text/css');
            el.setAttribute('href', theme);
            return el;
        }

        // function that remove <link> of current theme by href
        function removeLink(theme) {
            let el = document.querySelector(`link[href='${theme}']`)
            return el.parentNode.removeChild(el);
        }

        // define vars
        const darkTheme = newLink(themes.dark);
        const lightTheme = newLink(themes.light);
        const head = document.getElementsByTagName('head')[0];
        const toggle = document.getElementById('themeToggle');

        // define extra css and add as default
        const extraDarkThemeCSS = '.dataTables_length label, .dataTables_filter label, .dataTables_info {color: grey!important;} .paginate_button {background: white!important;} thead {color: grey;}'
        const extraDarkThemeElement = document.createElement('style');
        extraDarkThemeElement.appendChild(document.createTextNode(extraDarkThemeCSS));
        head.appendChild(extraDarkThemeElement);


        // define event - checked === 'light'
        toggle.addEventListener('input', function(event) {
            // if checked, switch to light theme
            if (toggle.checked) {
                // else add darktheme
                removeLink(themes.light);
                head.appendChild(extraDarkThemeElement)
                head.appendChild(darkTheme);
            }  else {
                removeLink(themes.dark);
                head.removeChild(extraDarkThemeElement);
                head.appendChild(lightTheme);
            }
        })
        "
                )
            )
        )
    )
)
