#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# Icons: https://fontawesome.com/icons?d=gallery&m=free

library("shiny")
library("shinythemes")
library("mice")
library("DT")
library("data.table")
#library("naniar")
library("rmarkdown")
library("ggplot2")
#library("shinycssloaders")


# Define UI for application that draws a histogram
shinyUI(
    navbarPage(
        title = "shinymice", 
        selected = "Data",
        header = checkboxInput(
            inputId = "themeToggle",
            label = icon("moon"),
            value = FALSE
            ),
        shinyjs::useShinyjs(),
        # # for logo/name as browser icon, see https://stackoverflow.com/questions/51688463/shiny-page-title-and-image
        # # does not work: div("Report", img(src = "www/logo.png", height = "10px", style = "position: relative; top: -3px; left: -1000px;")),#
        # # for right aligned navbar, see https://stackoverflow.com/questions/35584644/r-shiny-navbarpage-right-aligned-tabs
        # tags$head(
        #     tags$style(HTML("
        #   .navbar .navbar-nav {float: right}
        #   .navbar .navbar-header {float: right}
        # "))),
        theme = shinythemes::shinytheme("flatly"),
        collapsible = TRUE, #make 'hamburger' menu on small screens
        tabPanel(
            title = "Data",
            icon = icon("file-upload"),
            sidebarLayout(
                sidebarPanel(
                    id = "sidebar",
                    h2("Select a dataset"),
                    fileInput(
                        "upload",
                        label = h4("Upload a CSV file..."),
                        accept = c("text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv")
                    ),
                    checkboxInput("header", label = "CSV file contains variable names", value = TRUE),
                    selectInput(
                        #is the input of the app that the user interacts with
                        "choice",
                        label = h4("...or use `mice` data"),
                        choices = data(package = "mice")$results[-c(5, 7, 17, 18), "Item"]
                    ),
                    actionButton("reset", "Reset", icon = icon("redo")),
                ),
                mainPanel(
                    # checkboxInput(
                    #     inputId = "themeToggle",
                    #     label = icon("moon"),
                    #     value = FALSE,
                    #     width = "100%"
                    # ),
                    h2("Tabulated dataset"),
                    helpText("Sort variables descending to view missing values."),
                    DT::DTOutput("table")
                )
            )
        ),
        tabPanel(
            "Explore",
            icon = icon("th"),
            #icon("bar-chart-o") or table
            h2("Observed missingness pattern per variable"),
            helpText("Observed data is blue, missing data is red."),
            plotOutput("md_pattern", height = "150%", width = "100%"),
            style = 'width:100%;height:85vh;overflow-y: scroll;'
        ),
        tabPanel(
            "Impute",
            icon = icon("calculator"),
            #icon("list-alt")
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
            helpText("This may take a minute."),
            verbatimTextOutput("done") #%>% withSpinner(color="#0dc5c1")
            #plotOutput("traceplot")
        ),
        tabPanel(
            "Evaluate",
            icon = icon("chart-line"),
            #icon("list-alt")
            h2("Evaluate convergence"),
            plotOutput("traceplot")
        ),
        # tabPanel(checkboxInput(inputId = "themeToggle",
        #                        label = icon("moon"),
        #                        value = FALSE)),
        navbarMenu(
            "More",
            icon = icon("ellipsis-h"),
            # tabPanel(
            #     "Summary",
            #     h2("Descriptive statistics per variable"),
            #     helpText("Check the number of NAs."),
            #     verbatimTextOutput(#where to place the output code
            #         "summary")),
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
        const extraDarkThemeCSS = '.dataTables_length label, .dataTables_filter label, .dataTables_info {       color: grey!important;} .paginate_button { background: white!important;} thead { color: white;}'
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
