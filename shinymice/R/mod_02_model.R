#' 02_model UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_02_model_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(
        4,
        tags$b("Define the imputation model"),
        br(),
        br(),
        "1. Look at the missing data pattern for an overview of the missingness.",
        br(),
        "2. Evaluate the in and outflux of the model.",
        br(),
        "3. Check (and modify) the predictor matrix.",
        br(),
        "4. Make the imputations reproducible by specifying an initial random value (", tags$code("seed"), ")",
        set_number(ns("seed"), val = 123),
        no_br(),
        "5. Determine the number of imputations (", tags$code("m"),")",
        set_number(ns("m"), val = 5),
        no_br(),
        "6. And the number of iterations (",tags$code("maxit"),")",
        set_number(ns("maxit"), val = 2),
        no_br(),
        "7. Add optional arguments to the imputation model (e.g.,", tags$code("method = 'pmm'"), ")",
        textInput(ns("add_args"), label = NULL, value = NULL),
        no_br(),
        "8. Impute the incomplete data (run", tags$code("mice"), ")",
        br(),
        actionButton(ns("run_mice"), label = "Impute", width = 100),
        br()
      ),
      column(
        8,
        tabsetPanel(
          tabPanel(
            "Missingness pattern",
            plotOutput(ns("md_plot")),
            br(),
            tags$b("Interpretation:"),
            "The missing data pattern influences the amount of information that can be transferred between variables. Imputation can be more precise if other variables are non-missing for those cases that are to be imputed. The reverse is also true. Predictors are potentially more powerful if they have are non-missing in rows that are vastly incomplete."
          ),
          tabPanel(
            "Fluxplot",
            plotly::plotlyOutput(ns("flux_plot")),
            br(),
            tags$b("Interpretation:"),
            "Influx and outflux are summaries of the missing data pattern intended to aid in the construction of imputation models. The influx of a variable quantifies how well its missing data connect to the observed data on other variables. The outflux of a variable quantifies how well its observed data connect to the missing data on other variables. Keeping everything else constant, variables with high influx and outflux are preferred."
          ),
          tabPanel(
            "Predictor matrix",
            div(
              actionButton(ns("quickpred"), "Generate"), 
              "a predictor matrix with minimum (absolute) correlations of \U03C1 = ", 
              style = "display:inline-block;"
            ),
            div(
              numericInput(ns("mincor"), NULL, value = 0, min = 0, max = 1, step = 0.1, width = 80),
              style = "display:inline-block;"
            ),
            plotOutput(ns("pred_plot")),
            br(),
            tags$b("Interpretation:"),
            "Each row in the predictor matrix identifies which predictors are to be used for the variable in the row name. Note that the pedictor matrix also includes variables that are completely observed (and thus have no predictors).",
            br()
          )
        ))
    ))
}
    
#' 02_model Server Functions
#'
#' @noRd 
mod_02_model_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    stopifnot(is.reactive(data))
    output$md_plot <- renderPlot(plot_md_pattern(data()))
    output$flux_plot <-
      plotly::renderPlotly({
        p <- plot_flux(data())
        plotly::ggplotly(p) %>% 
          clean_plotly()
      })
    pred <- eventReactive(input$quickpred, {mice::quickpred(data(), mincor = input$mincor)}) #, ignoreNULL = FALSE
    output$pred_plot <-
      renderPlot(plot_pred_matrix(pred()))
    imp <-
      eventReactive(input$run_mice, {
        validate(need(input$run_mice > 0,
                      "Please impute the incomplete data first."))
        waiter::waiter_show(html = waiter::spin_throbber(),
                            color = waiter::transparent(.5))
        on.exit(waiter::waiter_hide())
        eval(parse(
          text = paste(
            "mice::mice(data(), seed = input$seed, m = input$m, maxit = input$maxit, predictorMatrix = pred(), ",
            input$add_args,
            ")"
          )
        ))
      }, ignoreNULL = FALSE)
    # TODO: validate additional arguments with formals(mice::mice)?
    return(reactive(imp()))
  })
}
    
## To be copied in the UI
# mod_02_model_ui("02_model_ui_1")
    
## To be copied in the server
# mod_02_model_server("02_model_ui_1", data)
