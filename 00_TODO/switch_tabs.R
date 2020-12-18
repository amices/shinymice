library(shiny)

mod_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(ns("back"), "Test")
  )
}

mod_server <- function(id, parent_session){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    observeEvent(input$back, {
    print("Button click, go back to home tab")
    updateTabsetPanel(session = parent_session, inputId = "tabs", selected = "00")
  })
})}

ui <- navbarPage(
  "example",
  id = "tabs",
  tabPanel(
    "home",
    value = "00",
    h4("updateTabsetPanel does not work with modules"),
    h5("But the button below does"),
    actionButton("switch", "switch")
  ),
  tabPanel(
    "secondtab",
    value = "01",
    mod_ui("second")
  )
)

server <- function(input, output, session){
  mod_server("second", parent_session = session)
  observeEvent(input$switch, {
    updateTabsetPanel(session = session, inputId = "tabs", selected = "01")
  })
}

shinyApp(ui, server)