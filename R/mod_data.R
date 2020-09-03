dataUI <- function(id, dat) {
  tagList(
           tabsetPanel(
             tabPanel(
               "Table",
               br(),
               # add that it's a subset
               helpText("Hint: Sort variables descending to view missing values."),
               DT::DTOutput(NS(id,"table"))
             ),
             tabPanel(
               "Descriptives",
               br(),
               #tags$b(textOutput("n")),
               helpText("Categorical variables denoted with '*'"),
               br(),
               DT::DTOutput(NS(id,"descr")),
               DT::DTOutput(NS(id,"categ"))
             )#,
             # tabPanel("Test",
             #          plotUI("hist1", dat = mice::boys))
           ))
  
}

dataServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
  
  stopifnot(is.reactive(data))
    
    vars = names(data())
    
  output$table <-
  renderDT({
    DT_NA_highlight(data(), vars())
  }, server = F)

#output$n <- renderText(paste0("Descriptive statistics (n = ", nrow(data()), ")"))
output$descr <- renderDT(
  descr(data) %>% 
    dplyr::mutate_if(is.numeric, round, 2) %>%
    DT::datatable(options = list(pageLength = 5)) %>%
    DT::formatStyle(
      "n",
      target = "cell",
      color = DT::styleInterval(c(nrow(data)-1), c("#B61A51", "black")),
      fontWeight = DT::styleInterval(c(nrow(data)-1), c("bold", "")),
    )
  
)

output$categ <- renderDT(least_perc(data))

})}