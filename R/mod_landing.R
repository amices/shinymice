# landing page module
homeUI <- function(id) {
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
      ),
      "For the most recent version of this app or to report an issue, see ",
      tags$a(href = "https://github.com/amices/shinyMice", "github.com/amices/shinyMice")
    )
  )}

# homeServer <- function(id, dat, r) {
#   moduleServer(id, function(input, output, session, data = dat, rv = r) {
#   })}