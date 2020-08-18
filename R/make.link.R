shinyLink <- function(to, label) {
  tags$a(
    class = "shiny__link",
    href = to,
    label
  )
}