#' ui utility
#'
#' @description A shiny utility
#'
#' @param Internal parameters for the utility.
#'
#' @return The return value, if any, from executing the utility.
#'
#' @export

# remove break (opposite of br())
no_br <- function() {
  div(style = "margin-bottom: -15px")
}

# select input with defaults
select_var <- function(ns_id, ...) {
  div("Select a variable:", 
      selectInput(
    ns_id,
    label = NULL,
    choices = names(mice::boys),
    width = 200,
    ...
  ),
  style = "display:inline-block; margin-left: 20px; margin-top: 10px")
}

# select number with defaults
set_number <- function(ns_id, val, ...) {
  numericInput(
    ns_id,
    label = NULL,
    value = val,
    min = 1,
    width = 100,
    ...
  )
}

# show NAs in tables
options("DT.TOJSON_ARGS" = list(na = "string"))