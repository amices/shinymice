# table with bold, red NA values
DT_NA_highlight <- function(dat, varnames) {
  dat %>%
    dplyr::mutate_if(is.numeric, round, 2) %>%
    DT::datatable(options = list(pageLength = 5)) %>%
    DT::formatStyle(
      varnames,
      target = "cell",
      color = DT::styleEqual("NA", "#B61A51"),
      fontWeight = DT::styleEqual("NA", "bold")
    )
}
