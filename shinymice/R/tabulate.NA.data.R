# table with bold, red NA values
DT_NA_highlight <- function(dat, varnames) {
  dat %>%
    dplyr::mutate_if(is.numeric, round, 2) %>%
    datatable(options = list(pageLength = 5)) %>%
    formatStyle(
      varnames,
      target = "cell",
      color = styleEqual("NA", "#B61A51"),
      fontWeight = styleEqual("NA", "bold")
    )
}
