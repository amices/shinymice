mids2spss <- function(imp,
                      filename = "midsdata",
                      path = getwd(),
                      compress = FALSE,
                      silent = FALSE, ...) {
  .id <- NULL # avoid empty global variable binding
  #extract a completed dataset (long format - all imputations stacked)
  #rename the .imp variable to imputation_, such that SPSS can identify a multiply imputed dataset
  out <-
    imp %>%
    mice::complete(action = "long", include = TRUE) %>%
    dplyr::select(-.id) %>%
    #write the data to a .sav file with package haven and print (optional) the saved location
    if (!compress) {
      whereto <- paste(path, "/", filename, ".sav", sep = "")
    } else {
      whereto <- paste(path, "/", filename, ".zsav", sep = "")
    }
  haven::write_sav(data = out,
                   path = whereto,
                   compress = compress)
  if (!silent) {
    cat("SPSS file written to", whereto, "\n")
  }
}
