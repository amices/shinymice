#' Title Save imputations in SPSS readable format
#'
#' @param imp A multiply imputed data set (mids) object
#' @param file Temporary file location needed for shiny::downloadHandler()
#'
#' @return A SPPS readable multiply imputed data set
mids2spss <- function(imp, file) {
  .id <- NULL # avoid empty global variable binding
  #extract a completed dataset (long format - all imputations stacked)
  #rename the .imp variable to imputation_, such that SPSS can identify a multiply imputed dataset
  out <-
    imp %>%
    mice::complete(action = "long", include = TRUE) %>%
    dplyr::select(-.id) %>%
    dplyr::rename("Imputation_" = ".imp")
  haven::write_sav(out, file)
} # what to do after export? See https://bookdown.org/mwheymans/bookmi/multiple-imputation.html#multiple-imputation-in-spss
