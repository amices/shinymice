# test the relation between a variable and missingness indicators
test_NA_y <- function(data, x) {
  # preprocess
  d <- data %>% mutate(dplyr::across(-x, is.na))
  if (is.numeric(d[[x]])) {
    fam <- "gaussian"
  } else {
    fam <- "binomial"
  }
  # run analysis
  coef <- glm(as.formula(paste(x, "~ .")), d, family = fam) %>%
    broom::tidy() %>%
    # drop intercept
    .[-1,]
  # clean-up output to print
  if (is.numeric(d[[x]])) {
    ordered <- coef %>% dplyr::arrange(desc(abs(estimate)))
    #not in the right order for log reg, see https://rpubs.com/rslbliss/r_logistic_ws
  } else {
    ordered <- coef %>% dplyr::arrange(desc(estimate))
  }
  top3 <- head(ordered$term, 3) %>% gsub("TRUE", "", .)
  return(list(top3 = top3, ests = ordered))
}

# # test the other way around (does not work!!)
# for plotting of logistic regression results, see https://rkabacoff.github.io/datavis/Models.html#logistic-regression
# test_NA_x <- function(data, x){
# glm(as.formula(paste0("is.na(", x, ") ~ .")) , data, family = "binomial", maxit = 100) %>%
#   broom::tidy() %>%
#   .[-1,] %>%
#   dplyr::arrange(desc(estimate)) %>%
#   mutate(estimate = exp(estimate))
# }

# function to select variables for the imputation model
test_predictors <- function(data, x) {
  # preprocess
  fam <- ifelse(is.numeric(data[[x]]), "gaussian", "binomial")
  # run analyses
  coeff <- data %>%
    mutate(across(where(is.numeric), scale)) %>%
    glm(as.formula(paste(x, "~ .")), data = ., family = fam) %>%
    broom::tidy() %>%
    .[-1, ] %>%
    dplyr::arrange(desc(abs(estimate)))
  # tidy up output
  out <- purrr::map_df(names(data) %>%
                         setNames(names(data)), function(x) {
                           grep(x, coeff$term) %>%
                             cbind(ord = ., var = x) %>%
                             as.data.frame()
                         }) %>%
    dplyr::arrange(as.numeric(ord))
  # extract best predictors to print
  out <- unique(out$var)
  is.na(out) <- length(out)
  return(out[1:3])
}
