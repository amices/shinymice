# test the relation between a variable and missingness indicators
test_relations <- function(data, x) {
  d <- data %>% mutate(dplyr::across(-x, is.na))
  if (is.numeric(d[[x]])) {
    fam <- "gaussian"
  } else {
    fam <- "binomial"
  }
  #empty_model <- glm(as.formula(paste(x, "~ 1")), d, family = fam)
  coef <- glm(as.formula(paste(x, "~ .")), d, family = fam) %>% 
    broom::tidy() %>% 
    # drop intercept
    .[-1,]
  #model <- broom::tidy(anova(empty_model, full_model))
  if (is.numeric(d[[x]])) {
    out <- coef %>% dplyr::arrange(desc(abs(estimate)))
    #not in the right order for log reg, see https://rpubs.com/rslbliss/r_logistic_ws
  } else {
    out <- coef %>% dplyr::arrange(desc(exp(estimate)))
  }
  out <- head(out$term, 3) %>% gsub("TRUE", "", .)
  return(out)
}

#boys %>% test_relations(x = "age")

#purrr::map(names(boys) %>% setNames(., .), ~ test_relations(boys, x = .x))
