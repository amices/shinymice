# test the relation between a variable and missingness indicators
test_relations <- function(data, x) {
  d <- data %>% mutate(dplyr::across(-x, is.na))
  empty_model <- lm(d[[x]] ~ 1)
  full_model <- d %>% lm(d[[x]] ~ . - d[[x]], data = .)
  model <- broom::tidy(anova(empty_model, full_model))
  coeff <-
    broom::tidy(full_model) %>% dplyr::arrange(desc(abs(estimate)))
  return(list(model = model, coeff = coeff))
}

boys %>% test_relations(x = "age")
