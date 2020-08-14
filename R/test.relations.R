# test the relation between a variable and missingness indicators
test_NA_y <- function(data, x) {
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
    .[-1, ]
  #model <- broom::tidy(anova(empty_model, full_model))
  if (is.numeric(d[[x]])) {
    ordered <- coef %>% dplyr::arrange(desc(abs(estimate)))
    #not in the right order for log reg, see https://rpubs.com/rslbliss/r_logistic_ws
  } else {
    ordered <- coef %>% dplyr::arrange(desc(estimate))
  }
  top3 <- head(ordered$term, 3) %>% gsub("TRUE", "", .)
  return(list(top3 = top3, ests = ordered))
}

# lm.beta <- function (model) 
# {
#   b <- summary(model)$coef[-1,1]
#   sx <- sapply(model$model[-1], sd)
#   sy <- sapply(model$model[1], sd)
#   beta <- b * sx /  sy
#   return(beta)
# }

#boys %>% test_relations(x = "age")

#purrr::map(names(boys) %>% setNames(., .), ~ test_relations(boys, x = .x))

# test_NA_x <- function(data, x){
# # chisq.test(is.na(boys$hgt), boys$gen)
# glm(as.formula(paste0("is.na(", x, ") ~ .")) , data, family = "binomial", maxit = 100) %>%
#   broom::tidy() %>%
#   .[-1,] %>%
#   dplyr::arrange(desc(estimate)) %>%
#   mutate(estimate = exp(estimate))
# }
#
#
# boys %>% test_NA_x(x = "tv")
data = boys
x = "hgt"
fam <- ifelse(is.numeric(data[[x]]), "gaussian", "binomial")
data %>% 
  mutate(across(where(is.numeric), scale)) %>% 
  glm(as.formula(paste(x, "~ .")), data = ., family = fam) %>% 
  broom::tidy() %>% 
  .[-1,] %>% 
  dplyr::arrange(desc(abs(estimate)))

# for plotting of logistic regression results, see https://rkabacoff.github.io/datavis/Models.html#logistic-regression