# descriptive statistics for incomplete data
options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))
mis_descr <- function(d){
  tab <- psych::describe(d)[,c("n", "mean", "sd", "min", "max", "median")] %>% 
    cbind(variable = rownames(.), ., n_missing = as.integer(nrow(d) - .$n))
  tab[,2] <- as.integer(tab[,2])
  return(tab)
}