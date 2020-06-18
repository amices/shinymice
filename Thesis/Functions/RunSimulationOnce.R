# function to run all of the simulation conditions once 
# requires the packages 'dplyr'/'magrittr', 'mice', and 'purrr'
# and the adapted imputation function

# load function
source("Thesis/Functions/ImputeMissingness.R")

# combine separate functions into wrapper
simulate <- function(complete_data = data,
                     amp_patterns = amp.pat,
                     mis_prop = p.miss,
                     final_it = n.iter,
                     ...) {
  # input: the simulated complete dataset, the missingness pattern,
  # the proportion of incomplete cases to consider, and 
  # the maximum number of iterations to consider
  # output: results for a single run of the simulation conditions 
  
  # ampute the complete data with each missingness proportion
  amps <- map(mis_prop, function(mis) {
    ampute(
      data = complete_data,
      prop = mis,
      patterns = amp_patterns,
      mech = "MCAR"
    )$amp
  }) %>% set_names(as.character(mis_prop))
  
  # with amputed datasets (as many as there are missingness proportions), impute missingness and compute diagnostics for every nr. of iterations
  imps <-
    map_df(mis_prop, function(mis) {
      map_df(1:n.iter, function(it) {
        test.impute(amp_data = amps[[as.character(mis)]],
                    it_nr = it,
                    final_it = n.iter) %>% cbind(p = mis, .)
      }) %>% cbind(chain_means, chain_vars)
    })
  # save seed in global environment
  seed <<- .Random.seed
  
  return(imps)
}
