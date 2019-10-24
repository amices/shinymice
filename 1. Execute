library(mice)
library(mvtnorm)
set.seed(321)

# create data
sigma <- matrix(c(0, 1, 1, 0), 2, 2)
simdata <- rmvnorm::mvtnorm(, sigma = sigma)


# function to make missing
make.missing <- function(){
  
}
data <- make.missing()

# object to store sims
OUT <- list()

# simulation function
simulate.function <- function(data, n.iter, nsim = 100){
  set.seed(123)
  #object to fill in
  out <- list()
  for (i in 1:nsim){
    #impute
    out[[i]] <- mice(maxit = n.iter, method = "norm")
  }
}
# start simulation
max.it <- 100

# simulate for each in max.it
for(i in 1:max.it){
  OUT[[i]] <- simulate.function(data, n.iter = max.it[i])
}


# Evaluate
evaluate.function <- function(OUT){
  eval.out <- list()
  for (j in OUT[[i]]){
    #calculate your evaluations vector - so Rhat, bias, cov and ciw
    eval.out[[i]] <- yourvector
    # 2. unlist(eval.out)
    # 3. Reduce("+", eval.out) / length(eval.out)
  }
}
#sapply is 'sneller?' en geeft je direct matrix
#sapply(out, evaluate.function) en dan colMeans
  
# evaluate for each simulated iteration in n.iter
# so 100 averaged (in evaluate.function()) simulations
# or use purrr::map()
for (i in OUT){
 evaluate.function
}