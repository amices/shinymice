# Extract aggregated simulation results by averaging over runs
# requires the packages "purrr" and "data.table"

evaluate.sim <-
  function(sims,
           n.iter,
           mean_or_SE = "mean") {
    # determine whether we want averages per simulation condition or MCMC SEs
    if (mean_or_SE == "se") {
      function_to_apply <-
        function(x)
          sd(x, na.rm = TRUE) # or use: function(x) sqrt((var(x)))
    } else if (mean_or_SE == "lower") {
      function_to_apply <- function(x)
        quantile(x, .025, na.rm = TRUE)
    } else if (mean_or_SE == "upper") {
      function_to_apply <- function(x)
        quantile(x, .975, na.rm = TRUE)
    } else {
      function_to_apply <- function(x)
        mean(x, na.rm = TRUE)
    }
    
    
    # organize output for evaluation
    dt <-
      map(sims, as.data.table) %>% rbindlist(fill = TRUE) #%>% group_by(t,p)
    # averages
    res <-
      aggregate(. ~ t + p, dt, function_to_apply, na.action = na.pass) %>% dplyr::na_if(., "NaN")
    
    # convergence
    #pcas <- dt[, grep("pca", names(dt)), with = FALSE] %>% cbind(dt[,1:2])
    
    ### this doesn't work!
    #function_to_apply <- ac_lag1
    #aggregate(.~t+p, pcas, function_to_apply, na.action = na.pass) %>% dplyr::na_if(., "NaN")
    ###
    
    thetas <-
      c(
        "chain.mean.X1",
        "chain.mean.X2",
        "chain.mean.X3",
        "chain.mean.Y",
        "chain.var.X1",
        "chain.var.X2",
        "chain.var.X3",
        "chain.var.Y",
        "pca"
      )
    
    #### THIS WORKS!
    # a <-
    #   map(sims, function(rpt) {
    #     map(thetas, function(vrb) {
    #       rpt[, grep(vrb, names(rpt))] %>% cbind(rpt[, 1:2], .) %>% base::split(., as.factor(.$p))
    #     })
    #   }) 
    # 
    # map(c(.05, .25, .5, .75, .95), function(msp){
    #   map(2:n.iter, function(itr) {
    #     a[[1]][[1]][[msp]][1:itr, 3:7] %>% ac_lag1()
    #   })
    # })
    
    a <-
      map(sims, function(rpt) {
        map(thetas, function(vrb) {
          rpt[, grep(vrb, names(rpt))] %>% cbind(rpt[, 1:2], .) %>% base::split(., as.factor(.$p))
        })
      }) 
    
    map(c(.05, .25, .5, .75, .95), function(msp){
      map(2:n.iter, function(itr) {
        a[[1]][[1]][[msp]][1:itr, 3:7] %>% ac_lag1()
      })
    })
    ### decided to remove group_by, so this doesn't work anymore:
    # a[[1]][[1]] %>% summarise(sums = ac_lag1(chain.mean.X1.Chain.1))
    # a[[1]][[1]] %>%  summarise_all(~ac_lag1(.))
    # map(2:n.iter, function(z) {a[[1]][[1]][1:z,] %>% summarise(acs = ac_lag1(chain.mean.X1.Chain.1))})
    # map(2:n.iter, function(z) {a[[1]][[1]][1:z,] %>% summarise_all(~ac_lag1(.))})
    
    
    # # apply function to aggregate simulation runs per simulation condition
    # d <- dt[, lapply(.SD, function_to_apply)] %>% as.data.frame
    
    # # create object for output
    # output <- 1:n.iter %>% as.data.frame()
    
    # # add names
    # vars <- names(sims[[1]][[1]])
    # for (var in vars) {
    #   output <- cbind(output, d[, grep(var, names(d))] %>% t)
    # }
    # rownames(output) <- NULL
    # colnames(output) <- c("T", vars)
    
    #output
    return(res)
  }
