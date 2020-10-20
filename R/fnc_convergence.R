# convergence function

# load("~/shinyMice/data/testmids.Rdata")

# basic function
rhat <- function(d, v) {
  nit <- max(d$it)
  d %>%
    group_by(m) %>%
    summarise(mean = mean(.data[[v]]), var = var(.data[[v]])) %>%
    summarise(B = var(mean) * nit, W = mean(var)) %>%
    mutate(varplus = (((nit - 1) / nit) * W) + B / nit,
           rhat = sqrt(varplus / W)) %>%
    select(rhat)
}

# complete function incl preprocessing
max_rhat <- function(mids, param = "means") {
  if (param == "means") {
    dat <- mids$chainMean
  }
  if (param == "vars") {
    dat <- mids$chainVar
  }
  nit <- mids$iteration
  out <- dat %>%
    as.data.frame() %>%
    t() %>%
    as.data.frame() %>%
    setNames(paste0(param, ".", names(.))) %>%
    cbind(it = 1:nit,
          m = rep(1:mids$m, each = nit),
          .)
  rownames(out) <- NULL
  out
  
  # compute rhat for each variable
  purrr::map_dfr(1:max(out$it) + 1, function(itr) {
    out <- filter(out, it < itr)
    purrr::map_dfc(names(out)[-c(1:2)], function(vrb) {
      nit <- max(out$it)
      OG <- out %>%
        # get rhat
        rhat(d = ., v = vrb)
      
      bulk <- out %>%
        mutate(# split chains
          m = m +
            c(rep(0, ceiling(nit / 2)), rep(ceiling(nit / 2), floor(nit / 2))),
          # rank-normalize
          vrb = qnorm((rank(.data[[vrb]]) - 3 / 8) / (nrow(out) - 1 / 4))) %>%
        # get rhat
        rhat(d = ., v = vrb)
      
      tails <- out %>%
        mutate(
          # fold chains
          vrb = abs(.data[[vrb]] - median(.data[[vrb]])),
          # split chains
          m = m +
            c(rep(0, ceiling(nit / 2)), rep(ceiling(nit / 2), floor(nit / 2))),
          # rank-normalize
          vrb = qnorm((rank(.data[[vrb]]) - 3 / 8) / (nrow(out) - 1 / 4))
        ) %>%
        # get rhat
        rhat(d = ., v = vrb)
      
      data.frame(max(OG, bulk, tails)) %>%
        setNames(paste0("rhat.", vrb))
    }) %>% cbind(it = max(out$it), .)
    
  })
}

#max_rhat(mids) %>% full_join(max_rhat(mids, param = "vars"))
