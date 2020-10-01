# convergence function

# load("~/shinyMice/data/testmids.Rdata")

ext = "mu."
nit <- mids$iteration
out <- mids$chainMean %>%
  as.data.frame() %>%
  t() %>%
  as.data.frame() %>%
  setNames(paste0(ext, names(.))) %>%
  cbind(it = 1:nit,
        m = rep(1:mids$m, each = nit),
        .)
rownames(out) <- NULL
out

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

purrr::map_dfc(names(out)[-c(1:2)], function(vrb){
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
}) %>% cbind(it = nit, .)
