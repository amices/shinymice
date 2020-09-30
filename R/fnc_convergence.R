# convergence function

# load("~/shinyMice/data/testmids.Rdata")

ext = "mu."
nit <- mids$iteration
out <- mids$chainMean %>% 
  as.data.frame() %>% 
  t() %>% 
  as.data.frame() %>%
  setNames(paste0(ext, names(.))) %>%
  cbind(
    it = 1:nit,
    m = rep(1:mids$m, each = nit),
    .
  )
rownames(out) <- NULL
out

out %>% 
  group_by(m) %>% 
  summarise(mean = mean(.data[["mu.Y"]]), var = var(.data[["mu.Y"]])) %>% 
  summarise(B = var(mean) * nit, W = mean(var)) %>% 
  mutate(varplus = (((nit-1)/nit)*W) + B/nit,
         rhat = sqrt(varplus/W))
