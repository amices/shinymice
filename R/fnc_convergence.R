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

OG <- out %>% 
  # get rhat
  group_by(m) %>% 
  summarise(mean = mean(.data[["mu.Y"]]), var = var(.data[["mu.Y"]])) %>% 
  summarise(B = var(mean) * nit, W = mean(var)) %>% 
  mutate(varplus = (((nit-1)/nit)*W) + B/nit,
         rhat = sqrt(varplus/W)) %>% 
  select(rhat) %>% 
  as.double()

bulk <- out %>% 
# split chains
  mutate(m = m + c(rep(0, ceiling(nit/2)), rep(ceiling(nit/2), floor(nit/2)))) %>% 
# rank-normalize
  mutate("mu.Y" = qnorm((rank(.data[["mu.Y"]])-3/8)/(nrow(out)-1/4))) %>% 
# get rhat
  group_by(m) %>% 
  summarise(mean = mean(.data[["mu.Y"]]), var = var(.data[["mu.Y"]])) %>% 
  summarise(B = var(mean) * nit, W = mean(var)) %>% 
  mutate(varplus = (((nit-1)/nit)*W) + B/nit,
         rhat = sqrt(varplus/W))  %>% 
  select(rhat) %>% 
  as.double()

tails <- out %>% 
  # fold chains
  mutate("mu.Y" = abs(.data[["mu.Y"]] - median(.data[["mu.Y"]]))) %>% 
  # split chains
  mutate(m = m + c(rep(0, ceiling(nit/2)), rep(ceiling(nit/2), floor(nit/2)))) %>% 
  # rank-normalize
  mutate("mu.Y" = qnorm((rank(.data[["mu.Y"]])-3/8)/(nrow(out)-1/4))) %>% 
  # get rhat
  group_by(m) %>% 
  summarise(mean = mean(.data[["mu.Y"]]), var = var(.data[["mu.Y"]])) %>% 
  summarise(B = var(mean) * nit, W = mean(var)) %>% 
  mutate(varplus = (((nit-1)/nit)*W) + B/nit,
         rhat = sqrt(varplus/W))  %>% 
  select(rhat) %>% 
  as.double()
