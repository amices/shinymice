# md pattern plot, see https://stackoverflow.com/questions/32220446/how-to-plot-a-matrix-in-a-fixed-grid-pattern-in-r
# The columns A, B and C are either 0 (missing) or 1 (observed). The first column provides the frequency of each pattern. The last column lists the number of missing entries per pattern. The bottom row provides the number of missing entries per variable, and the total number of missing cells. 
md <- md.pattern(boys)
#colnames(md) <- c(colnames(md)[-dim(md)[2]], "nmis") 
#md <- md %>% cbind(freq = as.numeric(row.names(.)))
md[1:nrow(md)-1,1:ncol(md)-1] %>% 
  as.data.frame() %>% 
  cbind(id = 1:nrow(.)) %>% 
  tidyr::pivot_longer(cols = -id) %>% 
  ggplot(aes(x= name, y = id, fill = factor(value))) + 
  geom_tile()

temp <- md[1:nrow(md)-1,1:ncol(md)-1] %>% 
  as.data.frame()

dd <- expand.grid(x = 1:ncol(temp), y = 1:nrow(temp))
dd$col <- unlist(c(temp))
ggplot(dd, aes(x = x, y = y, fill = factor(col))) + geom_tile() 
