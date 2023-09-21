meta_small <- readRDS(list.files(path = "data_temp", pattern = "meta_small", full.names = T)[length(
   list.files(path = "data_temp", pattern = "meta_small", full.names = T))])
meta_tiny <- unique(meta_small[,11:20])

library(GGally)

ggpairs(meta_tiny)
