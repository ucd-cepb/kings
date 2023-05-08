library(network)
library(sna)
library(igraph)
library(intergraph)


model <- readRDS(list.files(path = "data_output/mdl", pattern = "model", full.names = T)[length(
      list.files(path = "data_output/mdl", pattern = "model", full.names = T))])
gsp_out <- readRDS(list.files(path = "data_temp", pattern = "slam", full.names = T)[length(
   list.files(path = "data_temp", pattern = "slam", full.names = T))])

gspids <- unique(gsp_out$meta$gsp_id)
topic_prevalences <- data.frame(matrix(NA, nrow = length(gspids), ncol=60))
rownames(topic_prevalences) <- gspids

for(m in 1:length(gspids)){
   pages <- gsp_out$meta$gsp_id == gspids[m]
   topic_prevalences[m,] <- colSums(model$theta[pages,])/sum(colSums(model$theta[pages,]))
   
}

saveRDS(topic_prevalences, "data_output/topic_prevalence")
