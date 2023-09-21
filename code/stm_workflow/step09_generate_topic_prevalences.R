metadata <- inputs$meta
prev <- as.data.frame(model$theta)

topic_prev <- cbind(prev, metadata)

ntopics <- model$settings$dim$K

tps <- aggregate(as.matrix(topic_prev[,1:ntopics]), data.frame(topic_prev$gsp_id), mean)
rownames(tps) <- tps[,1]
tps <- tps[,2:31]

saveRDS(tps, "data_output/topic_prevalence")
