#find top lemmas and entities

gsp_text_with_meta <- readRDS("prepped_for_sna")
gspids <- unique(gsp_text_with_meta$gsp_id)
rm(gsp_text_with_meta)

#not including GSP 0089
files <- sapply(c(gspids[1:67],gspids[69:length(gspids)]), function (i) paste0("data_output/full_directed_graph_",i))
   
top_feats <- top_features(files,from_file=T)
   

saveRDS(top_feats, "data_output/feature_prevalence_in_corpus_normalized")
