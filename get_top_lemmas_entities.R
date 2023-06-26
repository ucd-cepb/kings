#find top lemmas and entities

gsp_text_with_meta <- readRDS("prepped_for_sna")
gspids <- unique(gsp_text_with_meta$gsp_id)
rm(gsp_text_with_meta)

files <- sapply(gspids, function (i) paste0("data_output/full_directed_graph_",i))

top_feats <- textNet::top_features(files,from_file=T)

saveRDS(top_feats, "data_output/feature_prevalence_in_corpus_normalized")