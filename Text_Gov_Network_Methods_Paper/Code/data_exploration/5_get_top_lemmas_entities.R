#find top lemmas and entities

edges_and_nodes <- list.files(path = "cleaned_extracts", full.names = T)
gspids <- substr(edges_and_nodes, 18,21)

#not including GSP 0089 for formatting reasons or 0053, which is a duplicate
files <- sapply(c(gspids[1:38],gspids[40:67],gspids[69:length(gspids)]), function (i) paste0("data/output_large_files/full_directed_graph_",i))
   
top_feats <- top_features(files,from_file=T)
   

saveRDS(top_feats, "data/output_large_files/feature_prevalence_in_corpus_normalized")
