#this was run to remove non a-z letters before running script 2_process_edgelists_nodelists
#because the clean_entities function hadn't yet been updated. This is now redundant
#with the textnet_extract() functionality
filekey <- read.csv("filekey.csv")

netextracts <- filekey[filekey$var_name=="nondisambiged_extracts_govnetpaper",]$filepath
netextracts <- substr(netextracts, 1, nchar(netextracts)-1)
edges_and_nodes <- list.files(path = netextracts, full.names = T)
gspids <- stringr::str_extract(edges_and_nodes,'[0-9]{1,}')

for(m in 1:length(gspids)){
   en <- readRDS(edges_and_nodes[m])
   edgelist <- en$edgelist
   nodelist <- en$nodelist
   verblist <- en$verblist
   
   edgelist$source <- clean_entities(edgelist$source)
   edgelist$target <- clean_entities(edgelist$target)
   nodelist$entity_cat <- clean_entities(nodelist$entity_cat)
   verblist$head_verb_lemma <- clean_entities(verblist$head_verb_lemma)
   
   #remove empty strings 
   edgelist$source <- ifelse(is.na(edgelist$source),
                                             edgelist$source,
                                             ifelse(nchar(edgelist$source)==0,NA,
                                                    edgelist$source))
   edgelist$target <- ifelse(is.na(edgelist$target),
                                             edgelist$target,
                                             ifelse(nchar(edgelist$target)==0,NA,
                                                    edgelist$target))
   nodelist <- nodelist[nchar(nodelist$entity_cat)>0]
   verblist <- verblist[nchar(verblist$head_verb_lemma)>0]
   #remove any incomplete edges that may have resulted from the cleaning process
   edgelist$edgeiscomplete <- !is.na(edgelist$source) & !is.na(edgelist$target)
   edgelist[, `:=`(hascompleteedge, any(edgeiscomplete==T)), by = c("doc_sent_verb")]
   edgelist <- edgelist %>% filter((hascompleteedge==T & edgeiscomplete==T) | hascompleteedge==F)
   edgelist$hascompleteedge <- NULL
   
   en$edgelist <- edgelist
   en$nodelist <- nodelist
   en$verblist <- verblist
   
   saveRDS(en, edges_and_nodes[m])
   
   
}