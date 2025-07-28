library(pbapply)
library(stringr)
filekey <- read.csv("filekey.csv")

parsed_filenames <- list.files(path = "data/Text_Gov_Network_Methods_Paper/parsed", pattern = "parsed_",
                              full.names = T)
for(i in 1:length(parsed_filenames)){
   mydf <- readRDS(parsed_filenames[i])
   #in universal dependencies, the arrow goes from the head to the dependent
   mydf$targetID <- paste0(mydf$doc_id, "_", mydf$sentence_id, "_", mydf$token_id)
   mydf$sourceID <- paste0(mydf$doc_id, "_", mydf$sentence_id, "_", mydf$head_token_id)
   #we want to collapse all targets and source nodes with the same token; that is,
   #all nodes representing the word "groundwater" are collapsed
   mydf$target <- mydf$lemma
   gspid <- stringr::str_remove(list.files(path = "data/Text_Gov_Network_Methods_Paper/parsed", pattern = "parsed_",
                                           full.names = F)[i], "parsed_")
   print(paste0("Extracting network from GSP ID ", gspid))
   #we find the token (target) representing the head token (source) and collect its name
   mydf$source <- unlist(pblapply(1:nrow(mydf), function(j){
      mydf[mydf$targetID == mydf$sourceID[j],]$lemma
   }))
   saveRDS(mydf, paste0("data/Innovation_Paper/", "unfiltered_dependency_edgelist_",gspid,".RDS"))
   
}

