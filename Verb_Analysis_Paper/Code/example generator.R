filekey <- read.csv("filekey.csv")

edgelist_w_meta$sentence_id <- stringr::str_remove(edgelist_w_meta$doc_sent_verb,".*pdf")
edgelist_w_meta$sentence_id <- stringr::str_remove(edgelist_w_meta$sentence_id, "\\d*_")
edgelist_w_meta$sentence_id <- as.numeric(stringr::str_remove(edgelist_w_meta$sentence_id, "_\\d*"))
edgelist_w_meta$doc_id <- edgelist_w_meta$page_num
edgelist_w_meta <- edgelist_w_meta[!edgelist_w_meta$gsp_id %in% c("0053", "0089"),]

types <- colnames(edgelist_w_meta)[
   stringr::str_detect(colnames(edgelist_w_meta),
                       "type_\\d")
]
verb_examples <- vector(mode = "list", length = length(types))
set.seed(9876)
for(i in seq_along(types)){
   selectedverbs <- edgelist_w_meta[as.vector(
      edgelist_w_meta[,get(types[i])]==T),]
   print(types[i])
   if(nrow(selectedverbs)>0){
      verbfreq <- sort(table(selectedverbs$head_verb_lemma),decreasing = T)
      mostcommon <- which(selectedverbs$head_verb_lemma==names(verbfreq[1]))
      if(length(verbfreq)>1){
         nextcommon <- which(selectedverbs$head_verb_lemma==names(verbfreq[2]))
      }
      verb_examples[[i]] <- list(name = types[i], verbfreq = verbfreq,
                                 examples_1 = NULL, examples_2 = NULL)
      
      if(length(mostcommon)>2){
         rownum <- sample(mostcommon, 3, replace = F)
      }else{
         rownum <- sample(mostcommon, length(mostcommon), replace = F)
      }
      
      print(names(verbfreq[1]))
      for(j in 1:length(rownum)){
         parsed <- readRDS(paste0(filekey[filekey$var_name=="parsed_files_govnetpaper",]$filepath, selectedverbs[rownum[j],]$gsp_id))
         parsed$doc_id <- as.numeric(stringr::str_remove(parsed$doc_id, ".*.pdf"))
         mysent <- paste(str_squish(parsed[parsed$doc_id==selectedverbs[rownum[j],]$doc_id & 
                                              parsed$sentence_id==selectedverbs[rownum[j],]$sentence_id,]$token),
                         collapse = " ")
         print(mysent)
         verb_examples[[i]]$examples_1 <- append(verb_examples[[i]]$examples_1, mysent)
      }
      if(length(verbfreq)>1){
         if(length(nextcommon)>2){
            rownum <- sample(nextcommon, 3, replace = F)
         }else{
            rownum <- sample(nextcommon, length(nextcommon), replace = F)
         }
         
         print(names(verbfreq[2]))
         for(j in 1:length(rownum)){
            parsed <- readRDS(paste0(filekey[filekey$var_name=="parsed_files_govnetpaper",]$filepath, selectedverbs[rownum[j],]$gsp_id))
            parsed$doc_id <- as.numeric(stringr::str_remove(parsed$doc_id, ".*.pdf"))
            mysent <- paste(str_squish(parsed[parsed$doc_id==selectedverbs[rownum[j],]$doc_id & 
                                                 parsed$sentence_id==selectedverbs[rownum[j],]$sentence_id,]$token),
                            collapse = " ")
            print(mysent)
            verb_examples[[i]]$examples_2 <- append(verb_examples[[i]]$examples_2, mysent)
         }
      }
   }
   
}

saveRDS(verb_examples, filekey[filekey$var_name=="verb_examples_verbpaper",]$filepath)




textNet::verb_classifications[
   textNet::verb_classifications$verb=="partner"]



#hedging examples
   selectedverbs <- edgelist_w_meta[edgelist_w_meta$has_hedge == T,]
   rownum <- sample(1:nrow(selectedverbs), 15, replace = F)
   
   for(j in 1:length(rownum)){
      parsed <- readRDS(paste0(filekey[filekey$var_name=="parsed_files_govnetpaper",]$filepath, selectedverbs[rownum[j],]$gsp_id))
      parsed$doc_id <- as.numeric(stringr::str_remove(parsed$doc_id, ".*.pdf"))
      mysent <- paste(str_squish(parsed[parsed$doc_id==selectedverbs[rownum[j],]$doc_id & 
                                           parsed$sentence_id==selectedverbs[rownum[j],]$sentence_id,]$token),
                      collapse = " ")
      print(mysent)
   }
   
   




