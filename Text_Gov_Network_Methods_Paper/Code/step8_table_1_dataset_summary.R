library(dplyr)
filekey <- read.csv("filekey.csv")
edges_and_nodes <- list.files(path = filekey[filekey$var_name=="disambiged_extracts_govnetpaper",]$filepath, full.names = T)
gspids <- substr(edges_and_nodes, 18,21)

length(gspids) # = number of plans


parsedfilename <- filekey[filekey$var_name=="parsed_files_govnetpaper",]$filepath
parsedfilenamesplits <- unlist(strsplit(parsedfilename,split="/"))
parsedpath <- paste(parsedfilenamesplits[1:(length(parsedfilenamesplits)-1)],collapse = "/")
parsedpattern <- parsedfilenamesplits[length(parsedfilenamesplits)]

all_parsed <- list.files(path = parsedpath, pattern = parsedpattern, full.names = T)

#removing identical plan 39 ("0053") and 69 ("0089")
all_parsed <- all_parsed[c(1:38,40:67,69:119)]
num_tokens <- 0
num_sentences <- 0
for(i in 1:length(all_parsed)){
   parsed_i <- readRDS(all_parsed[i])
   num_sentences <- num_sentences+ nrow(parsed_i %>% dplyr::count(doc_id, sentence_id))
   num_tokens <- num_tokens + nrow(parsed_i)
}

num_sentences
num_tokens # = number of tokens

meta <- readRDS(filekey[filekey$var_name=="gsp_docs_lean",]$filepath)
#removing duplicate and poorly formatted pdf
meta <- meta[!(meta$gsp_id %in% c("0089","0053")),]
num_pages <- sum(!meta$is_comment & !meta$is_reference)
num_pages
