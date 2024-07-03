library(textNet)

filekey <- read.csv("filekey.csv")

ret_path <- "/Users/elisemiller/miniconda3/envs/spacy_condaenv/bin/python"
generate_phrases <- FALSE
keep_hyph_together <- FALSE
parse_from_file <- F
use_filtered_parsefiles <- T

#mini_data uses only a sample of pages to make testing and editing go faster
source(filekey[filekey$var_name=="generate_proper_names_function",]$filepath)

   pages <- readRDS(filekey[filekey$var_name=="cleaned_pdfs_for_govnetpaper",]$filepath)
   file_ids <- unlist(sapply(1:length(pages), function(q) rep(names(pages[q]),length(pages[[q]]))))
   file_ids <- stringr::str_extract(file_ids,'[0-9]{1,}')
   pages <- unlist(pages)
   
   unique_files <- unique(file_ids)
   
   parsed_filenames <- paste0(filekey[filekey$var_name=="parsed_files_govnetpaper",]$filepath,unique_files)
   
   if(use_filtered_parsefiles==T){
      nodeedge_filenames <- paste0(filekey[filekey$var_name=="nondisambiged_filtered_extracts_superpaper",]$filepath,unique_files,".RDS")
      
   }else{
      nodeedge_filenames <- paste0(filekey[filekey$var_name=="nondisambiged_unfiltered_extracts_superpaper",]$filepath,unique_files,".RDS")
      
   }
   


#ifelse does not work here since there are two different classes
if(generate_phrases==T){
   phrases_to_concatenate <- generate_proper_names(underscore=F,to_lower=F)
}else{
   phrases_to_concatenate <- NA
}

#### overwrite existing results?
overwrite = T

all_parsed <- textNet::parse_text(ret_path, keep_hyph_together, phrases_to_concatenate, 
                  concatenator="_",
                  pages, file_ids, parsed_filenames, 
                 overwrite)


if(use_filtered_parsefiles==T){
   myfilteredparse <- vector(mode = "list", length = length(parsed_filenames))
   for(m in 1:length(parsed_filenames)){
      myparse <- readRDS(parsed_filenames[m])
      myfilteredparse[[m]] <- textNet::filter_sentences(myparse)
      print(paste0("The length of myparse was ", nrow(myparse)))
      print(paste0("the length of the filtered parse is ", nrow(myfilteredparse[[m]])))
   }
   saveRDS(myfilteredparse, file = filekey[filekey$var_name=="parsed_files_filtered_superpaper",]$filepath)
   for(m in 1:length(myfilteredparse)){
      textNet::textnet_extract(myfilteredparse[[m]],concatenator="_",file = nodeedge_filenames[m],cl=4,
                               keep_entities = c('ORG','GPE','PERSON'), 
                               return_to_memory=F, keep_incomplete_edges=T)
   }
}else{
   for(m in 1:length(all_parsed)){
      textNet::textnet_extract(all_parsed[[m]],concatenator="_",file = nodeedge_filenames[m],cl=4,
                               keep_entities = c('ORG','GPE','PERSON'), 
                               return_to_memory=F, keep_incomplete_edges=T)
   }
}






gspids <- stringr::str_extract(nodeedge_filenames,'[0-9]{1,}')
pctalphaineng <- vector(mode="numeric",length=length(gspids))
pctlettersineng <- vector(mode="numeric",length=length(gspids))
for(m in 1:length(gspids)){
   parsed <- readRDS(paste0(filekey[filekey$var_name=="parsed_files_govnetpaper",]$filepath,gspids[m]))
   alphatokens <- parsed$token[str_detect(parsed$token, "[[:alpha:]]")]
   lettertokens <- parsed$token[str_detect(parsed$token, "[a-zA-Z]")]
   pctalphaineng[m] <- sum(alphatokens %in% eng_words)/length(alphatokens) 
   pctlettersineng[m] <- sum(lettertokens %in% eng_words)/length(lettertokens) 
   print(paste0("GSPID ",gspids[m]," has ",pctlettersineng[m] ," % of its letter-containing tokens in the English dictionary."))
   print(paste0("GSPID ",gspids[m]," has ",pctalphaineng[m] ," % of its alpha-containing tokens in the English dictionary."))
   
}

plot(gspids,pctalphaineng,type="l",col="red")
lines(gspids,pctlettersineng,col="blue")
#gspids5 ("0012"), 14 ("0021"), 34 ("0048"), 68 ("0089"), 103 ("0129"), and 108 ("0134")
#identified as having a low percentage of letter-containing words in the 
#English language.
#Of these, only 68 ("0089") was below 50%.
dif <- pctalphaineng - pctlettersineng
plot(gspids,dif)
#gspid29 ("0042") and gspid34 ("0048") are outliers,
#but the diference is no more than 7%, so this was
#considered acceptable tolerance.
