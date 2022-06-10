library(stm)
library(tm)
library(SnowballC)
library(tidytext)
library(data.table)
library(tidyverse)
library(sf)
library(pbapply)

build_corpus <- function(gsp_text_with_meta){
   #builds corpus
   #corpus pulls documents from column 1 of gsp_text_with_meta
   #removes comments and references
   #metadata is all other columns
   #metadata: num rows = num documents. num columns = num metadata type
   #TODO add social metadata
   gsp_corpus <- VCorpus(VectorSource(gsp_text_with_meta[[1]][!is_comment&!is_reference]))
   meta(gsp_corpus, tag = "admin", type = "indexed") <- gsp_text_with_meta[[2]][!is_comment&!is_reference]
   meta(gsp_corpus, tag = "basin", type = "indexed") <- gsp_text_with_meta[[3]][!is_comment&!is_reference]
   meta(gsp_corpus, tag = "sust_criteria", type = "indexed") <- gsp_text_with_meta[[4]][!is_comment&!is_reference]
   meta(gsp_corpus, tag = "monitoring", type = "indexed") <- gsp_text_with_meta[[5]][!is_comment&!is_reference]
   meta(gsp_corpus, tag = "projects_mgmt", type = "indexed") <- gsp_text_with_meta[[6]][!is_comment&!is_reference]
   meta(gsp_corpus, tag = "gsp_id", type = "indexed") <- gsp_text_with_meta[[7]][!is_comment&!is_reference]
   meta(gsp_corpus, tag = "i", type = "indexed") <- c(1:length(gsp_corpus))
   #col names 
   #NLP::meta(txt, colnames(metadata)[i]) <- metadata[,i]
   
   
   #remove white spaces
   gsp_corpus <- tm_map(gsp_corpus, stripWhitespace)
   
   #convert to lower case
   #if else needed because of API differences, adapted from textProcessor
   if(utils::packageVersion("tm") >= "0.6") {
      gsp_corpus <- tm_map(gsp_corpus, content_transformer(tolower)) 
   } else {
      gsp_corpus <- tm_map(gsp_corpus, tolower)
   }
   
   #remove punctuation
   #ucp = T would remove larger set of punctuation
   gsp_corpus <- tm_map(gsp_corpus, removePunctuation, preserve_intra_word_dashes = TRUE,ucp=F)
   
   #TODO make sure underscores are not removed/bigrams, 
   
   #TODO custom punctuation removal would go here based on this textProcessor template
   #if(length(custompunctuation)==1 && 
   #   substr(custompunctuation,0,1)=="[") {
   #   #if there is only one entry and it starts with open bracket
   #   #we are going to assume its a regular expression and let it
   #   #through
   #   punct_pattern <- custompunctuation
   #} else {
   #   punct_pattern <-sprintf("[%s]",paste0(custompunctuation,collapse=""))
   #}
   #gsp_corpus<- tm_map(gsp_corpus, content_transformer(function(x, pattern) gsub(pattern, "", x)), 
   #                    punct_pattern)
   
   #Remove stopwords in English
   #this takes a while
   gsp_corpus <- tm_map(gsp_corpus, removeWords, stopwords("en")) 
   
   #TODO custom stopwords would be removed here
   #gsp_corpus <- tm_map(gsp_corpus, removeWords, customstopwords)
   
   #remove numbers #do this after removing punctuation
   gsp_corpus <- tm_map(gsp_corpus, removeNumbers)
   #TODO numbers (using regex) remove any number except if letter on both ends or 
   
   #stem words
   gsp_corpus <- tm_map(gsp_corpus, stemDocument, language="en")
   
   saveRDS(gsp_corpus, file = paste0("data_temp/","gsp_corpus_",format(Sys.time(), "%Y%m%d-%H:%M")))
   gsp_corpus <- readRDS(list.files(path = "data_temp", pattern = "corpus", full.names = T)[length(
      list.files(path = "data_temp", pattern = "corpus", full.names = T))])
}