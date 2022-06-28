library(stm)
library(tm)
library(SnowballC)
library(tidytext)
library(data.table)
library(tidyverse)
library(sf)
library(pbapply)
library(quanteda)

source('functions/custom_dictionary.R')

build_corpus <- function(gsp_text_with_meta){
   is_comment <- gsp_text_with_meta$is_comment
   is_reference <- gsp_text_with_meta$is_reference
      
   #builds corpus
   #corpus pulls documents from column 1 of gsp_text_with_meta
   #removes comments and references
   #metadata is all other columns
   #metadata: num rows = num documents. num columns = num metadata type
   #TODO add social metadata
   
   qcorp <- quanteda::corpus(x = gsp_text_with_meta[!is_comment&!is_reference],
                             text_field = "text")
   qtok <- quanteda::tokens(qcorp,
                            what = "word",
                            remove_punct = T,
                            remove_symbols = F,
                            remove_numbers = F,
                            remove_url = T,
                            remove_separators = T,
                            split_hyphens = F,
                            include_docvars = T,
                            padding = F,
                            verbose = T)
   #make sure punct and any symbols removed before numbers
   #TODO include custom symbols like delta we want saved
   qtok <- quanteda::tokens(qtok,
                            what = "word",
                            remove_numbers = T,
                            verbose = T)
   
   
   compounds <- custom_dictionary(c())
   
   #this takes about 3 hours
   #converts toLower, does not stem
  
   tok_1 <- quanteda::tokens_compound(qtok[1:500],pattern = phrase(compounds),
                                      concatenator = '_',valuetype = 'regex',
                                      case_insensitive=T,window = 0)
   paste0("tok 1 complete featuring rows 1:500")
   qdfm <- quanteda::dfm(tok_1, verbose = T)
   
   for(i in 2:(length(qtok)/500)){
      tok_i <- quanteda::tokens_compound(qtok[(500*(i-1)+1):(500*i)],pattern = phrase(compounds),
                                         concatenator = '_',valuetype = 'regex',
                                         case_insensitive=T,window = 0)
      print(paste0("tok", i, "complete featuring rows ", (500*(i-1)+1),":",(500*i)))
      qdfm_i <- quanteda::dfm(tok_i, verbose = T)
      qdfm <- rbind(qdfm, qdfm_i)
   }
   
   tok_n <- quanteda::tokens_compound(qtok[(floor(length(qtok)/500)*500):length(qtok)],pattern = phrase(compounds),
                                      concatenator = '_',valuetype = 'regex',
                                      case_insensitive=T,window = 0)
   paste0("tok n complete featuring rows ", (floor(length(qtok)/500)*500),":",length(qtok))
   qdfm_n <- quanteda::dfm(tok_n, verbose = T)
   qdfm <- rbind(qdfm, qdfm_n)
   
   #dfm_wordstem(qdfm, language = "en") would be used here to stem
   
   saveRDS(qdfm, file = paste0("data_temp/","gsp_tok_",format(Sys.time(), "%Y%m%d-%H:%M")))
   
   qdfm <- readRDS(list.files(path = "data_temp", pattern = "tok", full.names = T)[length(
      list.files(path = "data_temp", pattern = "tok", full.names = T))])

   qdfm_nostop <- dfm_remove(qdfm, pattern = stopwords("en"))
   
}