library(stm)
library(tm)
library(SnowballC)
library(tidytext)
library(data.table)
library(tidyverse)
library(sf)
library(pbapply)
library(quanteda)
library(stringi)

source('functions/custom_dictionary.R')
source('functions/generate_place_names.R')

build_corpus <- function(gsp_text_with_meta){
   is_comment <- gsp_text_with_meta$is_comment
   is_reference <- gsp_text_with_meta$is_reference
      
   #builds corpus
   #corpus pulls documents from column 1 of gsp_text_with_meta
   #removes comments and references
   #metadata is all other columns
   #metadata: num rows = num documents. num columns = num metadata type

   #format words in equations to readable text
   #removes parenthetical pieces that are attached to the end of words, eg SurfaceFlow(i)
   #to help with equation word formatting
   gsp_text_with_meta$text <- pblapply(1:length(gsp_text_with_meta$text), function(i){
      stri_replace_all_regex(gsp_text_with_meta$text[i], pattern = c("𝑎","𝑏","𝑐","𝑑","𝑒","𝑓","𝑔","ℎ","𝑖","𝑗","𝑘","𝑙","𝑚",
                                                                     "𝑛","𝑜","𝑝","𝑞","𝑟","𝑠","𝑡","𝑢","𝑣","𝑤","𝑥","𝑦","𝑧",
                                                                     "𝐴","𝐵","𝐶","𝐷","𝐸","𝐹","𝐺","𝐻","𝐼","𝐽","𝐾","𝐿","𝑀",
                                                                     "𝑁","𝑂","𝑃","𝑄","𝑅","𝑆",
                                                                     "𝑇","𝑈","𝑉","𝑊","𝑋","𝑌","𝑍","(?<=\\w)\\([^\\)]+\\)"),
                             replacement = c(letters,LETTERS,""),
                             vectorize= F)
   })
   
   saveRDS(gsp_text_with_meta, file = paste0("data_temp/","gsp_formatted",format(Sys.time(), "%Y%m%d-%H:%M")))
   
   gsp_text_with_meta <- readRDS(list.files(path = "data_temp", pattern = "gsp_formatted", full.names = T)[length(
      list.files(path = "data_temp", pattern = "gsp_formatted", full.names = T))])
   
   
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
   
   qtok <- quanteda::tokens(qtok,
                            what = "word",
                            remove_numbers = T,
                            verbose = T)
   
   #removes case-sensitive custom stopwords "NA" and "na" 
   #but keeps "Na" (sodium) before converting toLower
   qtok <- tokens_replace(qtok, pattern = c("NA","na"), replacement = c("",""), 
                          valuetype = "fixed", case_insensitive = F, verbose = T)
   
   pl_names <- generate_place_names()
   
   compounds <- custom_dictionary(c(pl_names[grepl("\\s", pl_names)]))
   
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
   
   tok_n <- quanteda::tokens_compound(qtok[((floor(length(qtok)/500)*500)+1):length(qtok)],pattern = phrase(compounds),
                                      concatenator = '_',valuetype = 'regex',
                                      case_insensitive=T,window = 0)
   paste0("tok n complete featuring rows ", (floor(length(qtok)/500)*500),":",length(qtok))
   qdfm_n <- quanteda::dfm(tok_n, verbose = T)
   qdfm <- rbind(qdfm, qdfm_n)
   
   #dfm_wordstem(qdfm, language = "en") would be used here to stem
   
   saveRDS(qdfm, file = paste0("data_temp/","gsp_tok_",format(Sys.time(), "%Y%m%d-%H:%M")))
   
   qdfm <- readRDS(list.files(path = "data_temp", pattern = "tok", full.names = T)[length(
      list.files(path = "data_temp", pattern = "tok", full.names = T))])

   
   
}