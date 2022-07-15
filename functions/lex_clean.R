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

lex_clean <- function(gsp_text_with_meta){
   is_comment <- gsp_text_with_meta$is_comment
   is_reference <- gsp_text_with_meta$is_reference
      
   #builds corpus, converts to stm-compatible format, cleans lexicon
   #corpus pulls documents from column 1 of gsp_text_with_meta
   #removes comments and references
   #metadata is all other columns
   #metadata: num rows = num documents. num columns = num metadata type

   #format words in equations to readable text
   #removes parenthetical pieces that are attached to the end of words, eg SurfaceFlow(i)
   #to help with equation word formatting
   
   #the following commands may need to be executed across multiple RStudio sessions
   #to clear up enough memory
   gsp_text_with_meta$text <- pblapply(1:length(gsp_text_with_meta$text), function(i){
      stri_replace_all_regex(gsp_text_with_meta$text[i], pattern = c("𝑎","𝑏","𝑐","𝑑","𝑒","𝑓","𝑔","ℎ","𝑖","𝑗","𝑘","𝑙","𝑚",
                                                                     "𝑛","𝑜","𝑝","𝑞","𝑟","𝑠","𝑡","𝑢","𝑣","𝑤","𝑥","𝑦","𝑧",
                                                                     "𝐴","𝐵","𝐶","𝐷","𝐸","𝐹","𝐺","𝐻","𝐼","𝐽","𝐾","𝐿","𝑀",
                                                                     "𝑁","𝑂","𝑃","𝑄","𝑅","𝑆",
                                                                     "𝑇","𝑈","𝑉","𝑊","𝑋","𝑌","𝑍","(?<=\\w)\\([^\\)]+\\)"),
                             replacement = c(letters,LETTERS,""),
                             vectorize= F)
   })
   print("Math script formatted")
   saveRDS(gsp_text_with_meta, file = paste0("data_temp/","gsp_formatted",format(Sys.time(), "%Y%m%d-%H:%M")))
   
   gsp_text_with_meta <- readRDS(list.files(path = "data_temp", pattern = "gsp_formatted", full.names = T)[length(
      list.files(path = "data_temp", pattern = "gsp_formatted", full.names = T))])
   
   gsp_text_with_meta$text <- as.character(gsp_text_with_meta$text)
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
   rm(qcorp)
   #this built-in function only consistently removes positive numbers
   qtok <- quanteda::tokens(qtok,
                            what = "word",
                            remove_numbers = T,
                            verbose = T)
   print("Punctuation and numbers removed")
   #removes case-sensitive custom stopwords "NA" and "na" 
   #but keeps "Na" (sodium) before converting toLower
   qtok <- tokens_remove(qtok, pattern = c("NA","na",""),  
                          valuetype = "fixed", case_insensitive = F, verbose = T)
   
   pl_names <- generate_place_names()
   
   compounds <- custom_dictionary(pl_names[grepl("\\s", pl_names)])
   
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
   paste0("tok n complete featuring rows ", (floor(length(qtok)/500)*500)+1,":",length(qtok))
   qdfm_n <- quanteda::dfm(tok_n, verbose = T)
   qdfm <- rbind(qdfm, qdfm_n)
   
   print("compound tokens generated")
   rm(qtok)
   rm(qdfm_i)
   rm(qdfm_n)
   rm(tok_1)
   rm(tok_i)
   rm(tok_n)
   #dfm_wordstem(qdfm, language = "en") would be used here to stem
   
   saveRDS(qdfm, file = paste0("data_temp/","gsp_tok_",format(Sys.time(), "%Y%m%d-%H:%M")))
   
   qdfm <- readRDS(list.files(path = "data_temp", pattern = "tok", full.names = T)[length(
      list.files(path = "data_temp", pattern = "tok", full.names = T))])

   months <- c("^jan$", "^feb$", "^mar$", "^apr$", "^may$", "^jun$", "^jul$", 
               "^aug$", "^sep$", 
               "^sept$", "^oct$", "^nov$", "^dec$", "^january$", "^february$", "^march$",
               "^april$", "^june$", "^july$", "^august$", "^september$", "^october$",
               "^november$", "^december$",
               "jan\\p{Pd}","feb\\p{Pd}","mar\\p{Pd}","apr\\p{Pd}","may\\p{Pd}",
               "jun\\p{Pd}","jul\\p{Pd}","aug\\p{Pd}",
               "sep\\p{Pd}","sept\\p{Pd}","oct\\p{Pd}","nov\\p{Pd}","dec\\p{Pd}")
   
   #removes stopwords, including placenames, poor conversion cues, months, 
   #and words that have no letters (eg negative numbers or number ranges)
   pl_names <- generate_place_names(underscore = T)
   qdfm_nostop <- quanteda::dfm_remove(qdfm, pattern = c(stopwords("en"),pl_names))
   qdfm_nostop <- quanteda::dfm_remove(qdfm_nostop, 
                                       pattern = c("ƌ","ă","ƶ","ƚ","ϯ",
                                                   "ϭ","ĩ",
                                                   "ž","ğ","ŝ","ÿ", months), 
                                       valuetype = "regex")
   qdfm_nostop <- quanteda::dfm_keep(qdfm_nostop, pattern = c("[a-z]"), 
                                     valuetype = "regex")

   print("English stopwords, months, and place names removed")
   saveRDS(qdfm_nostop, file = paste0("data_temp/","nostop",format(Sys.time(), "%Y%m%d-%H:%M")))
   
   rm(qdfm)
   #retrieves the latest save of qdfm_nostop
   qdfm_nostop <- readRDS(
      list.files(path = "data_temp", pattern = "nostop", full.names = T)[length(
         list.files(path = "data_temp", pattern = "nostop", full.names = T))])
   
   #drops short words less than min_nchar long
   qdfm_2plus <- dfm_select(qdfm_nostop, min_nchar = 2)
   
   #deletes duplicate rows, if any
   qdfm_2plus <-dfm_compress(qdfm_2plus)
   
   saveRDS(qdfm_2plus, file = paste0("data_temp/","noshort",format(Sys.time(), "%Y%m%d-%H:%M")))
   
   rm(qdfm_nostop)
   print("short words removed")
   
   #retrieves the latest save of qdfm_2plus
   qdfm_2plus <- readRDS(
      list.files(path = "data_temp", pattern = "noshort", full.names = T)[length(
         list.files(path = "data_temp", pattern = "noshort", full.names = T))])
   
   #prepare metadata to add to tidy dtm
   metadata <- cbind(quanteda::docvars(qdfm_2plus),"document"=
                        as.integer(substr(
                           docnames(qdfm_2plus),
                           5,str_length(docnames(qdfm_2plus)
                           ))))
   
   #join metadata with dtm in tidyverse
   dtm_tidy <- tidy(qdfm_2plus) %>% 
      mutate("document" = as.integer(
         substr(document,5,str_length(document)))) %>% 
      inner_join(metadata, by = c("document" = "document"))
   #11578325 observations in dtm_tidy
   
   saveRDS(dtm_tidy, file = paste0("data_temp/","dtm_tidylg",format(Sys.time(), "%Y%m%d-%H:%M")))
   
   # elements
   ntokens <- sum(ntoken(qdfm_2plus))
   nvocab <- sum(ntype(qdfm_2plus))
   rm(qdfm_2plus)
   
   #retrieves the latest save of dtm_tidy
   dtm_tidy <- readRDS(
      list.files(path = "data_temp", pattern = "tidylg", full.names = T)[length(
         list.files(path = "data_temp", pattern = "tidylg", full.names = T))])
   
   #use tidyverse to filter out terms found in < 3 gsps
   dtm_tidy_med <- dtm_tidy %>% group_by(term) %>% filter(length(unique(gsp_id))>2) %>% ungroup()
   #9373829 observations in dtm_tidy_med
   
   saveRDS(dtm_tidy_med, file = paste0("data_temp/","dtm_tidymed",format(Sys.time(), "%Y%m%d-%H:%M")))
   
   #retrieves the latest save of dtm_tidy_med
   dtm_tidy_med <- readRDS(
      list.files(path = "data_temp", pattern = "tidymed", full.names = T)[length(
         list.files(path = "data_temp", pattern = "tidymed", full.names = T))])
   
   print("uncommon words removed")
   #TODO move to quanteda or tm
   #filter out terms found in at least 30 percent of pages
   #this sometimes hangs. should not take over 10 min. if it does, restart R.
   tidy_docs <- length(unique(dtm_tidy$document))
   dtm_tidy_small <- dtm_tidy_med %>% group_by(term) 
   dtm_tidy_small <- dtm_tidy_small %>% 
      filter( (n() / tidy_docs) < 0.3)
   print("very common words removed")
   rm(dtm_tidy)
   rm(dtm_tidy_med)
   # observations in dtm_tidy_small
   dtm_tidy_small <- dtm_tidy_small %>% ungroup()
   
   saveRDS(dtm_tidy_small, file = paste0("data_temp/","dtm_tidysm",format(Sys.time(), "%Y%m%d-%H:%M")))
   
   #retrieves the latest save of dtm_tidy_med
   dtm_tidy_small <- readRDS(
      list.files(path = "data_temp", pattern = "tidysm", full.names = T)[length(
         list.files(path = "data_temp", pattern = "tidysm", full.names = T))])
   
   
   #this sometimes hangs. should not take over 10 min. if it does, restart R.
   gsp_dtm_small <- cast_dtm(dtm_tidy_small,document = document, term = term, value = count)
   #i=doc
   #j=feat
   #v=count
   
   saveRDS(gsp_dtm_small, file = paste0("data_temp/","gsp_dtm_",format(Sys.time(), "%Y%m%d-%H:%M")))
   gsp_dtm_small <- readRDS(list.files(path = "data_temp", pattern = "dtm", full.names = T)[length(
      list.files(path = "data_temp", pattern = "dtm", full.names = T))])
   
   #this sometimes hangs. should not take over 10 min. if it does, restart R.
   meta_small <- unique(dtm_tidy_small[,c(1,4:length(dtm_tidy_small))])
   
   saveRDS(meta_small, file = paste0("data_temp/","gsp_meta_small",format(Sys.time(), "%Y%m%d-%H:%M")))
   meta_small <- readRDS(list.files(path = "data_temp", pattern = "meta_small", full.names = T)[length(
      list.files(path = "data_temp", pattern = "meta_small", full.names = T))])
   
   rm(dtm_tidy_small)
   print(sprintf("Removed %i of %i terms (%i of %i tokens) for appearing in < 3 gsps or > 0.3 of pages", 
                 nvocab-ncol(gsp_dtm_small), nvocab,
                 ntokens-sum(gsp_dtm_small$v), ntokens
   ))
   #removed 11495099 of 11578325 terms (1767807 of 18928618 tokens) for appearing in < 3 gsps or > 0.3 of pages"

   #This records documents dropped in cleaning process
   is_kept <- (1:length(gsp_text_with_meta[[1]][!is_comment&!is_reference]) %in% unique(gsp_dtm_small$dimnames$Docs))
   sum(is_kept)
   #121666 kept pages
   
   rm(gsp_text_with_meta)
   rm(metadata)
   #sometimes this hangs
   gsp_out_slam <- readCorpus(gsp_dtm_small, type = "slam") #using the read.slam() function in stm to convert
   #type = dtm is for dense matrices
   
   gsp_out <- list(documents=gsp_out_slam$documents, vocab=as.character(gsp_out_slam$vocab),
                   meta=meta_small, docs.removed=which(!is_kept))
   
   colnames(gsp_out$meta) <- colnames(meta_small)
   
   saveRDS(gsp_out, file = paste0("data_temp/","gsp_slam_",format(Sys.time(), "%Y%m%d-%H:%M")))
   gsp_out <- readRDS(list.files(path = "data_temp", pattern = "slam", full.names = T)[length(
      list.files(path = "data_temp", pattern = "slam", full.names = T))])
   
   return(gsp_out)
}