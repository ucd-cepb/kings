source('code/stm_workflow/utils/custom_dictionary.R')
source('code/stm_workflow/utils/generate_proper_names.R')

lex_clean <- function(gsp_text_with_meta, topic_indicators = NULL){
   packs <- c('stm','tm','SnowballC','tidytext','data.table',
              'tidyverse','sf','pbapply','quanteda','stringi')
   need <- packs[!packs %in% installed.packages()[,'Package']]
   if(length(need)>0){install.packages(need)}
   lapply(packs, require, character.only = TRUE)
   
   
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
   
   #Section 1: Math Script Formatting ####
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
   if(!dir.exists('data_temp')){dir.create('data_temp')}
   saveRDS(gsp_text_with_meta, file = paste0("data_temp/","gsp_formatted",format(Sys.time(), "%Y%m%d-%H:%M")))
   
   gsp_text_with_meta <- readRDS(list.files(path = "data_temp", pattern = "gsp_formatted", full.names = T)[length(
      list.files(path = "data_temp", pattern = "gsp_formatted", full.names = T))])
   
   gsp_text_with_meta$text <- as.character(gsp_text_with_meta$text)

   
   #Section 2: Tokenization (Also Removes Punctuation, URLs, and Separators) ####
   qcorp <- quanteda::corpus(x = gsp_text_with_meta[!is_comment&!is_reference,], text_field = "text")
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
   gc()
   #this built-in function only consistently removes positive numbers
   qtok <- quanteda::tokens(qtok,
                            what = "word",
                            remove_numbers = T,
                            verbose = T)
   print("Punctuation and numbers removed")
   
   #Section 3a: Remove Case-Sensitive Custom Stopwords and Protect Short Acronyms Before tolowerization ####
   #removes case-sensitive custom stopwords "NA" and "na" 
   #but keeps "Na" (sodium) before converting toLower
   qtok <- tokens_remove(qtok, pattern = c("NA","na",""),  
                          valuetype = "fixed", case_insensitive = F, verbose = T)
   
   #acronym conversion so that short acronyms don't get dropped
   qtok <- quanteda::tokens_replace(qtok, pattern = c("EJ","Na","SA","pH"),
                                        replacement = c("environmental_justice",
                                                    "sodium",
                                                    "situation_assessment",
                                                    "potential_of_hydrogen"))
   
   
   #Section 3b Underscore and Dash Cleaning####
   #removing multiple underscores 
   qtok <- tokens_replace(qtok, pattern = types(qtok), replacement = stringi::stri_replace_all_regex(types(qtok), "_+", "_"),
                          valuetype = "fixed")
   #removing multiple hyphens/dashes 
   qtok <- tokens_replace(qtok, pattern = types(qtok), replacement = stringi::stri_replace_all_regex(types(qtok), "\\p{Pd}+", "-"),
                          valuetype = "fixed")
   #removing leading and trailing underscores and leading and trailing hyphens/dashes
   qtok <- tokens_replace(qtok, pattern = types(qtok), replacement = stringi::stri_replace_all_regex(types(qtok), "^_|^\\p{Pd}|_$|\\p{Pd}$", ""),
                          valuetype = "fixed")
   
   #Section 4: Compoundization Using Custom Phrase Dictionary of Topic Indicators and Proper Names ####
   #adds compound topic indicator words and proper names to custom dictionary
   
   pr_names <- generate_proper_names(underscore=F, to_lower=T)
   pr_compounds <- pr_names$names[grep("\\s", pr_names$names)]
   
   compounds <- custom_dictionary(
      #removes ^ and $ characters from topic_indicator list
      c(str_remove_all(unlist(topic_indicators[grepl("\\s",topic_indicators)]),"\\^|\\$"),
        pr_compounds))
   compounds <- stri_remove_empty_na(compounds)
   #this takes about 30 min
   #converts toLower, does not stem
   sect_len <- 10
   tok_1 <- quanteda::tokens_compound(qtok[1:sect_len],pattern = phrase(compounds),
                                      concatenator = '_',valuetype = 'regex',
                                      case_insensitive=T,window = 0)
   print(paste0("tok 1 complete featuring rows 1:",sect_len))
   qdfm <- quanteda::dfm(tok_1, verbose = T)
   
   for(i in 2:(length(qtok)/sect_len)){
      tok_i <- quanteda::tokens_compound(qtok[(sect_len*(i-1)+1):(sect_len*i)],pattern = phrase(compounds),
                                         concatenator = '_',valuetype = 'regex',
                                         case_insensitive=T,window = 0)
      if(i %% (500/sect_len) == 0 ){
         print(paste0("tok ", i, " complete featuring rows ", (sect_len*(i-1)+1),":",(sect_len*i)))
      }
      qdfm_i <- quanteda::dfm(tok_i, verbose = T)
      qdfm <- rbind(qdfm, qdfm_i)
   }
   
   tok_n <- quanteda::tokens_compound(qtok[((floor(length(qtok)/sect_len)*sect_len)+1):length(qtok)],pattern = phrase(compounds),
                                      concatenator = '_',valuetype = 'regex',
                                      case_insensitive=T,window = 0)
   print(paste0("tok n complete featuring rows ", (floor(length(qtok)/sect_len)*sect_len)+1,":",length(qtok)))
   qdfm_n <- quanteda::dfm(tok_n, verbose = T)
   qdfm <- rbind(qdfm, qdfm_n)
   
   print("compound tokens generated")
   rm(qtok)
   rm(qdfm_i)
   rm(qdfm_n)
   rm(tok_1)
   rm(tok_i)
   rm(tok_n)
   gc()
   #dfm_wordstem(qdfm, language = "en") would be used here to stem

   saveRDS(qdfm, file = paste0("data_temp/","gsp_tok_",format(Sys.time(), "%Y%m%d-%H:%M")))
   
   qdfm <- readRDS(list.files(path = "data_temp", pattern = "tok", full.names = T)[length(
      list.files(path = "data_temp", pattern = "tok", full.names = T))])
   
   #Section 5: Stopwords Removed, including Months, California/US, and Place Names Replaced with Generic Names ####
   months <- c("^jan$", "^feb$", "^mar$", "^apr$", "^may$", "^jun$", "^jul$", 
               "^aug$", "^sep$", 
               "^sept$", "^oct$", "^nov$", "^dec$", "^january$", "^february$", "^march$",
               "^april$", "^june$", "^july$", "^august$", "^september$", "^october$",
               "^november$", "^december$",
               "jan\\p{Pd}","feb\\p{Pd}","mar\\p{Pd}","apr\\p{Pd}","may\\p{Pd}",
               "jun\\p{Pd}","jul\\p{Pd}","aug\\p{Pd}",
               "sep\\p{Pd}","sept\\p{Pd}","oct\\p{Pd}","nov\\p{Pd}","dec\\p{Pd}")
   
   #removes stopwords, including poor conversion cues, months, 
   #and words that have no letters (eg negative numbers or number ranges)
   custom <- c("us", "u.s","u.s.", "california", "united states")
   
   #TODO consider removing "^plot[0-9]+$"

   #only replace names that have a generic alternative. don't turn names into an empty string generic
   pr_names <- pr_names[!is.na(pr_names$combogeneric) & nchar(pr_names$combogeneric)>0,]
   pr_names$names <- gsub("\\s+", "_", x = pr_names$names)
   #replacing specific place names with generic place names
   qdfm_nostop <- quanteda::dfm_replace(qdfm,pattern = pr_names$names,
                                        replacement = pr_names$combogeneric)
   qdfm_nostop <- quanteda::dfm_remove(qdfm_nostop, pattern = c(stopwords("en"),
                                                         custom))


   #Section 6: SpellCheck and Filtering to Remove Poor Conversion Cues and Keep Only Alpha-Containing Words ####
   #spell check for mispellings that show up commonly in FREX
   qdfm_nostop <- quanteda::dfm_replace(qdfm_nostop,pattern = c("waterhsed","waterhseds"),
                                        replacement = c("watershed","watersheds"))
   #remove poor pdf conversion cues
   qdfm_nostop <- quanteda::dfm_remove(qdfm_nostop, 
                                       pattern = c("ƌ","ă","ƶ","ƚ","ϯ",
                                                   "ϭ","ĩ",
                                                   "ž","ğ","ŝ","ÿ","þ", months), 
                                       valuetype = "regex")
   #Keep only tokens that contain letters
   qdfm_nostop <- quanteda::dfm_keep(qdfm_nostop, pattern = c("[a-z]"), 
                                     valuetype = "regex")

   print("English stopwords and months removed")
   print("Place names replaced with generic equivalents")
   
   saveRDS(qdfm_nostop, file = paste0("data_temp/","nostop",format(Sys.time(), "%Y%m%d-%H:%M")))
   
   rm(qdfm)
   gc()
   #retrieves the latest save of qdfm_nostop
   qdfm_nostop <- readRDS(
      list.files(path = "data_temp", pattern = "nostop", full.names = T)[length(
         list.files(path = "data_temp", pattern = "nostop", full.names = T))])
   
   #Section 7: Delete Short Words ####
   #drops short words less than min_nchar long
   qdfm_long <- dfm_select(qdfm_nostop, min_nchar = 3)
   
   #deletes duplicate rows, if any
   qdfm_long <-dfm_compress(qdfm_long)
   
   saveRDS(qdfm_long, file = paste0("data_temp/","noshort",format(Sys.time(), "%Y%m%d-%H:%M")))
   
   rm(qdfm_nostop)
   print("short words removed")
   
   #retrieves the latest save of qdfm_long
   qdfm_long <- readRDS(
      list.files(path = "data_temp", pattern = "noshort", full.names = T)[length(
         list.files(path = "data_temp", pattern = "noshort", full.names = T))])
   
   #removes 0089 and 0053 before tidying
   qdfm_long <- dfm_subset(qdfm_long, !(gsp_id %in% c("0053","0089")))
   
   library(quanteda.textstats)
   long_stats <- quanteda.textstats::textstat_frequency(qdfm_long, groups = c(gsp_id))
   #Section 8: Remove Very Common and Very Uncommon Words ####

   #find terms found in < 3 gsps
   uncommon_terms <- long_stats %>% group_by(feature) %>% filter(length(unique(group))<3) %>% ungroup()
   uncommon_terms <- uncommon_terms$feature
   
   
   qdfm_med <- dfm_select(qdfm_long, pattern = uncommon_terms, selection = "remove", valuetype = "fixed")
   print("uncommon words removed")
   
   #filter out terms found in at least 30 percent of pages
   qdfm_sm <- dfm_trim(qdfm_med, max_docfreq= 0.3, docfreq_type = "prop")
   
   
   
   #122299 kept pages
   gsp_out <- convert(qdfm_sm, to = "stm")
   
   return(gsp_out)
}
