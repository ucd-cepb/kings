
library(data.table)
library(tidyverse)
bse <- 'data/Innovation_Paper/'
fls <- list.files(bse,'unfiltered.*RDS')

#### access page metaata ####
page_meta <- fread('Network_Innovation_Paper/data_products/page_metadata.csv')
page_meta <- page_meta[sust_criteria==T,]
page_meta <- page_meta[grepl('^v1',page_meta$file_name),]

trips <- lapply(fls,function(f) {
   print(f)
   file <-  paste0(bse,f)
   temp <- readRDS(file)
   temp <- as.data.table(temp)
   temp$page_num <- as.numeric(str_extract(temp$doc_id,'[0-9]{1,}$'))
   temp$file_name <- basename(str_extract(temp$doc_id,'.*pdf'))
   temp$gsp_id <- str_extract(temp$file_name,'[0-9]{4}')
   temp$gsp_id <- as.numeric(temp$gsp_id)
   temp <- temp[order(page_num,doc_id,sentence_id,token_id),]
   fln <- str_replace(unique(temp$file_name),'pdf$','txt')
   sust_page_index <- page_meta$page_num[page_meta$gsp_id == unique(temp$gsp_id)]
   temp <- temp[page_num %in% sust_page_index,]
      if(nrow(temp)>0){
      advanced_triples <- extract_advanced_triples_from_df(temp) %>% clean_triples()
      if(nrow(advanced_triples)>0){
         advanced_triples$file <- f
         advanced_triples
      }
   }
   }
   )


trips_dt <- rbindlist(trips,use.names = T,fill = T)
write.csv(trips_dt,'Network_Innovation_Paper/data_products/knowledge_triples_sustcrit.csv')




