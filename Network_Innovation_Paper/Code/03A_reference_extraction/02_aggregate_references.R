# 

library(referenceExtract)
#devtools::install_github('govscienceuseR/referenceClassify')
library(referenceClassify)
library(pbapply)
library(data.table)
library(stringr)
library(parallel)
# 
# #### THIS IS WHERE YOU START FROM SCRATCH
#### CAN LOAD THE RDS FILE BELOW TO USE LAST RUN, SKIP reference_clean(), WHICH IS SLOW
refs <- reference_compile('Network_Innovation_Paper/data_products/03A_reference_extraction/extracted_references/',cores = 9)
saveRDS(refs,'Network_Innovation_Paper/data_products/03A_reference_extraction/gsp_raw_references.rds')

refs <- readRDS('Network_Innovation_Paper/data_products/03A_reference_extraction/gsp_raw_references.rds')

### note, sometimes this breaks for what I hink is simply "this cleaning didn't return enougH" for some step
### hav enever troubleshooted it, but if you up the split size it usualy goes away
ptiles <- dplyr::ntile(x = 1:nrow(refs),n = nrow(refs)%/%40e3)
split_refs <- split(refs,ptiles)

clean_splits <- mclapply(split_refs,reference_clean3,mc.cores = 3,mc.cleanup = T,mc.preschedule = T)
clean_dt <- rbindlist(clean_splits)
saveRDS(clean_dt,'Network_Innovation_Paper/data_products/03A_reference_extraction/gsp_clean_unclassified_refs.rds')


clean_dt <- readRDS('Network_Innovation_Paper/data_products/03A_reference_extraction/gsp_clean_unclassified_refs.rds')
clean_dt$journal_disambig <- referenceClassify::journal_disambig(clean_dt$container)
clean_dt$journal_disambig <- str_replace_all(clean_dt$journal_disambig,'The Review of Economics and Statistics','Review of Economics and Statistics')
cls <- referenceClassify::regex_classify(clean_dt,journal_column = 'journal_disambig')
cls$class[grepl('journal|',tolower(cls$journal_disambig))&is.na(cls$class)]  <- 'journal'
cls$class[which(!is.na(cls$url) & is.na(cls$class) & grepl('doi',cls$url))] <- 'journal'
cls$class[!is.na(cls$journal_disambig) & cls$journal_disambig=="Photochemistry and Photobiology" ] <- 'journal'
cls <- cls[!grepl('^memorandum to',tolower(title)),]
#jdf <- sjrdata::sjr_journals
#jdf$title <- str_replace_all(jdf$title,"\\&amp;",'&')
#also_journals <- cls[is.na(cls$class) & str_replace_all(cls$journal_disambig,'\\&','&amp;')%in%jdf$title,][,.N,by=.(journal_disambig)][order(-N),]$journal_disambig
#cls$class[is.na(cls$class) & cls$journal_disambig %in% also_journals] <- 'journal'
saveRDS(cls,'Network_Innovation_Paper/data_products/03A_reference_extraction/gsp_classified_refs.rds')



