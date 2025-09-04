#### edits here ####
# read in journal file to confirm type = journal, use size count to filter

CLOBBER = F
library(jsonlite)
library(stringr)
library(httr2)
library(data.table)
library(devtools)
#devtools::install_github('govscienceuseR/indexBuild')
library(indexBuild)
mt = readRDS('ria_analysis/input/epa_solr_OA_matches_V2.RDS')
#frame <- fread('ria_analysis/ria_documents/epa_corpus/analytical_sample.csv')
mt$document.id <- str_remove(str_remove(basename(mt$RIA.File),'\\.json$'),'__.*')
#mt_epa_all <- mt[document.id %in% frame$file,]
first_year <- 1980
cutoff_score <- 11
mt_epa <- mt[year.oa>=first_year & {score > cutoff_score | title.ria == title.oa},]

jns <- readRDS('ria_analysis/input/epa_openalex_source_index.RDS')
jns$source.id <- basename(jns$source.id)
jns <- jns[source.type %in% c('journal')]

journal_storage = 'ria_analysis/large_input/epa_journal_records_1980onwards/'
dir.create(journal_storage)
fls <- list.files(journal_storage,full.names = T)
#fls2 <- list.files('ria_analysis/large_input/journal_records_1980/',full.names = T)
fls <- c(fls)
f_id <- str_extract(basename(fls),'^[A-Z0-9]+')


deleted_journal_oa_code <- 'https://openalex.org/S4317411217'

if(!CLOBBER){
still_need <- jns$id[!jns$source.id %in% f_id]
still_need <- still_need[!is.na(still_need)]
}else{still_need <- jns$source.id[!is.na(jns$source.id)]}

still_need <- still_need[!still_need %in% basename(deleted_journal_oa_code)]

for(jid in rev(still_need)){
  sp <- paste0('https://openalex.org/',jid)
  print(paste('accessing',sp))
  tryCatch(extractWorks(source_page = sp,data_style = 'custom',mailto = 'tascott@ucdavis.edu',
                        from_date = '1980',to_date = '2024',return_to_workspace = F,parallel = 5,
                        keep_paratext = F,batch_size = 50e3,
                        per_page = 200,override = 120e3,sleep_time = 1,
                        dest_file = paste0(journal_storage,jid,'.rds')),error = function(e) NULL)
  gc()
}

