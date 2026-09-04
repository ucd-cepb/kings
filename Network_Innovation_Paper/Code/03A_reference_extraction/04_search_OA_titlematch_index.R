library(stringr)
library(data.table)
library(devtools)
library(referenceSearch)
library(pbapply)
library(tidyverse)
refs <- readRDS('Network_Innovation_Paper/data_products/03A_reference_extraction/gsp_classified_refs.rds')

NEW_INDEX = T
index_name = "GSP_OA_title_matches"

refs$docId <- str_remove(str_remove(basename(refs$File),'__.*'),'\\.json$')
#refs$docketId <- docs$docketId[match(refs$docId ,docs$id)]
refs <- refs[refs$class=='journal',]
refs <- refs[!is.na(refs$title),]



oa_candidates <- readRDS('Network_Innovation_Paper/data_products/03A_reference_extraction/gsp_openalex_title_results.rds')
setnames(oa_candidates,c('publication_year','source.display_name','author_display_name','host.display_name'),c('year','journal_title','authors','publisher'))
oa_candidates <- oa_candidates[type %in% c('article','peer-review','review'),]
oa_candidates$miscid <- oa_candidates$id
oa_candidates$source <- 'openAlex'
oa_index <- oa_candidates[,.(title,authors,year,publisher,journal_title,doi,source,miscid)]

setnames(refs,c('author','journal_disambig'),c('authors','journal_title'))
refs_to_search <- refs[,.(title,authors,year,publisher,journal_title,doi)]

#### note, it looks like boosting doesn't work?
### fix some weird extracts
### should check in reference_clean3() why these don't get fixed
##### janky workaround for broken boost is just to include multiple times (boosting occurs because matches to the duplicates count more)
na_pattern <- "^(Na\\s*)+Na?$"
refs_to_search$journal_title[grepl(na_pattern,refs_to_search$journal_title)] <- NA
refs_to_search$journal_title[refs_to_search$journal_title %in% c('Environmental Science Technol','Environmental Science and Technolology',
    'Environmental Scientific Technology','Environmental Science And Technology',
    'Environmental Science and Technol','Environmental Science And Technol',
    'Envion Science Technol')] <- "Environmental Science & Technology"
refs_to_search$journal_title[grepl("Env.*Scie.*Letters",refs_to_search$journal_title)] <- "Environmental Science & Technology Letters"
refs_to_search$journal_title[grepl("Sci.*Total.*Environment",refs_to_search$journal_title)] <- "Science of The Total Environment"


refs_to_search$year <- str_extract(refs_to_search$year,'^[0-9]{4}')

system('solr start -c')
conn = solrium::SolrClient$new()

if(NEW_INDEX){index_records(oa_index, collection_name=index_name,overwrite = T)}
#unique queries is about 20% the length of all queries, so condensing and then matching back saves time
phrase_queries = create_queries(refs_to_search,treat_as_phrase = c('title','journal_title'),boost_fields = c('title','journal_title','authors'),boost_values = c(5,5,0.2))
jumble_queries = create_queries(refs_to_search,boost_fields = c('title','journal_title','authors'),
                                boost_value = c(1,1,1),
                                treat_as_phrase = NULL)
                         #fuzzy_fields = c('journal_title'),
                        # boost_values = c(2,2),
                        # boost_fields = c('title','journal_title'))
refs_to_search$GSP.File <- refs$File
refs_to_search$anystyle.ID <- refs$ID
refs_to_search$query <- phrase_queries
refs_to_search$alt_query <- jumble_queries

uq_queries <- data.table(q = refs_to_search$query,q2 = refs_to_search$alt_query)
uq_queries <- uq_queries[!duplicated(uq_queries),]

#### alt strategies -- could retain more than topn = 1 and do post-hoc title fuzzy match?
#### that is sort of janky...
#### core problem is title string exact is to strict and bag of words too ineffective.
#### is there a longest-common-string style option?

## search collection
library(pbapply)
q_results <- pblapply(seq_along(uq_queries$q),function(query) {
  temp_res = search_collection(conn = conn,uq_queries$q[query], topn = 1, collection_name="GSP_OA_title_matches") |> mutate(alt = F)
  temp_res2 = search_collection(conn = conn,uq_queries$q2[query], topn = 1,collection_name="GSP_OA_title_matches") |> mutate(alt = T)
  if(nrow(temp_res)>1 & nrow(temp_res2)>1){
    temp_res <- ifelse(temp_res$score>temp_res2$score,temp_res,temp_res2)
  }
  if(nrow(temp_res)<1 & nrow(temp_res2)>1){
    temp_res <- temp_res2
  }
  return(temp_res)
  }
)

q_results <- lapply(seq_along(q_results),function(x) if(nrow(q_results[[x]])==0){data.table(query = uq_queries$q[x])}else{q_results[[x]]|>mutate(query = uq_queries$q[x])})
q_dt <- rbindlist(q_results,fill = T,use.names = T)

repl <- c("title","authors","year", "publisher","journal_title", "doi")
nw <- paste0(repl,'.gsp')
setnames(refs_to_search,repl,nw)
setnames(q_dt,repl,paste0(repl,'.oa'))
q_dt$source.id.oa <- oa_candidates$source.id[match(q_dt$miscid,oa_candidates$id)]

results <- merge(refs_to_search,q_dt)#[match(refs_to_search$query,q_dt$query),])
setnames(results,'miscid','openalex.ID')

saveRDS(results,'Network_Innovation_Paper/data_products/03A_reference_extraction/gsp_solr_OA_matches.rds')
system('solr stop')
#### roughly, scores > 11 seem to be pretty good

#rw[grepl('dam removal on the elwha river',tolower(rw$title)),]
#openalexR::oa_query(entity = 'works',search = 'large-scale dam removal on the elwha river channel and floodplain geomorphic change')
# 
# for (q in queries) {
#   print(count)
#   res = search_collection(q, collection_name="OA_title_matches")
#   res$id = count
#   res$q = q
#   results[[count]] = res
#   count = count + 1
# }
# results_df = do.call(dplyr::bind_rows, results)


