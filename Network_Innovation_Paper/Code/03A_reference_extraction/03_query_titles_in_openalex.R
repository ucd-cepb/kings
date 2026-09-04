library(stringr)
library(data.table)
library(parallel)
library(referenceExtract)
library(tidyverse)
library(indexBuild)
core_set = detectCores()-1

# Set CLOBBER flag
CLOBBER <- F

refs <- readRDS('Network_Innovation_Paper/data_products/03A_reference_extraction/gsp_classified_refs.rds')
refs$class[grepl('Journal',refs$journal_disambig)] <- 'journal'
refs$class[!is.na(refs$doi)] <- 'journal'

refs$docId <- str_remove(str_remove(basename(refs$File),'__.*'),'\\.json$')

refs <- refs[refs$class=='journal',]
refs <- refs[!is.na(refs$title),]
refs$title_OG <- refs$title
### too many numbers to be a real title
refs <- refs[!str_count(refs$title,"[0-9]") > str_count(refs$title,"[A-Za-z]"),]
#### too many dollar signs to be a real title
refs <- refs[str_count(refs$title,'\\$')<=1,]
refs$title <- str_remove(refs$title,'^\\s+')
refs <- refs[str_count(refs$title,'[^\\s]')>4,]
refs$title <- str_remove_all(refs$title,'\"')
refs$title <- str_remove_all(refs$title,'\\!')
refs$title <- str_remove_all(refs$title,'^[^[A-Za-z0-9]]+')
refs$title <- str_remove_all(refs$title,'\\|')

title_counts <- refs[,.N,by=.(title)][order(-N),]

# Check for existing results unless CLOBBER is TRUE
if(!CLOBBER && file.exists('Network_Innovation_Paper/data_products/03A_reference_extraction/gsp_openalex_title_results.rds')) {
  existing_results <- readRDS('Network_Innovation_Paper/data_products/03A_reference_extraction/gsp_openalex_title_results.rds')
  # Filter out titles that have already been queried
  title_counts <- title_counts[!title %in% existing_results$query_title,]
}

library(openalexR)
library(jsonlite)
library(parallel)
mailto = 'tascott@ucdavis.edu'
title_counts <- title_counts[N>1,]
#### store in running list so restarting/stopping doesn't mean starting over
#### split/apply/combine #####
split_n = 10
title_splits = split(title_counts$title,f = dplyr::ntile(x = title_counts$title,n = split_n))
title_sets = vector(mode='list', length=split_n)


while(any(sapply(title_sets,length)==0)){
  for(t in which(sapply(title_sets,length)==0)){
    print(t)
    title_sets[[t]] <- queryTitles(title_splits[[t]],
                data_style = 'comprehensive',
                mailto = 'tascott@ucdavis.edu',
                try_reduced_string = T,
                max_results = 5)
    Sys.sleep(2)
    gc()
  }
}

#processOA <- function(return_list,keycol = 'id',keep_rows = 4){
#   if(is.null(return_list)){return(NULL)}
#   x = return_list$results
#   if(identical(x,list())){return(NULL)}
#   else{
#     flat <- !sapply(x,class) %in% c('data.frame', 'list') & sapply(x,length) == length(x[[keycol]])
#     flat_dt <- as.data.table(x[flat]) %>% select(-query_title)
#     flat_dt <- data.table(flat_dt,x$host_venue[sapply(x$host_venue,class)!='list'])
#     flat_dt <- data.table(flat_dt,x$biblio)
#     flat_dt$concepts <- unlist(sapply(x$concepts,function(y) if(identical(y,data.frame())){NA}else{y %>% filter(score>=0.5|score==max(score))%>%select(id)%>% .[['id']] %>% basename(.) %>% paste(.,collapse = ';')},simplify = T))
#     flat_dt$authors <- sapply(x$authorships,function(y) if(identical(y,data.frame())){NA}else{paste(y$author$display_name,collapse = '; ')})
#     return(flat_dt[1:min(nrow(flat_dt),keep_rows),])
#   }
# }

title_query_dts <- lapply(title_sets,rbindlist,use.names = T,fill = T)
title_query_dt <- rbindlist(title_query_dts,use.names = T,fill = T)
title_query_dt <- title_query_dt[!duplicated(title_query_dt$id),]


#### for whatever reason, OA doenst's store publisher names w/ works anymore.
#### linke by host ID
### so query those and get display names


library(pbapply)
host_orgs <- unique(title_query_dt$source.host_organization)
host_orgs <- host_orgs[!is.na(host_orgs)]
host_q <- paste0('https://api.openalex.org/',basename(host_orgs),"?select=display_name&mailto=tascott@ucdavis.edu")
host_names = pbsapply(host_q,fromJSON,cl = 1)

host_id_names <- data.table(host.id = basename(host_orgs),host.display_name = unlist(host_names))
title_query_dt$host.display_name = host_id_names$host.display_name[match(basename(title_query_dt$source.host_organization),host_id_names$host.id)]

# Combine with existing results if they exist and CLOBBER is FALSE
if(!CLOBBER && file.exists('Network_Innovation_Paper/data_products/03A_reference_extraction/gsp_openalex_title_results.rds')) {
  title_query_dt <- rbindlist(list(existing_results, title_query_dt), fill=TRUE)
}

saveRDS(title_query_dt,file = 'Network_Innovation_Paper/data_products/03A_reference_extraction/gsp_openalex_title_results.rds')
