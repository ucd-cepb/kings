library(stringr)
library(data.table)
library(indexBuild)
library(pbapply)

source("Network_Innovation_Paper/Code/_paths.R")

mailto = 'tascott@ucdavis.edu'

# Set CLOBBER flag
CLOBBER <- F

### OpenAlex API key: read from the file kept outside the repo and expose it as
### an env var. indexBuild's performOA()/readOA() pick this up and send it as an
### Authorization: Bearer header, so the key never lands in a stored query URL.
### This raises the free daily allowance and enables paid-usage tracking.
oa_key_path <- '../openalex_api'
oa_key <- if(file.exists(oa_key_path)) trimws(readLines(oa_key_path, warn = FALSE)[1]) else ""
if(length(oa_key) == 1 && !is.na(oa_key) && nzchar(oa_key)){
  Sys.setenv(OPENALEX_API_KEY = oa_key)
} else {
  warning('OpenAlex API key missing or blank at ', oa_key_path,
          ' -- proceeding with polite-pool (mailto) auth only.')
}

results_path <- nip_product('03A_reference_extraction', 'gsp_openalex_title_results.rds')

refs <- readRDS(nip_product('03A_reference_extraction', 'gsp_classified_refs.rds'))
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
if(!CLOBBER && file.exists(results_path)) {
  existing_results <- readRDS(results_path)
  # Filter out titles that have already been queried
  title_counts <- title_counts[!title %in% existing_results$query_title,]
}

title_counts <- title_counts[N>1,]

#### split/apply/combine: query new titles in chunks. Finished titles are saved
#### to results_path, so a mid-run stop only loses the current session, and the
#### existing-results filter above lets a re-run pick up where it left off.
if(nrow(title_counts) > 0){
  split_n <- max(1, min(10, nrow(title_counts)))
  title_splits <- split(title_counts$title, f = dplyr::ntile(title_counts$title, split_n))
  title_sets <- lapply(seq_along(title_splits), function(t){
    message('querying title split ', t, ' of ', length(title_splits))
    res <- queryTitles(title_splits[[t]],
                       data_style = 'comprehensive',
                       mailto = mailto,
                       try_reduced_string = T,
                       max_results = 5)
    Sys.sleep(2)
    gc()
    res
  })

  title_query_dts <- lapply(title_sets, rbindlist, use.names = T, fill = T)
  title_query_dt <- rbindlist(title_query_dts, use.names = T, fill = T)
  title_query_dt <- title_query_dt[!duplicated(title_query_dt$id),]

  #### OA no longer stores publisher names with works, so link by host id and
  #### fetch display names. readOA() (indexBuild) throttles to the polite pool
  #### and retries 429/5xx with backoff; per-item tryCatch so one bad/failed host
  #### id yields NA instead of aborting the whole lookup (and losing the run
  #### before saveRDS).
  host_orgs <- unique(title_query_dt$source.host_organization)
  host_orgs <- host_orgs[!is.na(host_orgs)]
  host_q <- paste0('https://api.openalex.org/', basename(host_orgs),
                   "?select=display_name&mailto=", mailto)
  host_names <- pbsapply(host_q, function(u){
    tryCatch({
      dn <- readOA(u)$display_name
      if(is.null(dn)) NA_character_ else dn
    }, error = function(e) NA_character_)
  }, cl = 1)

  host_id_names <- data.table(host.id = basename(host_orgs), host.display_name = unlist(host_names))
  title_query_dt$host.display_name <- host_id_names$host.display_name[match(basename(title_query_dt$source.host_organization), host_id_names$host.id)]

  # Combine with existing results if they exist and CLOBBER is FALSE
  if(!CLOBBER && file.exists(results_path)) {
    title_query_dt <- rbindlist(list(existing_results, title_query_dt), fill=TRUE)
  }

  saveRDS(title_query_dt, file = results_path)
} else {
  message('No new titles to query; ', results_path, ' is up to date.')
}
