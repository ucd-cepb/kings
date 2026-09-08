library(data.table)
library(stringr)
source("Network_Innovation_Paper/Code/_paths.R")
source("Network_Innovation_Paper/Code/_corpus.R")   # load_id_crosswalk()

refs = readRDS(nip_product('03A_reference_extraction', 'gsp_solr_OA_matches.rds'))
#first_year <- 1980
cutoff_score <- 11
refs2 <- refs[{score > cutoff_score | title.gsp == title.oa} & !is.na(source.id.oa),]

# The upstream GSP.File basename is now the canonical gsp_doc_id itself
# (e.g. 'gsp_doc_id_3712.json'), so it already IS the version-unambiguous
# plan-document vertex key -- no crosswalk translation from the legacy
# '(gsp_id, v-number)' filename is needed any more. Parse it straight out and
# keep only ids that resolve against the crosswalk manifest (a guard against a
# stray/unknown doc id). Both documents of a resubmitted plan are kept here; the
# single-document selection happens downstream in 04_modeling/.
refs3 <- refs2[, .(
  work       = basename(openalex.ID),
  gsp_doc_id = str_extract(basename(GSP.File), '(?<=gsp_doc_id_)[0-9]+')
)]
xw <- load_id_crosswalk()
refs3 <- refs3[!is.na(gsp_doc_id) & gsp_doc_id %in% as.character(xw$gsp_doc_id)]

# V1 = OpenAlex work id, V2 = canonical gsp_doc_id (the plan-document vertex key).
saveRDS(refs3[, .(V1 = work, V2 = gsp_doc_id)],
        nip_product('03A_reference_extraction', 'gsp_reference_pairs.rds'))
