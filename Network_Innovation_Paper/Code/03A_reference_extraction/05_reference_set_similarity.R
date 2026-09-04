library(data.table)
library(stringr)
source("Network_Innovation_Paper/Code/_paths.R")
source("Network_Innovation_Paper/Code/_corpus.R")   # load_id_crosswalk()

refs = readRDS(nip_product('03A_reference_extraction', 'gsp_solr_OA_matches.rds'))
#first_year <- 1980
cutoff_score <- 11
refs2 <- refs[{score > cutoff_score | title.gsp == title.oa} & !is.na(source.id.oa),]

# Resolve the legacy GSP.File basename (e.g. 'v1_gsp_num_id_0124.json') to the
# canonical, version-unambiguous gsp_doc_id, so the modeling stage can pick one
# document per plan through select_plan_docs() -- exactly like every other input --
# instead of the old '^v1' filename filter. The 'v' number IS the document rank
# (v1 = original = doc_rank 1, v2 = resubmitted = doc_rank 2), so the
# (gsp_id, v-number) -> (gsp_id, doc_rank) join to the crosswalk is exact. Both
# documents of a resubmitted plan are kept here; the single-document selection
# happens downstream in 04_modeling/.
refs3 <- refs2[, .(
  work     = basename(openalex.ID),
  gsp_id   = as.integer(str_extract(basename(GSP.File), '[0-9]{4}')),
  doc_rank = as.integer(str_extract(basename(GSP.File), '(?<=^v)[0-9]'))
)]
xw <- load_id_crosswalk()
xw[, `:=`(gsp_id = as.integer(gsp_id), doc_rank = as.integer(doc_rank))]
refs3 <- merge(refs3, xw[, .(gsp_id, doc_rank, gsp_doc_id)], by = c('gsp_id', 'doc_rank'))

# V1 = OpenAlex work id, V2 = canonical gsp_doc_id (the plan-document vertex key).
saveRDS(refs3[, .(V1 = work, V2 = gsp_doc_id)],
        nip_product('03A_reference_extraction', 'gsp_reference_pairs.rds'))
