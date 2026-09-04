#' Knowledge-triple extraction (Stage 3C, step 1)
#'
#' Assembles subject-predicate-object triples from the CORE dependency parses over
#' the sustainability-criteria pages, and writes them for the similarity notebook
#' (semantic_kg_similarity.ipynb) to embed and score -> triple_similarity.csv.
#'
#' Extractor: textNet::textnet_extract() — the maintained dependency-parse SPO
#' extractor. It consumes the parsed parquet in its native textNet format
#' (doc_id/sentence_id/token_id/token/lemma/pos/tag/entity/head_token_id/dep_rel)
#' and returns an edgelist whose source / head_verb_lemma / target ARE the
#' subject / predicate / object we need. `keep_incomplete_edges = FALSE` drops
#' edges missing a subject or object, so no separate triple-cleaning pass is
#' needed. (The pre-refactor code called a custom extract_advanced_triples_from_df()
#' %>% clean_triples() pair that was never defined in this repo or exported by
#' textNet; textnet_extract() is the canonical replacement.)
#'
#' Paths were migrated off the pre-refactor layout (data/Innovation_Paper/
#' unfiltered_*.RDS + page_metadata.csv + ^v1 filenames). Modern contract:
#'   - dependency parses  : core parsed_plans/parsed_<stem>.parquet (one per gspDocId)
#'   - sust-criteria pages : page_metadata.RDS (sust_criteria flag; built by Stage 2)
#'   - one document per plan: the ORIGINAL doc (doc_rank == 1), matching the old ^v1
#'     filter, so the per-plan `file` key stays 1:1 with gsp_id.
#'
#' Output columns: file, subject, predicate, object. The notebook groups by `file`
#' and zips (subject, predicate, object); the models downstream recover the plan id
#' with str_extract(node, '[0-9]{4}'), so `file` MUST be the 4-digit gsp_id.

library(data.table)
library(arrow)
library(stringr)
library(dplyr)
library(textNet)

source("Network_Innovation_Paper/Code/_paths.R")

# ---- sust-criteria pages of the original document per plan --------------------
page_meta <- as.data.table(readRDS(nip_product("page_metadata.RDS")))
sust_pages <- page_meta[doc_rank == 1 & sust_criteria == TRUE,
                        .(gsp_doc_id, gsp_id, page_num)]
if (!nrow(sust_pages)) stop("no sust-criteria pages in page_metadata.RDS", call. = FALSE)

# ---- extract triples per plan from the core parse ----------------------------
stems <- unique(sust_pages$gsp_doc_id)

trips <- lapply(stems, function(stem) {
  parquet <- file.path(core_parsed(), paste0("parsed_", stem, ".parquet"))
  if (!file.exists(parquet)) {
    message("no parse for gsp_doc_id ", stem, " — skipping")
    return(NULL)
  }
  message("triples: gsp_doc_id ", stem)
  temp <- as.data.table(read_parquet(parquet))

  # Core doc_id is "<stem>_<page_num>" (page is 1-indexed, matches page_metadata).
  temp[, page_num := as.integer(str_extract(doc_id, "[0-9]+$"))]
  keep_pages <- sust_pages[gsp_doc_id == stem, page_num]
  temp <- temp[page_num %in% keep_pages]
  if (!nrow(temp)) return(NULL)

  temp <- temp[order(page_num, doc_id, sentence_id, token_id)]

  extract <- textnet_extract(temp, concatenator = "_", cl = 1,
                             return_to_memory = TRUE,
                             keep_incomplete_edges = FALSE, progress = FALSE)
  edges <- as.data.table(extract$edgelist)
  if (!nrow(edges)) return(NULL)

  # source / predicate-verb / target -> subject / predicate / object.
  # Prefer the verb lemma; fall back to its surface form if the lemma is blank.
  triples <- edges[, .(
    subject   = source,
    predicate = fifelse(is.na(head_verb_lemma) | head_verb_lemma == "",
                        head_verb_name, head_verb_lemma),
    object    = target
  )]
  triples <- triples[!is.na(subject) & subject != "" &
                     !is.na(object)  & object  != ""]
  if (!nrow(triples)) return(NULL)

  # `file` is the plan key downstream (see header): the 4-digit gsp_id.
  triples$file <- sust_pages[gsp_doc_id == stem, gsp_id][1]
  triples
})

trips_dt <- rbindlist(trips, use.names = TRUE, fill = TRUE)
if (!nrow(trips_dt)) stop("no triples extracted from any plan", call. = FALSE)
setcolorder(trips_dt, c("file", "subject", "predicate", "object"))

out <- nip_product("knowledge_triples_sustcrit.csv")
fwrite(trips_dt, out)
cat("wrote", nrow(trips_dt), "triples across", uniqueN(trips_dt$file), "plans ->", out, "\n")
