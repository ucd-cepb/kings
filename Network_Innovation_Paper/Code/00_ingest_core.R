#' 00_ingest_core.R — the ONLY data bridge from core_data to Network_Innovation_Paper
#'
#' "Ingest" means exactly that: carry a fact core already computed across into the
#' paper, with NO derivation. The one such product is the ID crosswalk, read
#' straight from the plan-family manifest. It reads NO upstream raw docs
#' (source_pdfs, plan_txts_*).
#'
#' Produces, into Network_Innovation_Paper/data_products/00_ingest/:
#'   - id_crosswalk.csv     gspDocId <-> legacy gsp_id/version (from manifest; no derivation)
#'
#' The entity products that used to live here are NOT ingest — they are in-project
#' analysis and now own their own scripts under 01_entity_classification/:
#'   - node_dictionary.csv  (LLM classification)      -> build_node_dictionary.R
#'   - all_gsa_edges.csv    (folds core graphs by GSA type) -> build_gsa_edges.R
#' Both depend on the semantic types core does not carry, so they run AFTER this
#' bridge. See run_all.R for the order.
#'
#' Run from the repo root:  Rscript Network_Innovation_Paper/Code/00_ingest_core.R
#' Set CLOBBER=TRUE to overwrite existing products.

suppressMessages({
  library(data.table)
})

# Resolve this script's directory robustly whether sourced or Rscript-run.
.this_dir <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) NA)
if (is.na(.this_dir)) {
  a <- commandArgs(FALSE); m <- grep("^--file=", a, value = TRUE)
  .this_dir <- if (length(m)) dirname(normalizePath(sub("^--file=", "", m[1]))) else
    "Network_Innovation_Paper/Code"
}
source(file.path(.this_dir, "_paths.R"))

# ---- config ------------------------------------------------------------------
CLOBBER <- toupper(Sys.getenv("CLOBBER", "FALSE")) %in% c("TRUE", "1", "YES")

.skip <- function(path) {
  if (file.exists(path) && !CLOBBER) {
    message("exists, skipping (set CLOBBER=TRUE to rebuild): ", path); TRUE
  } else FALSE
}

# =============================================================================
# ID crosswalk (pure core; columns are pre-joined in the manifest)
# =============================================================================
build_crosswalk <- function() {
  out <- nip_product("00_ingest", "id_crosswalk.csv")
  man <- fread(core_manifest(), colClasses = list(character = c("gspDocId", "gspId")))
  xw <- man[, .(
    gsp_doc_id     = gspDocId,
    gsp_id         = gspId,
    canonical_gsp_id = canonical_gspId,
    version        = as.character(version),
    plan_section, submitted_date,
    from_gsp_id    = fromGspId,
    gsa_ids, gsa_names, basin,
    out_basename
  )]
  # doc_rank orders a plan's documents by submission date (1 = earliest). A plan
  # resubmitted after its original gets rank 2; single-document plans are always
  # 1. This — NOT the manifest `version` field, which is an unrelated
  # adoption-cycle axis — is how the paper selects one document per plan (see
  # select_plan_docs() / NIP_DOC_SELECT in _corpus.R).
  xw[, .subdate := as.IDate(submitted_date, format = "%m/%d/%Y")]
  setorder(xw, gsp_id, .subdate, gsp_doc_id)  # gsp_doc_id breaks any date tie deterministically
  xw[, doc_rank := seq_len(.N), by = gsp_id]
  xw[, .subdate := NULL]
  fwrite(xw, out)
  message("wrote ", out, " (", nrow(xw), " rows)")
  xw
}

# =============================================================================
# main
# =============================================================================
if (sys.nframe() == 0) {
  message("== 00_ingest_core.R (CLOBBER=", CLOBBER, ") ==")
  build_crosswalk()
  message("== ingest complete ==")
}
