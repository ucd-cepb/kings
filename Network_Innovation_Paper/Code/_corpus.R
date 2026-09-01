#' Network_Innovation_Paper — core clean-text corpus + id-crosswalk helpers
#'
#' Shared by the text-reuse feeders (01_text_preprocessing/, 02B_text_reuse/). Core
#' stores the clean plan-text corpus as ONE parquet per plan document under
#' core_txt_clean(), named by the numeric gspDocId stem (e.g. `1147.parquet`),
#' with two columns: `page` (1-indexed page number) and `text` (that page's
#' clean text). The legacy layout — one flat `v*_gsp_num_id_*.txt` per doc with
#' pages joined by a `<<PAGE_BREAK>>` sentinel and the 4-digit gsp_id embedded in
#' the filename — is gone. The gspDocId stem is the join key; recover the legacy
#' 4-digit gsp_id and the version from `data_products/id_crosswalk.csv`.

# Resolve paths (scripts are run from the repo root; _paths.R defines helpers).
if (!exists("nip_product") || !exists("core_txt_clean")) {
  source("Network_Innovation_Paper/Code/_paths.R")
}

if (!requireNamespace("arrow", quietly = TRUE)) {
  stop("The 'arrow' package is required to read the core parquet corpus.")
}
suppressPackageStartupMessages({
  library(data.table)
})

#' gspDocId -> legacy gsp_id (4-digit) + version + doc-selection fields, from the
#' ingest bridge file. `doc_rank`/`plan_section`/`submitted_date` are what pick
#' one document per plan (see select_plan_docs); `version` is the manifest's
#' adoption-cycle field and is NOT the original/resubmitted axis.
load_id_crosswalk <- function() {
  cw <- data.table::fread(
    nip_product("id_crosswalk.csv"),
    colClasses = list(character = c("gsp_doc_id", "gsp_id"))
  )
  keep <- intersect(
    c("gsp_doc_id", "gsp_id", "version", "plan_section", "submitted_date", "doc_rank"),
    names(cw)
  )
  unique(cw[, ..keep])
}

#' Pick exactly ONE plan document per gsp_id.
#'
#' 79 plans were submitted once; 53 were resubmitted, so the corpus holds two
#' documents for them (an `original` and a `resubmitted`). Every downstream
#' plan-to-plan analysis needs a single document per plan. `doc_rank` (written by
#' 00_ingest_core.R) orders a plan's documents by submission date, 1 = earliest.
#'
#' mode:
#'   "original" (default) -> earliest submission  (reproduces the legacy '^v1' selection)
#'   "latest"             -> most recent submission (the resubmitted doc where one exists)
#'
#' Single-document plans keep their sole document under either mode. Flip the
#' whole pipeline at once by setting the NIP_DOC_SELECT env var. Returns one
#' crosswalk row per gsp_id (columns gsp_id, gsp_doc_id, doc_rank).
select_plan_docs <- function(xw = load_id_crosswalk(),
                             mode = Sys.getenv("NIP_DOC_SELECT", "original")) {
  mode <- match.arg(mode, c("original", "latest"))
  if (!"doc_rank" %in% names(xw)) {
    stop("id_crosswalk is missing 'doc_rank'; rebuild it with 00_ingest_core.R ",
         "(build_crosswalk / CLOBBER=TRUE).")
  }
  xw <- unique(xw[, .(gsp_doc_id, gsp_id, doc_rank)])
  chooser <- if (mode == "original") which.min else which.max
  xw[, .SD[chooser(doc_rank)], by = gsp_id]
}

#' Read the whole core clean-text corpus as a long data.table.
#' Returns columns: gsp_doc_id (character stem), page_num (int), text (chr).
read_core_corpus <- function(dir = core_txt_clean()) {
  files <- list.files(dir, pattern = "\\.parquet$", full.names = TRUE)
  if (!length(files)) stop("No parquet corpus files found in ", dir)
  data.table::rbindlist(lapply(files, function(f) {
    pg <- arrow::read_parquet(f)  # cols: page, text
    data.table::data.table(
      gsp_doc_id = stem_from_filename(f),
      page_num   = as.integer(pg$page),
      text       = pg$text
    )
  }))
}

#' Newest page-score file written by hash_and_compare_pages.R. Names are
#' portal_page_scores_YYYYMMDD.rds, so lexical sort == chronological.
latest_page_scores <- function() {
  fs <- list.files(nip_product("score_results"),
                   pattern = "^portal_page_scores_.*\\.rds$", full.names = TRUE)
  if (!length(fs)) stop("No portal_page_scores_*.rds in ", nip_product("score_results"))
  utils::tail(sort(fs), 1)
}
