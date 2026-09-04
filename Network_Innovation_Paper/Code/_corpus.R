#' Network_Innovation_Paper — core clean-text corpus + id-crosswalk helpers
#'
#' Shared by the text-reuse feeders (02_text_preprocessing/, 03B_text_reuse/). Core
#' stores the clean plan-text corpus as ONE parquet per plan document under
#' core_txt_clean(), named by the numeric gspDocId stem (e.g. `1147.parquet`),
#' with two columns: `page` (1-indexed page number) and `text` (that page's
#' clean text). The legacy layout — one flat `v*_gsp_num_id_*.txt` per doc with
#' pages joined by a `<<PAGE_BREAK>>` sentinel and the 4-digit gsp_id embedded in
#' the filename — is gone. The gspDocId stem is the join key; recover the legacy
#' 4-digit gsp_id and the version from `data_products/00_ingest/id_crosswalk.csv`.

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
    nip_product("00_ingest", "id_crosswalk.csv"),
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

#' Per-plan document selection for the modeling stage, with ids formatted the way
#' the 04_modeling/ scripts key on them. Wraps select_plan_docs() (so it honors
#' NIP_DOC_SELECT) and returns one row per plan:
#'   gsp_doc_id  canonical, version-unambiguous plan-document id (character) --
#'               THE vertex key for every network in the modeling stage.
#'   gsp_id      4-digit zero-padded legacy plan id (character) -- kept ONLY to
#'               attach the plan-level covariates/shapefile (which have no document
#'               version) onto the chosen document.
#'   doc_rank    the selected document's rank.
#' gsp_id is not unique across plan versions; gsp_doc_id is. Downstream code keys on
#' gsp_doc_id and uses this table to translate the legacy gsp_id-keyed inputs onto it.
modeling_plan_selection <- function() {
  s <- select_plan_docs()
  s[, .(gsp_doc_id = as.character(gsp_doc_id),
        gsp_id     = formatC(as.integer(gsp_id), width = 4L, flag = "0"),
        doc_rank   = doc_rank)]
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

#' Re-attach clean page text to a page_metadata table on demand.
#'
#' page_metadata.RDS is metadata-only (Stage 2 drops the text column); this reads
#' the text back from the CORE clean-text corpus for exactly the rows in `meta`.
#' It reads only the parquet stems present in `meta`, keys the join strictly on
#' the identifiers (gsp_doc_id, page_num) — never on row position — and applies the
#' same gsub('""','') Stage 2's cleanText applied, so the result is byte-identical
#' to the text page_metadata used to bundle. `meta` must carry gsp_doc_id and
#' page_num; the join is 1:1 (the corpus has one row per (stem, page)). Row order
#' of the returned table follows `meta`; callers that concatenate pages (e.g. 03B)
#' MUST setorder on page_num first so the sequence derives from the identifier, not
#' incidental order. Returns `meta` plus a `text` column.
attach_page_text <- function(meta, dir = core_txt_clean()) {
  meta <- data.table::as.data.table(meta)
  if (!all(c("gsp_doc_id", "page_num") %in% names(meta))) {
    stop("attach_page_text: `meta` needs columns gsp_doc_id and page_num.")
  }
  if ("text" %in% names(meta)) meta[, text := NULL]
  stems <- unique(meta$gsp_doc_id)
  txt <- data.table::rbindlist(lapply(stems, function(s) {
    f <- file.path(dir, paste0(s, ".parquet"))
    if (!file.exists(f)) stop("attach_page_text: no core corpus file for ", s, " (", f, ")")
    pg <- arrow::read_parquet(f)  # cols: page, text
    data.table::data.table(
      gsp_doc_id = s,
      page_num   = as.integer(pg$page),
      text       = gsub('""', '', pg$text, fixed = TRUE)
    )
  }))
  # Left-join text onto meta by identifier, preserving meta's row order.
  out <- txt[meta, on = .(gsp_doc_id, page_num)]
  missing <- out[is.na(text), .N]
  if (missing) warning("attach_page_text: ", missing,
                       " page(s) had no matching text in the core corpus.")
  out
}

#' Newest page-score file written by hash_and_compare_pages.R. Names are
#' portal_page_scores_YYYYMMDD.rds, so lexical sort == chronological.
latest_page_scores <- function() {
  fs <- list.files(nip_product("03B_text_reuse", "score_results"),
                   pattern = "^portal_page_scores_.*\\.rds$", full.names = TRUE)
  if (!length(fs)) stop("No portal_page_scores_*.rds in ", nip_product("03B_text_reuse", "score_results"))
  utils::tail(sort(fs), 1)
}

#' The project-section Jaccard file written by compare_project_sections.R.
#' A single file, project_section_jaccard_scores.rds, overwritten each run.
#' This is the 03B modeling input consumed by 04_modeling/*. For backward
#' compatibility we fall back to any leftover dated file if the fixed one is
#' absent (older runs wrote project_section_jaccard_scores_YYYYMMDD.rds).
latest_project_jaccard <- function() {
  dir <- nip_product("03B_text_reuse", "project_jaccard_results")
  fixed <- file.path(dir, "project_section_jaccard_scores.rds")
  if (file.exists(fixed)) return(fixed)
  fs <- list.files(dir, pattern = "^project_section_jaccard_scores_.*\\.rds$",
                   full.names = TRUE)
  if (!length(fs)) stop("No project_section_jaccard_scores.rds in ", dir)
  utils::tail(sort(fs), 1)
}
