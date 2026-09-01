#' Network_Innovation_Paper — single source of truth for file paths
#'
#' Every NIP script sources this file instead of hardcoding paths. It defines
#' the *only* two external contracts the paper depends on:
#'   1. `data/core_data/`      — canonical outputs of core_code (the pipeline)
#'   2. `core_code/dicts/`     — canonical NER dictionaries
#' plus paper-local `inputs/` (curated, non-core) and `data_products/`
#' (paper-generated intermediates, including the bridge files 00_ingest_core.R
#' writes). The paper reads NOTHING from sibling paper directories.
#'
#' Usage (from any NIP script):
#'   source(file.path(dirname(sys.frame(1)$ofile %||% "."), "_paths.R"))
#' or, more simply, run scripts from the repo root and:
#'   source("Network_Innovation_Paper/Code/_paths.R")

# ---- Repo-root anchoring -----------------------------------------------------
# Walk up from the working directory until we find the repo sentinel
# (filekey.csv at the repo root). This lets scripts run regardless of getwd().
.find_repo_root <- function(start = getwd()) {
  d <- normalizePath(start, winslash = "/", mustWork = FALSE)
  for (i in 1:40) {
    if (file.exists(file.path(d, "filekey.csv")) &&
        dir.exists(file.path(d, "core_code"))) {
      return(d)
    }
    parent <- dirname(d)
    if (identical(parent, d)) break
    d <- parent
  }
  # Fall back to working directory if sentinel not found.
  normalizePath(start, winslash = "/", mustWork = FALSE)
}

REPO_ROOT <- .find_repo_root()

.rp <- function(...) file.path(REPO_ROOT, ...)

# ---- Top-level contracts -----------------------------------------------------
CORE       <- .rp("data", "core_data")        # canonical pipeline outputs
CORE_DICTS <- .rp("core_code", "dicts")        # canonical NER dictionaries
NIP        <- .rp("Network_Innovation_Paper")  # this paper's root

# ---- Core data helpers (the ONLY inputs from core) ---------------------------
core_pdfs             <- function() file.path(CORE, "source_pdfs")
core_txt_raw          <- function() file.path(CORE, "plan_txts_raw")
core_txt_clean        <- function() file.path(CORE, "plan_txts_clean")
core_parsed           <- function() file.path(CORE, "parsed_plans")
core_disambig         <- function() file.path(CORE, "textnet_objects", "disambig")
core_nondisambig      <- function() file.path(CORE, "textnet_objects", "nondisambig")
core_igraph_multiplex <- function() file.path(CORE, "igraph_objects", "multiplex_directed_graphs")
core_igraph_weighted  <- function() file.path(CORE, "igraph_objects", "uniplex_weighted_graphs")
core_manifest         <- function() file.path(CORE, "source_pdfs", "plan_family_manifest.csv")
core_gsa_full         <- function() file.path(CORE, "metadata", "sgma_gsa_full.csv")
core_page_sections    <- function() file.path(CORE, "metadata", "gsp_page_sections.csv")

# NER dictionaries (used by the NER pipeline; NOT entity-type labels)
core_dict <- function(name) file.path(CORE_DICTS, name)

# ---- Paper-local helpers -----------------------------------------------------
nip_input   <- function(...) file.path(NIP, "inputs", ...)         # curated, non-core
nip_product <- function(...) file.path(NIP, "data_products", ...)  # generated (incl. ingest bridge)
nip_code    <- function(...) file.path(NIP, "Code", ...)

# ---- gsp_doc_id filename helpers ---------------------------------------------
# Core object files use the bare numeric gspDocId stem: "<stem>.RDS".
# Core PDFs use the "gsp_doc_id_<stem>.pdf" prefix. The manifest's `gspDocId`
# column is the join key; `gspId` is the paper's legacy 4-digit gsp_id.
stem_from_filename <- function(x) stringr::str_extract(basename(x), "[0-9]+")
core_pdf_for_stem  <- function(stem) file.path(core_pdfs(), paste0("gsp_doc_id_", stem, ".pdf"))
core_rds_for_stem  <- function(dir, stem) file.path(dir, paste0(stem, ".RDS"))

# null-coalescing helper used above
`%||%` <- function(a, b) if (is.null(a)) b else a
