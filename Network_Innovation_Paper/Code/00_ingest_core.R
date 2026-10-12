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
#' The ONE departure from "no derivation" is the authoring-agency repair in
#' apply_author_overrides(): where the DWR scrape left `gsa_ids`/`gsa_names`
#' BLANK, they are filled from the paper-owned inputs/doc_author_overrides.csv.
#' It never edits a value DWR published, and it stamps provenance into a
#' `gsa_ids_source` column so a consumer can always tell a repaired row from a
#' scraped one. See the "author overrides" section below.
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
# Author overrides — paper-owned repair of gaps in the DWR scrape
# =============================================================================
#' Fill EMPTY `gsa_ids`/`gsa_names` from inputs/doc_author_overrides.csv.
#'
#' Why this exists: `gsa_ids` is the sole source of the authoring-agency count
#' (`mult_gsa`, a `nodecov()` term in the 04_modeling ERGMs). The DWR portal
#' published no GSA list at all for both documents of plan 0043 — the only 2 of
#' 185 docs with both fields blank — which made that plan enter the model as
#' `mult_gsa = 0`, a value no GSP can really have (a plan exists because a GSA
#' filed it). The fields are blank in `plan_family_manifest.csv` too, so the gap
#' is DWR's and a re-scrape does not close it; a documented local override does.
#'
#' Ingest is otherwise a pure copy, and this stays as near to that as a repair
#' can. Three rules keep it honest:
#'   1. FILL-ONLY. An override may only populate a field the portal left blank.
#'      If DWR has since published a GSA list for that document, this STOPS —
#'      the override has served its purpose and its row should be deleted, not
#'      allowed to silently shadow real upstream data.
#'   2. ROSTER-CHECKED. Every overriding GSA_ID must exist in the canonical
#'      roster (`core_gsa_full()`) and the supplied name must match the roster's
#'      name for that id. This is the guard against the id collision that makes
#'      this particular repair easy to get wrong: plan id 43 and GSA_ID 43 are
#'      unrelated (GSA 43 is Pixley Irrigation District GSA, author of plan
#'      0065), so the correct value for plan 0043 is 205 — Eastern Tule GSA.
#'   3. PROVENANCE STAMPED. Adds `gsa_ids_source` to the crosswalk, one of
#'      "scrape" (DWR published it), "override" (repaired here) or "missing"
#'      (still blank). Downstream code keys on column names, so this is additive.
apply_author_overrides <- function(xw) {
  xw[, gsa_ids_source := ifelse(nzchar(trimws(gsa_ids)), "scrape", "missing")]

  f <- nip_input("doc_author_overrides.csv")
  if (!file.exists(f)) {
    warning("no author overrides at ", f, "; leaving blank gsa_ids as-is")
    return(xw)
  }
  ov <- fread(f, colClasses = list(character = c("gsp_doc_id", "gsa_ids", "gsa_names")))
  if (!nrow(ov)) return(xw)
  if (anyDuplicated(ov$gsp_doc_id)) {
    stop("doc_author_overrides.csv has duplicate gsp_doc_id rows: ",
         paste(unique(ov$gsp_doc_id[duplicated(ov$gsp_doc_id)]), collapse = ", "))
  }

  unknown <- setdiff(ov$gsp_doc_id, xw$gsp_doc_id)
  if (length(unknown)) {
    stop("doc_author_overrides.csv targets gsp_doc_id(s) absent from the manifest: ",
         paste(unknown, collapse = ", "))
  }

  # Rule 1: fill-only.
  occupied <- xw[gsp_doc_id %in% ov$gsp_doc_id & gsa_ids_source == "scrape", gsp_doc_id]
  if (length(occupied)) {
    stop("doc_author_overrides.csv would overwrite a gsa_ids value DWR now publishes ",
         "for doc(s) ", paste(occupied, collapse = ", "),
         ". The upstream gap has closed — delete those override rows.")
  }

  # Rule 2: roster-checked.
  roster_f <- core_gsa_full()
  if (file.exists(roster_f)) {
    roster <- unique(fread(roster_f, select = c("GSA_ID", "GSA_Name")))
    roster[, GSA_ID := as.character(GSA_ID)]
    for (i in seq_len(nrow(ov))) {
      ids <- trimws(strsplit(ov$gsa_ids[i], ",")[[1]])
      ids <- ids[nzchar(ids)]
      if (!length(ids)) {
        stop("doc_author_overrides.csv row for doc ", ov$gsp_doc_id[i], " has no gsa_ids")
      }
      miss <- setdiff(ids, roster$GSA_ID)
      if (length(miss)) {
        stop("doc_author_overrides.csv row for doc ", ov$gsp_doc_id[i],
             " names GSA_ID(s) absent from ", basename(roster_f), ": ",
             paste(miss, collapse = ", "))
      }
      want <- trimws(strsplit(ov$gsa_names[i], ",")[[1]])
      want <- want[nzchar(want)]
      if (length(want)) {
        have <- roster[match(ids, GSA_ID), GSA_Name]
        if (length(want) != length(ids) || !all(trimws(want) == trimws(have))) {
          stop("doc_author_overrides.csv row for doc ", ov$gsp_doc_id[i],
               " disagrees with the roster: gsa_ids (", paste(ids, collapse = ", "),
               ") resolve to [", paste(have, collapse = "; "),
               "] but gsa_names says [", paste(want, collapse = "; "),
               "]. Fix the id or the name — this is the GSA-id/plan-id collision guard.")
        }
      }
    }
  } else {
    warning("roster ", roster_f, " not found; applying author overrides UNVALIDATED")
  }

  ap <- ov[, .(gsp_doc_id, .ids = gsa_ids, .names = gsa_names)]
  xw[ap, on = "gsp_doc_id",
     `:=`(gsa_ids = i..ids, gsa_names = i..names, gsa_ids_source = "override")]
  for (i in seq_len(nrow(ov))) {
    message("  author override: doc ", ov$gsp_doc_id[i], " <- gsa_ids ", ov$gsa_ids[i],
            " (", ov$gsa_names[i], ")")
  }
  still <- xw[gsa_ids_source == "missing", gsp_doc_id]
  if (length(still)) {
    message("  NOTE: ", length(still), " doc(s) still have no authoring GSA: ",
            paste(still, collapse = ", "))
  }
  xw
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
    plan_family_id,
    version        = as.character(version),
    plan_section, submitted_date,
    from_gsp_id    = fromGspId,
    gsa_ids, gsa_names, basin,
    out_basename
  )]
  # doc_rank orders a plan's documents by submission date (1 = earliest). Plan
  # identity is plan_family_id, NOT gsp_id: most resubmissions share their
  # original's gsp_id (a second document under it, rank 2), but some were filed
  # under a fresh gsp_id of their own -- those still carry the original's
  # plan_family_id, so ranking by family keeps EVERY version of a plan in one
  # group (original rank 1, resubmissions rank 2+) however it was keyed.
  # Single-document plans are always 1. This — NOT the manifest `version` field, an
  # unrelated adoption-cycle axis — is how the paper selects one document per plan
  # (see select_plan_docs() / NIP_DOC_SELECT in _corpus.R). gsp_doc_id stays the
  # unique document id throughout.
  # plan_family_id is DWR's, not ours (compute_chain() in core step0). KNOWN
  # DEFECT: for 12 docs submitted 2025-07-14+ it over-collapses -- DWR files each
  # GSA's separately amended plan as a child of one sibling plan, so family 0150
  # and 0050 each hold several DISTINCT plans. Harmless for doc_rank-1 selection
  # (all 12 rank 3+), which is why NIP_DOC_SELECT=latest is disabled in _corpus.R.
  # No integrity assertion is enforced here yet; see the README note.
  # Repair blank authoring-agency fields the DWR scrape left empty (fill-only,
  # roster-checked, provenance stamped into gsa_ids_source).
  xw <- apply_author_overrides(xw)
  xw[, .subdate := as.IDate(submitted_date, format = "%m/%d/%Y")]
  setorder(xw, plan_family_id, .subdate, gsp_doc_id)  # gsp_doc_id breaks any date tie deterministically
  xw[, doc_rank := seq_len(.N), by = plan_family_id]
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
