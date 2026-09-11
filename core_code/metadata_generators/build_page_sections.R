# build_page_sections.R
# Build the canonical core page-level section table (document-structure metadata).
#
# Every GSP submitted to DWR ships with an "Elements Guide" xlsx (the SGMA
# portal's per-document crosswalk from each regulatory element of 23 CCR
# Article 5 / section 354 to the page numbers of the plan where that element is
# addressed). step0 downloads one guide per plan document into
# data/core_data/source_elements/gsp_doc_id_<gspDocId>_elements.xlsx. This
# script parses ALL of them into a per-page section table so any paper can flag
# which section of the plan a given page belongs to.
#
# WHY this replaces the old table: the previous gsp_page_sections.csv was staged
# once from hand-made per-plan page keys that stopped at gsp_id 0156, so the
# newer plans (0157-0174) had no section flags and dropped out of downstream
# networks. The Elements Guides are DWR's own authoritative source for the same
# mapping and cover every document, so regenerating from them both fixes the
# gap and makes the table reproducible. Validation against the legacy hand keys
# (each legacy gsp_id first matched to its physical document by page count) gives
# Jaccard 1.00 on every derivable flag for all 119 documents the old table
# covered.
#
# KEY: gsp_doc_id (the plan *document* id) is the only globally unique GSP
# identifier -- one plan can have several submitted versions, each its own
# document with its own page layout and its own Elements Guide. The table is
# therefore keyed on (gsp_doc_id, page_num); gsp_id (4-digit canonical plan id)
# and version are carried as attributes.
#
# Section flags (one per SubArticle of section 354, from the element id
# ART5_SUBART<n>_...):
#   admin (1) | basin_plan (2) | sust_criteria (3) | monitoring_networks (4)
#   | projects_mgmt_actions (5)
# NA vs FALSE is decided PER SECTION. If the guide gives at least one plan page
# for a section, that section is known: the listed pages are TRUE and every
# other page is FALSE (known-absent). If the guide gives NO plan pages for a
# section -- the element is missing, or its location is written in the
# section/chapter-number column instead of the page column, or the doc uses the
# old spelled-out template -- we do not know which pages are that section, so
# the flag is NA (unknown) on every page, never a misleading FALSE. Only
# resubmitted documents are affected (8825/gsp0015 & 8757/gsp0032 on all five
# sections; 9238/gsp0051 on admin/monitoring/projects; 9921/gsp0031 on
# projects), and each has a fully-keyed original sibling that the first-version
# NIP selection uses instead. is_reference (below) follows the same rule.
# plus:
#   is_reference  -- pages of the "list of references" element, section 354.4(b).
#   is_comment    -- comment/response letters. NOT indexed by the Elements Guide
#                    (post-hoc appendix material). Carried forward from the
#                    legacy hand-keyed table for the documents it covered (each
#                    matched to its physical document by page count); NA for any
#                    document where it is not yet available (e.g. plans 0157+).
#
# Output: data/core_data/metadata/gsp_page_sections.csv, one row per
#   (gsp_doc_id, page_num), full 1..N grid where N is the document's page count
#   (from plan_txts_raw_pages). Columns: gsp_doc_id, gsp_id, version, page_num,
#   admin, basin_plan, sust_criteria, monitoring_networks,
#   projects_mgmt_actions, is_comment, is_reference.
# Companion (provenance, written once): metadata/page_comment_flags_legacy.csv
#   -- the legacy is_comment values re-keyed onto their physical gsp_doc_id, so
#   the non-reproducible flag survives this table being overwritten.
#
# Run:  Rscript core_code/metadata_generators/build_page_sections.R
# Rerun anytime; it overwrites the section table (the is_comment snapshot is
# created only on the first run, from the pre-existing legacy table).

suppressPackageStartupMessages({
  library(data.table)
  library(readxl)
  library(stringr)
})

source("core_code/_config.R")   # provides fk(); resolves paths via filekey.csv

ELEM_DIR   <- fk("core_data_source_elements")
RAWPG_DIR  <- fk("plan_txts_raw_pages_core")
MANIFEST   <- file.path(fk("core_data_source_pdfs"), "plan_family_manifest.csv")
OUT_CSV    <- fk("gsp_page_sections_core")
META_DIR   <- dirname(OUT_CSV)
SNAPSHOT   <- file.path(META_DIR, "page_comment_flags_legacy.csv")

pad4 <- function(x) formatC(as.integer(x), width = 4L, format = "d", flag = "0")

# SubArticle number (from the element id) -> section flag column.
SUBART_FLAG <- c("1" = "admin", "2" = "basin_plan", "3" = "sust_criteria",
                 "4" = "monitoring_networks", "5" = "projects_mgmt_actions")
SECTION_FLAGS <- unname(SUBART_FLAG)

# Expand an Elements-Guide page cell into an integer page vector.
# Cells look like "56", "26:50", or "60, 433:436" (comma-joined single pages
# and a:b ranges). Non-numeric notes are ignored.
parse_pages <- function(x) {
  if (is.na(x) || !nzchar(trimws(x))) return(integer(0))
  out <- integer(0)
  for (s in str_split(x, ",")[[1]]) {
    s <- trimws(s)
    if (!nzchar(s)) next
    if (str_detect(s, ":")) {
      ab <- suppressWarnings(as.integer(str_split(s, ":")[[1]]))
      if (length(ab) == 2L && all(!is.na(ab)) && ab[2] >= ab[1] && ab[1] >= 1L)
        out <- c(out, ab[1]:ab[2])
    } else {
      v <- suppressWarnings(as.integer(s))
      if (!is.na(v) && v >= 1L) out <- c(out, v)
    }
  }
  unique(out)
}

# --- 0. Inputs -------------------------------------------------------------
cat("=== BUILD PAGE SECTIONS (core document-structure metadata) ===\n")
if (!file.exists(MANIFEST)) stop("manifest not found: ", MANIFEST)
man <- fread(MANIFEST, colClasses = "character")
man <- man[!is.na(gspDocId) & nzchar(gspDocId)]
man[, gsp_id := pad4(canonical_gspId)]
setnames(man, "gspDocId", "gsp_doc_id")
man <- unique(man[, .(gsp_doc_id, gsp_id, version)])
cat(sprintf("manifest: %d documents across %d plans\n",
            nrow(man), uniqueN(man$gsp_id)))

doc_npages <- function(docid) {
  f <- file.path(RAWPG_DIR, paste0(docid, ".RDS"))
  if (!file.exists(f)) return(NA_integer_)
  length(readRDS(f))
}

# --- 1. is_comment snapshot (one-time bootstrap from the legacy table) ------
# The legacy gsp_page_sections.csv is keyed on gsp_id (one document per plan).
# is_comment is a per-document property, so re-key it onto the physical
# gsp_doc_id by matching each legacy gsp_id's page grid to the document whose
# page count equals it. Done once, before this script overwrites OUT_CSV.
if (!file.exists(SNAPSHOT) && file.exists(OUT_CSV)) {
  legacy <- fread(OUT_CSV)
  if ("is_comment" %in% names(legacy) && "gsp_id" %in% names(legacy) &&
      !("gsp_doc_id" %in% names(legacy))) {
    cat("Bootstrapping is_comment snapshot from legacy table ...\n")
    legacy[, gsp_id := pad4(gsp_id)]
    leg_grid <- legacy[, .(leg_maxpage = max(page_num)), by = gsp_id]

    npg <- copy(man)
    npg[, n_raw := vapply(gsp_doc_id, doc_npages, integer(1))]
    cand <- merge(leg_grid, npg, by = "gsp_id", allow.cartesian = TRUE)
    cand <- cand[!is.na(n_raw) & n_raw == leg_maxpage]
    # Deterministic tiebreak when >1 version shares the page count: lowest
    # version, then lowest gsp_doc_id.
    setorder(cand, gsp_id, version, gsp_doc_id)
    matched <- cand[, .SD[1], by = gsp_id][, .(gsp_id, gsp_doc_id)]
    unmatched <- setdiff(leg_grid$gsp_id, matched$gsp_id)
    if (length(unmatched))
      cat(sprintf("  WARNING: %d legacy gsp_id(s) unmatched (is_comment dropped): %s\n",
                  length(unmatched), paste(unmatched, collapse = ", ")))

    snap <- merge(legacy[, .(gsp_id, page_num, is_comment)], matched, by = "gsp_id")
    snap <- snap[, .(gsp_doc_id, page_num, is_comment)]
    dir.create(META_DIR, showWarnings = FALSE, recursive = TRUE)
    fwrite(snap, SNAPSHOT)
    cat(sprintf("  wrote %s (%d rows, %d documents)\n",
                SNAPSHOT, nrow(snap), uniqueN(snap$gsp_doc_id)))
  }
}
comment_snap <- if (file.exists(SNAPSHOT)) {
  s <- fread(SNAPSHOT, colClasses = list(character = "gsp_doc_id"))
  setkey(s, gsp_doc_id, page_num); s
} else NULL
comment_docs <- if (!is.null(comment_snap)) unique(comment_snap$gsp_doc_id) else character(0)

# --- 2. Parse one Elements Guide -> per-page flags -------------------------
read_guide <- function(path) {
  sheets <- excel_sheets(path)
  sh <- sheets[str_detect(sheets, regex("elements", ignore_case = TRUE))]
  sh <- if (length(sh)) sh[1] else sheets[length(sheets)]
  d <- as.data.table(read_excel(path, sheet = sh, col_names = FALSE,
                                .name_repair = "minimal"))
  if (ncol(d) < 6L) return(NULL)
  setnames(d, paste0("c", seq_len(ncol(d))))
  d[!is.na(c1) & nzchar(c1)]
}

build_doc <- function(docid, gid, ver) {
  npages <- doc_npages(docid)
  if (is.na(npages) || npages < 1L) {
    cat(sprintf("  SKIP %s: no raw-pages file\n", docid)); return(NULL)
  }
  gpath <- file.path(ELEM_DIR, sprintf("gsp_doc_id_%s_elements.xlsx", docid))
  if (!file.exists(gpath)) {
    cat(sprintf("  SKIP %s: no elements guide\n", docid)); return(NULL)
  }
  el <- tryCatch(read_guide(gpath), error = function(e) NULL)
  if (is.null(el) || !nrow(el)) {
    cat(sprintf("  SKIP %s: unreadable/empty guide\n", docid)); return(NULL)
  }
  el[, subart := str_match(c1, "SUBART([0-9]+)")[, 2]]
  el[, flag := SUBART_FLAG[subart]]

  # Collect the plan pages the guide assigns to each SubArticle flag (col 6),
  # and to the references element (S354_4_b).
  flag_pages <- lapply(SECTION_FLAGS, function(fl) {
    pg <- sort(unique(unlist(lapply(el[flag == fl]$c6, parse_pages))))
    pg[pg <= npages]
  })
  names(flag_pages) <- SECTION_FLAGS
  ref_pg <- sort(unique(unlist(lapply(el[str_detect(c1, "S354_4_b")]$c6, parse_pages))))
  ref_pg <- ref_pg[ref_pg <= npages]

  # NA vs FALSE, decided PER SECTION (not per document). If the guide gives at
  # least one plan page for a section, we KNOW where it is: those pages are TRUE
  # and every other page is FALSE (known-absent). If the guide gives NO plan
  # pages for a section -- the element is missing, or its location is written in
  # the section/chapter-number column (c7) instead of the page column (c6), or
  # the doc uses the old spelled-out template with no machine element ids -- we
  # do NOT know which pages are that section, so the flag is NA (unknown) on
  # every page, never a misleading FALSE. Same principle as is_comment. Only
  # resubmitted documents are affected: 8825 (gsp0015) & 8757 (gsp0032) on all
  # five sections + references; 9238 (gsp0051) on admin/monitoring/projects +
  # references; 9921 (gsp0031) on projects. Each has a fully-keyed original
  # sibling, and the first-version NIP selection uses only those originals.
  dt <- data.table(gsp_doc_id = docid, gsp_id = gid, version = ver,
                   page_num = seq_len(npages))
  na_flags <- character(0)
  for (fl in SECTION_FLAGS) {
    if (length(flag_pages[[fl]]) == 0L) {
      dt[, (fl) := NA]; na_flags <- c(na_flags, fl)
    } else {
      dt[, (fl) := page_num %in% flag_pages[[fl]]]
    }
  }
  if (length(ref_pg) == 0L) {
    dt[, is_reference := NA]; na_flags <- c(na_flags, "is_reference")
  } else {
    dt[, is_reference := page_num %in% ref_pg]
  }
  if (length(na_flags))
    cat(sprintf("  NA-FLAGS %s: guide gives no plan pages for %s -> NA (unknown)\n",
                docid, paste(na_flags, collapse = ", ")))

  # is_comment: from the legacy snapshot for covered documents, else NA.
  if (docid %in% comment_docs) {
    dt <- merge(dt, comment_snap, by = c("gsp_doc_id", "page_num"), all.x = TRUE)
    dt[is.na(is_comment), is_comment := FALSE]
  } else {
    dt[, is_comment := NA]
  }
  dt
}

# --- 3. Build every document ----------------------------------------------
cat(sprintf("Parsing %d Elements Guides ...\n", nrow(man)))
parts <- vector("list", nrow(man))
for (i in seq_len(nrow(man))) {
  parts[[i]] <- build_doc(man$gsp_doc_id[i], man$gsp_id[i], man$version[i])
}
out <- rbindlist(parts, use.names = TRUE, fill = TRUE)
setcolorder(out, c("gsp_doc_id", "gsp_id", "version", "page_num",
                   SECTION_FLAGS, "is_comment", "is_reference"))
setorder(out, gsp_id, version, gsp_doc_id, page_num)

# --- 4. Write + report -----------------------------------------------------
dir.create(META_DIR, showWarnings = FALSE, recursive = TRUE)
fwrite(out, OUT_CSV)
cat(sprintf("\nWrote %s\n  %d rows, %d documents, %d plans\n",
            OUT_CSV, nrow(out), uniqueN(out$gsp_doc_id), uniqueN(out$gsp_id)))
cat("  pages flagged per section (guide-derived flags are NA where the guide\n")
cat("  gives no plan pages for that section in a document):\n")
for (fl in c(SECTION_FLAGS, "is_reference")) {
  na_ds <- out[, .(allNA = all(is.na(get(fl)))), by = gsp_doc_id][allNA == TRUE]$gsp_doc_id
  cat(sprintf("    %-22s %6d TRUE%s\n", fl, sum(out[[fl]], na.rm = TRUE),
              if (length(na_ds))
                sprintf("   (NA in %d doc: %s)", length(na_ds), paste(na_ds, collapse = ", "))
              else ""))
}
cat(sprintf("    %-22s %6d TRUE, %6d NA (pages, not doc-level)\n", "is_comment",
            sum(out$is_comment, na.rm = TRUE), sum(is.na(out$is_comment))))
