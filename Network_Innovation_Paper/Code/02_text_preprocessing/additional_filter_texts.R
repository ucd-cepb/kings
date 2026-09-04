#' Text Preprocessing for Portal Files
#'
#' Builds the standardized `page_metadata.RDS` used by downstream text-reuse
#' analyses, reading the CORE clean-text corpus (one parquet per plan document,
#' keyed by gspDocId — see Code/_corpus.R). The legacy `<<PAGE_BREAK>>`-delimited
#' `.txt` layout is gone: pages come pre-split as parquet rows, and the legacy
#' 4-digit gsp_id / version are recovered from the id crosswalk, not the filename.
#'
#' The output is METADATA-ONLY: page text is used to apply the filter, then dropped
#' before saving (see the save block). The surviving rows enumerate the
#' (gsp_doc_id, page_num) pages that passed; core remains the single source of the
#' text itself, which 03B re-attaches on demand via attach_page_text() (_corpus.R).

# ----- Configuration -----
source("Network_Innovation_Paper/Code/_paths.R")
source("Network_Innovation_Paper/Code/_corpus.R")
CONFIG <- list(
  min_text_length = 400,
  max_text_length = 1e10,
  cut_prop = 0.1,
  space_prop_multiplier = 5,
  output_file = nip_product("page_metadata.RDS")
)

# ----- Setup -----
suppressPackageStartupMessages({
  library(data.table)
  library(stringr)
  library(arrow)
})

# ----- Functions -----
# Core's step2_clean_text_pages.R already density-filters every page (blanks any
# page whose TOTAL punctuation > 0.10, numeric > 0.25, whitespace > 0.75, or that
# is empty / > 20000 chars). So the old per-symbol punctuation checks here
# (periods / quotes / tildes each < 0.10) were fully subsumed by core's punct gate
# and have been removed. What remains is the filtering core does NOT do:
#   - short-page drop (chars > min_text_length): core keeps short prose pages;
#   - all-caps drop (caps/chars < cut_prop): core has no caps filter;
#   - STRICTER numeric (< 0.10 vs core's 0.25) and whitespace (< 0.50 vs 0.75)
#     thresholds, kept deliberately for the page-similarity corpus.
cleanText <- function(text, cut_prop = CONFIG$cut_prop,
                     space_prop_multiplier = CONFIG$space_prop_multiplier) {
  text = gsub('\"\"', '', text, fixed = TRUE)
  chars = nchar(text)
  numbers = stringr::str_count(text, "[0-9]")
  caps = stringr::str_count(text, '[A-Z]')
  spaces = stringr::str_count(text, '\\s')
  valid_indices = chars > CONFIG$min_text_length &
     chars <= CONFIG$max_text_length &
     (numbers/chars) < cut_prop &
     (caps/chars) < cut_prop &
     (spaces/chars) < (cut_prop * space_prop_multiplier)
  text[!valid_indices] <- NA
  return(text)
}

# ----- Main Process -----
main <- function(corpus = NULL) {

  # Core clean-text corpus: one parquet per plan document, keyed by gspDocId,
  # pages already split into rows (columns `page`, `text`). See Code/_corpus.R.
  if (is.null(corpus)) corpus <- read_core_corpus()
  cat(paste("Processing", uniqueN(corpus$gsp_doc_id), "plan documents",
            "(", nrow(corpus), "pages )\n"))

  # Clean page text (cleanText is fully vectorized over the text column).
  corpus[, text := cleanText(text)]

  # Recover legacy 4-digit gsp_id + version from the ingest crosswalk. The core
  # filename stem is gspDocId (variable length), NOT the legacy 4-digit gsp_id.
  documents <- merge(corpus, load_id_crosswalk(), by = "gsp_doc_id", all.x = TRUE)

  # Keep the columns downstream scripts expect. `file_name` is the entity-key
  # base used by hash_and_compare_pages.R (paste0(file_name, '_', page_num)); it
  # is the gspDocId stem so map_similarity can recover ids via the crosswalk.
  documents[, file_name := gsp_doc_id]
  documents[, file_path := file.path(core_txt_clean(), paste0(gsp_doc_id, ".parquet"))]

  # Load page-level section flags from core metadata, keyed on the legacy 4-digit
  # gsp_id + 1-indexed page_num — same indexing as the core `page`. gsp_id is
  # zero-padded ("0007"), so it MUST be read as character to join the crosswalk.
  page_info <- fread(core_page_sections(), colClasses = list(character = "gsp_id"))
  page_info <- page_info[,.(gsp_id, page_num, admin, basin_plan, sust_criteria,
                            monitoring_networks, projects_mgmt_actions, is_comment, is_reference)]

  documents <- merge(documents, page_info, by = c("gsp_id", "page_num"), all.x = TRUE)
  documents <- documents[!is.na(text)]

  # page_metadata is METADATA ONLY: the surviving rows above already define which
  # (gsp_doc_id, page_num) pages passed filtering, so we drop the full page text
  # (~96% of the object) rather than store a second copy of the core corpus. The
  # sole downstream text consumer (03B) re-attaches it on demand via
  # attach_page_text() in _corpus.R, keyed on (gsp_doc_id, page_num) — the same
  # key page_num was derived from — reproducing this text byte-for-byte. 03C never
  # reads text. See Code/_corpus.R for the read-back contract.
  documents[, text := NULL]

  # Save. The slim table is ~0.2 MB, so plain gzip is fine (xz's slower write buys
  # nothing at this size). readRDS auto-detects the codec, so no reader changes.
  dir.create(dirname(CONFIG$output_file), showWarnings = FALSE, recursive = TRUE)
  saveRDS(documents, CONFIG$output_file, compress = TRUE)
  cat(paste("Saved preprocessed data to", CONFIG$output_file, "\n"))

  return(documents)
}

# Run if executed directly
if (sys.nframe() == 0) {
  documents <- main()
  cat("Preprocessing complete.\n")
}
