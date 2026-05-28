# step3_parse_and_extract.R
# Parses cleaned text with spaCy transformer (en_core_web_trf) via textNet,
# using an entity ruler built from the global water dictionaries. Writes
# per-GSP nondisambiguated extracts to nondisambiged_extracts_core/.
#
# Inputs:  plan_txts_clean_core/<stem>.parquet     (from step2)
# Outputs: core_data_parsed_plans/parsed_<stem>    (parquet)
#          nondisambiged_extracts_core/<stem>.RDS

library(textNet)
library(stringr)
library(arrow)

source("core_code/_config.R")   # provides CLOBBER, TESTING, TESTING_N, MIN_PAGE_CHARS, filekey, fk()

# === Python / spaCy env ===
# `SPACY_ENV` is set in core_code/_config.R (per-user, not in filekey).
# It can be either a conda env name (resolved via reticulate::conda_list())
# or a full path to a python binary. Either way the resolved env must have
# spacy + en_core_web_trf installed.
if (file.exists(SPACY_ENV)) {
  ret_path <- SPACY_ENV
} else {
  envs     <- reticulate::conda_list()
  matches  <- envs$python[envs$name == SPACY_ENV]
  ret_path <- if (length(matches) >= 1L) matches[1] else NA_character_
}
if (is.na(ret_path) || !nzchar(ret_path)) {
  stop("Could not resolve SPACY_ENV = '", SPACY_ENV,
       "'. Edit core_code/_config.R (or set CORE_SPACY_ENV) to a conda env ",
       "name that exists, or to a full path to a python binary with ",
       "spacy + en_core_web_trf installed.")
}
message("Using spaCy env: ", ret_path)

# === Load cleaned text ===
clean_dir <- fk("plan_txts_clean_core")
files <- list.files(clean_dir, pattern = "\\.parquet$", full.names = TRUE)

texts <- lapply(files, function(f) {
  tmp <- arrow::read_parquet(f)$text
  tmp <- str_replace_all(tmp, "\\n", " ")
  tmp[!is.na(tmp) & nchar(tmp) > MIN_PAGE_CHARS]   # drop short / blanked pages
})
names(texts) <- str_remove(basename(files), "\\.parquet$")  # bare gsp_id

# Drop GSPs with no usable pages so the file/text vectors stay aligned
empty <- vapply(texts, length, integer(1)) == 0L
if (any(empty)) {
  message("Dropping ", sum(empty), " GSP(s) with no usable text >",
          MIN_PAGE_CHARS, " chars: ",
          paste(names(texts)[empty], collapse = ", "))
  files <- files[!empty]
  texts <- texts[!empty]
}

if (TESTING) {
  n <- min(TESTING_N, length(files))
  message("TESTING mode: using ", n, " of ", length(files), " GSP(s)")
  files <- files[seq_len(n)]
  texts <- texts[seq_len(n)]
}

# === Entity ruler from all core dictionaries ===
# CORE_DICT_KEYS is the canonical list defined in _config.R; step4 reads
# the same list, so both steps stay in sync. Four water dicts + utilities
# dict use the `all_names` pipe-delimited schema; gov_entities_dict has
# (State, Agency, Abbr) columns instead — handled by a schema branch
# inside load_dict_terms().
dict_csvs <- vapply(CORE_DICT_KEYS, fk, character(1))

load_dict_terms <- function(f) {
  if (!file.exists(f)) { warning("Missing dictionary: ", f); return(character(0)) }
  d <- read.csv(f, stringsAsFactors = FALSE)
  if ("all_names" %in% names(d)) {
    unlist(strsplit(d$all_names, split = "|", fixed = TRUE))
  } else if (all(c("Agency", "Abbr") %in% names(d))) {
    # gov_entities schema: every Agency + every Abbr is a candidate entity term
    unique(c(d$Agency, d$Abbr))
  } else {
    warning("Unknown dictionary schema (", f, "): expected 'all_names' or Agency+Abbr")
    character(0)
  }
}

global_terms <- unlist(lapply(dict_csvs, load_dict_terms))

all_terms <- unique(global_terms)
all_terms <- all_terms[!is.na(all_terms) & nzchar(all_terms)]
# Keep only multi-word terms; single-token entries (e.g. "Plan", "Agency",
# "MWD") collide too often with prose and the entity ruler's whole-word
# matching catches them as false positives. Single-token aliases still feed
# step4's disambiguation map via the same CSVs.
all_terms <- all_terms[stringr::str_count(all_terms, "\\s+") >= 1L]
message("Entity ruler: ", length(all_terms), " unique multi-word term(s)")

dict_ents <- textNet::entity_specify(all_terms,
                                     case_sensitive  = TRUE,
                                     whole_word_only = TRUE,
                                     entity_label    = "DICT")
dict_ents <- c(dict_ents, textNet::build_structural_org_patterns())

# GSP-specific PARTIES 
parties <- c("GSA", "GSAs",
             "Agency", "Agencies",
             "Plan", "Plans",
             "Applicant", "Applicants",
             "we", "We")

# === Output paths ===
# parse_text_trf writes .parquet (via duckdb) and auto-appends the extension
# to whatever paths we pass. Build the full path here so the in-loop
# arrow::read_parquet() call below sees the same string the function wrote to.
parsed_stem <- fk("core_data_parsed_plans")  # "data/core_data/parsed_plans/parsed_"
dir.create(dirname(parsed_stem), recursive = TRUE, showWarnings = FALSE)
parse_fileloc <- paste0(parsed_stem, names(texts), ".parquet")

extract_dir <- fk("nondisambiged_extracts_core")
dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)

# === Parse ===
# parse_text_trf writes one parquet file per GSP to parse_fileloc[m].
# The loop below reads each one back via arrow::read_parquet — the on-disk
# file is the source of truth, so the function's return value is ignored.
textNet::parse_text_trf(
  ret_path,
  text_list             = texts,
  parsed_filenames      = parse_fileloc,
  overwrite             = CLOBBER,   # textNet param name is "overwrite"
  entity_ruler_patterns = dict_ents,
  overwrite_ents        = TRUE,
  ruler_position        = "after",
  custom_entities       = list(PARTIES = parties)
)

# === Extract per-GSP networks ===
# Broader than the legacy ORG/GPE/PERSON set: keep dict + pattern hits and all
# spaCy default entity types so step4 can filter downstream.
keptentities <- c("PERSON", "NORP", "FAC",
                  "ORG", "GPE", "LOC", "PRODUCT",
                  "EVENT", "WORK_OF_ART", "LAW", "LANGUAGE",
                  "PARTIES", "CUSTOM", "DICT", "PATTERN")

for (m in seq_along(texts)) {
  gsp_id       <- names(texts)[m]
  parse_file   <- parse_fileloc[m]
  extract_file <- file.path(extract_dir, paste0(gsp_id, ".RDS"))

  if (!CLOBBER && file.exists(extract_file)) {
    message(sprintf("[%d/%d] %s — extract exists, skipping",
                    m, length(texts), gsp_id))
    next
  }

  if (!file.exists(parse_file)) {
    message(sprintf("[%d/%d] %s — parse file missing (%s); skipping",
                    m, length(texts), gsp_id, parse_file))
    next
  }

  message(sprintf("[%d/%d] %s — extracting", m, length(texts), gsp_id))
  tryCatch({
    parsed_m <- arrow::read_parquet(parse_file)
    # textnet_extract writes directly to `file=`; route through atomic_write
    # so a Ctrl-C / OOM in the middle doesn't leave a corrupt RDS in place.
    atomic_write(extract_file, function(p) {
      textNet::textnet_extract(parsed_m,
                               concatenator          = "_",
                               file                  = p,
                               cl                    = PARSE_WORKERS,
                               keep_entities         = keptentities,
                               return_to_memory      = FALSE,
                               keep_incomplete_edges = TRUE)
    })
  },
  error = function(e) {
    message(sprintf("  ERROR (%s): %s -- continuing",
                    gsp_id, conditionMessage(e)))
  })
}
