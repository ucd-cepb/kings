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

# === Enumerate inputs / outputs (no I/O yet) ===
# Build the list of clean-text parquet inputs and the corresponding
# nondisambig RDS output paths, then derive todo_idx (indices that still
# need work) via a single vectorized file.exists() on the outputs. Reading
# parquet text only happens inside the `if (length(todo_idx) > 0L)` block
# below, so a rerun on a mostly-finished pipeline skips parquet reads
# for already-extracted GSPs.
clean_dir   <- fk("plan_txts_clean_core")
files       <- list.files(clean_dir, pattern = "\\.parquet$", full.names = TRUE)
gsp_ids     <- str_remove(basename(files), "\\.parquet$")
names(files) <- gsp_ids

extract_dir <- fk("nondisambiged_extracts_core")
dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)

extract_paths <- file.path(extract_dir, paste0(gsp_ids, ".RDS"))

# Vectorized file.exists — one syscall batch, microseconds total.
if (CLOBBER) {
  todo_idx <- seq_along(gsp_ids)
} else {
  todo_idx <- which(!file.exists(extract_paths))
  n_skipped <- length(gsp_ids) - length(todo_idx)
  if (n_skipped > 0L) {
    message("Skipping ", n_skipped, " GSP(s) with existing extracts ",
            "(set CORE_CLOBBER=1 to reparse).")
  }
}

if (TESTING) {
  n <- min(TESTING_N, length(todo_idx))
  message("TESTING mode: using ", n, " of ", length(todo_idx), " todo GSP(s)")
  todo_idx <- todo_idx[seq_len(n)]
}

if (length(todo_idx) == 0L) {
  message("Nothing to do; all extracts already present.")
  files <- files[integer(0)]
  texts <- list()
} else {
  # Only read parquets for the todo subset.
  todo_files <- files[todo_idx]
  texts <- lapply(todo_files, function(f) {
    tmp <- arrow::read_parquet(f)$text
    tmp <- str_replace_all(tmp, "\\n", " ")
    tmp[!is.na(tmp) & nchar(tmp) > MIN_PAGE_CHARS]   # drop short / blanked pages
  })
  names(texts) <- names(todo_files)

  # Drop GSPs with no usable pages so the file/text vectors stay aligned.
  empty <- vapply(texts, length, integer(1)) == 0L
  if (any(empty)) {
    message("Dropping ", sum(empty), " GSP(s) with no usable text >",
            MIN_PAGE_CHARS, " chars: ",
            paste(names(texts)[empty], collapse = ", "))
    todo_files <- todo_files[!empty]
    texts      <- texts[!empty]
  }
  files <- todo_files
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

# === Parse ===
# parse_text_trf writes one parquet file per GSP to parse_fileloc[m].
# The loop below reads each one back via arrow::read_parquet — the on-disk
# file is the source of truth, so the function's return value is ignored.
# Wrap in invisible() — parse_text_trf returns the full list of parsed
# data frames; without invisible() R auto-prints all of them to the console
# (a screenful per GSP). The on-disk parquets written to parse_fileloc[]
# are the source of truth; the in-memory return value is unused here.
#
# We only feed parse_text_trf the GSPs in the todo subset (texts was already
# trimmed to todo_idx above), so it never even has to look at the already-
# extracted GSPs' parquets. Skip the call entirely when nothing's queued.
if (length(texts) > 0L) {
  invisible(textNet::parse_text_trf(
    ret_path,
    text_list             = texts,
    parsed_filenames      = parse_fileloc,
    overwrite             = CLOBBER,   # textNet param name is "overwrite"
    entity_ruler_patterns = dict_ents,
    overwrite_ents        = TRUE,
    ruler_position        = "after",
    custom_entities       = list(PARTIES = parties)
  ))
}

# === Extract per-GSP networks ===
# Broader than the legacy ORG/GPE/PERSON set: keep dict + pattern hits and all
# spaCy default entity types so step4 can filter downstream.
keptentities <- c("PERSON", "NORP", "FAC",
                  "ORG", "GPE", "LOC", "PRODUCT",
                  "EVENT", "WORK_OF_ART", "LAW", "LANGUAGE",
                  "PARTIES", "CUSTOM", "DICT", "PATTERN")

# `texts` already only contains GSPs that need extraction (todo_idx subset),
# so the per-iteration file.exists skip check is gone. Defensive parse-file
# existence check stays — it can still be missing if parse_text_trf above
# errored on this specific GSP.
for (m in seq_along(texts)) {
  gsp_id       <- names(texts)[m]
  parse_file   <- parse_fileloc[m]
  extract_file <- file.path(extract_dir, paste0(gsp_id, ".RDS"))

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
