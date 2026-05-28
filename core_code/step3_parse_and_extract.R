# step3_parse_and_extract.R
# Parses cleaned text with spaCy transformer (en_core_web_trf) via textNet,
# using an entity ruler built from the global water dictionaries. Writes
# per-GSP nondisambiguated extracts to nondisambiged_extracts_core/.
#
# Inputs:  plan_txts_clean_core/<stem>.parquet     (from step2)
# Outputs: core_data_parsed_plans/parsed_<stem>    (parquet)
#          nondisambiged_extracts_core/<stem>.RDS

# === Flags ===
CLOBBER <- FALSE   # set TRUE to re-parse / re-extract even if outputs exist
TESTING <- FALSE   # set TRUE to restrict to first 5 GSPs

library(textNet)
library(stringr)
library(arrow)

filekey <- read.csv("filekey.csv")

# === Python / spaCy env ===
# Locate a conda env containing "spacy" (replaces the legacy hardcoded
# /Users/elisemiller/... path). Requires spacy + en_core_web_trf installed.
ret_path <- grep("spacy", reticulate::conda_list()$python, value = TRUE)[1]
if (is.na(ret_path)) {
  stop("No conda env containing 'spacy' found. ",
       "Create one with spacy + en_core_web_trf, then rerun.")
}
message("Using spaCy env: ", ret_path)

# === Load cleaned text ===
clean_dir <- filekey[filekey$var_name == "plan_txts_clean_core", ]$filepath
files <- list.files(clean_dir, pattern = "\\.parquet$", full.names = TRUE)

texts <- lapply(files, function(f) {
  tmp <- arrow::read_parquet(f)$text
  tmp <- str_replace_all(tmp, "\\n", " ")
  tmp[!is.na(tmp) & nchar(tmp) > 200]   # drop short / blanked pages
})
names(texts) <- str_remove(basename(files), "\\.parquet$")  # bare gsp_id

# Drop GSPs with no usable pages so the file/text vectors stay aligned
empty <- vapply(texts, length, integer(1)) == 0L
if (any(empty)) {
  message("Dropping ", sum(empty), " GSP(s) with no usable text >200 chars: ",
          paste(names(texts)[empty], collapse = ", "))
  files <- files[!empty]
  texts <- texts[!empty]
}

if (TESTING) {
  n <- min(5L, length(files))
  message("TESTING mode: using ", n, " of ", length(files), " GSP(s)")
  files <- files[seq_len(n)]
  texts <- texts[seq_len(n)]
}

# === Entity ruler from all core dictionaries ===
# Four water dicts + utilities dict use the `all_names` pipe-delimited schema.
# gov_entities_dict has (State, Agency, Abbr) columns instead — handled by
# a schema branch inside load_dict_terms().
dict_csvs <- c(filekey[filekey$var_name == "water_entity_dictionary",         ]$filepath,
               filekey[filekey$var_name == "water_infrastructure_dictionary", ]$filepath,
               filekey[filekey$var_name == "water_bodies_dictionary",         ]$filepath,
               filekey[filekey$var_name == "water_gsa_dictionary",            ]$filepath,
               filekey[filekey$var_name == "ca_utilities_dictionary",         ]$filepath,
               filekey[filekey$var_name == "gov_entities_dict_core",          ]$filepath)

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

# GSP-specific PARTIES — analog to tijuana's Project/Applicant/Permittee list,
# adapted to SGMA/GSP language.
parties <- c("GSA", "GSAs",
             "Agency", "Agencies",
             "Plan", "Plans",
             "Applicant", "Applicants",
             "we", "We")

# === Output paths ===
parsed_stem <- filekey[filekey$var_name == "core_data_parsed_plans", ]$filepath  # "data/core_data/parsed_plans/parsed_"
dir.create(dirname(parsed_stem), recursive = TRUE, showWarnings = FALSE)
parse_fileloc <- paste0(parsed_stem, names(texts))

extract_dir <- filekey[filekey$var_name == "nondisambiged_extracts_core", ]$filepath
dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)

# === Parse ===
parsed <- textNet::parse_text_trf(
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

for (m in seq_along(parsed)) {
  gsp_id       <- names(texts)[m]
  extract_file <- file.path(extract_dir, paste0(gsp_id, ".RDS"))

  if (!CLOBBER && file.exists(extract_file)) {
    message("Extract exists, skipping: ", basename(extract_file))
    next
  }

  textNet::textnet_extract(parsed[[m]],
                           concatenator          = "_",
                           file                  = extract_file,
                           cl                    = 4,
                           keep_entities         = keptentities,
                           return_to_memory      = FALSE,
                           keep_incomplete_edges = TRUE)
}
