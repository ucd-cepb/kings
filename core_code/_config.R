# _config.R — shared configuration for the core pipeline.
# Sourced at the top of every step{1..5}_*.R and step_audit_pipeline.R.
# Centralizes flags and tunables that more than one step depends on, so
# the steps stay consistent and a single change rolls through the pipeline.
#
# Usage in a step script:
#   source("core_code/_config.R")
#
# Override defaults from the shell with environment variables (set by
# run_pipeline.sh --clobber / --testing, or by the user before invoking
# Rscript directly):
#   CORE_CLOBBER=1          -> CLOBBER         <- TRUE         (default FALSE)
#   CORE_TESTING=1          -> TESTING         <- TRUE         (default FALSE)
#   CORE_TESTING_N=10       -> TESTING_N       <- 10           (default 5)
#   CORE_MIN_PAGE_CHARS=300 -> MIN_PAGE_CHARS  <- 300          (default 200)
#   CORE_PARSE_WORKERS=8    -> PARSE_WORKERS   <- 8            (default 4)
#   CORE_SPACY_ENV=myenv    -> SPACY_ENV       <- "myenv"      (default "spacy-env")

# === Helper: bool / int env var with default ================================
.env_bool <- function(name, default = FALSE) {
  v <- Sys.getenv(name, unset = NA)
  if (is.na(v) || !nzchar(v)) return(default)
  tolower(v) %in% c("1", "true", "t", "yes", "y")
}
.env_int <- function(name, default) {
  v <- Sys.getenv(name, unset = NA)
  if (is.na(v) || !nzchar(v)) return(default)
  suppressWarnings(as.integer(v))
}

# === Pipeline flags ==========================================================
# CLOBBER: re-run a step's work even if the output file already exists.
# Default FALSE so reruns are cheap; flip to TRUE via `run_pipeline.sh --clobber`
# or `CORE_CLOBBER=1 Rscript core_code/stepN_*.R`.
CLOBBER   <- .env_bool("CORE_CLOBBER", FALSE)

# TESTING: restrict to the first TESTING_N input files. Used by step2, step3
# (and any other step that wants a fast smoke-test loop). Off by default.
TESTING   <- .env_bool("CORE_TESTING", FALSE)
TESTING_N <- .env_int("CORE_TESTING_N", 5L)

# === Shared thresholds ======================================================
# MIN_PAGE_CHARS: a "usable" page must have more than this many characters
# after step2's cleaning. Used by step3 (filters pages before parsing) and
# step_audit_pipeline (reports usable-page counts). Must stay in sync across
# both — that's why it lives here, not in the individual scripts.
MIN_PAGE_CHARS <- .env_int("CORE_MIN_PAGE_CHARS", 200L)

# PARSE_WORKERS: number of parallel workers passed as `cl` to
# textNet::textnet_extract() in step3. 4 is a sensible default on a typical
# laptop; raise on a workstation, drop to 1 if you're debugging.
PARSE_WORKERS <- .env_int("CORE_PARSE_WORKERS", 4L)

# === Dictionary keys ========================================================
# Canonical list of all curated dictionaries the pipeline knows about.
# Used by step3 (entity ruler patterns) AND step4 (alias → canonical map).
# Edit here, both steps pick it up.
#
# Schema note: the four water dicts and ca_utilities share the `all_names`
# pipe-delimited schema. gov_entities_dict has (State, Agency, Abbr)
# columns. step3's load_dict_terms() handles both. step4 loads gov_entities
# separately as `govscitbl` in Section 1; its global_dict path silently
# no-ops on the Agency/Abbr schema (no `all_names` column to iterate), so
# listing gov_entities here is harmless — it just doesn't contribute to
# step4's global_dict (it still feeds step4 via govscitbl).
CORE_DICT_KEYS <- c("water_entity_dictionary",
                    "water_infrastructure_dictionary",
                    "water_bodies_dictionary",
                    "water_gsa_dictionary",
                    "ca_utilities_dictionary",
                    "gov_entities_dict_core")

# === spaCy env ==============================================================
# Different users/machines will have different conda envs containing spaCy +
# en_core_web_trf, so SPACY_ENV is intentionally a user-edited setting here
# (not a filekey row that would be shared across the team). Set this to the
# name of your conda env or to a full path to a python binary. Step3 resolves
# names via reticulate::conda_list().
SPACY_ENV <- Sys.getenv("CORE_SPACY_ENV", unset = "spacy-env")

# === Filekey ================================================================
# Loaded once here and validated for unique var_name. Every step script can
# rely on `filekey` and `fk("key")` being available after sourcing this file.
filekey <- read.csv("filekey.csv", stringsAsFactors = FALSE)
.dups <- filekey$var_name[duplicated(filekey$var_name)]
if (length(.dups) > 0L) {
  stop("Duplicate var_name(s) in filekey.csv: ",
       paste(unique(.dups), collapse = ", "),
       ". Each var_name must be unique.")
}
rm(.dups)

# fk("some_var") -> filepath as a single string. Errors if the key is missing.
fk <- function(key) {
  row <- filekey[filekey$var_name == key, ]
  if (nrow(row) == 0L) stop("filekey.csv: no row with var_name = '", key, "'")
  row$filepath[1]
}

# === Atomic writes ==========================================================
# Long pipeline steps that get Ctrl-C'd, run out of disk, or crash mid-write
# would otherwise leave half-written outputs that satisfy file.exists() and
# get skipped on the next CLOBBER=FALSE rerun. atomic_write() lets the
# caller write to <out>.tmp and only renames into place on success.
#
# Usage:
#   atomic_write(path, function(p) write.csv(x, p, row.names = FALSE))
#   atomic_saveRDS(obj, path)
atomic_write <- function(out_path, writer_fn) {
  tmp <- paste0(out_path, ".tmp")
  on.exit(if (file.exists(tmp)) file.remove(tmp), add = TRUE)
  writer_fn(tmp)
  if (!file.exists(tmp)) {
    stop("atomic_write: writer_fn did not create ", tmp)
  }
  file.rename(tmp, out_path)
}

atomic_saveRDS <- function(obj, out_path, ...) {
  atomic_write(out_path, function(p) saveRDS(obj, p, ...))
}
