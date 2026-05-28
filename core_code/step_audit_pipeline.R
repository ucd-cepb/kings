# step_audit_pipeline.R
# Cross-stage retention audit for the kings/GSP pipeline.
# Joins three stages on gsp_id:
#   (1) plan_txts_raw_core/<stem>.txt          — raw TSV from step1
#   (2) plan_txts_clean_core/<stem>.parquet    — cleaned by step2
#   (3) nondisambiged_extracts_core/<stem>.RDS — extracts from step3
#
# Reports file-level status, page retention totals, low-yield documents,
# and writes a per-GSP CSV to pipeline_audit.csv.
# Port of tijuana/code/_XX_helpers/audit_pipeline_correspondence.R.

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(stringr)
  library(purrr)
  library(tibble)
})

filekey <- read.csv("filekey.csv")

raw_dir     <- filekey[filekey$var_name == "plan_txts_raw_core",            ]$filepath
clean_dir   <- filekey[filekey$var_name == "plan_txts_clean_core",          ]$filepath
extract_dir <- filekey[filekey$var_name == "nondisambiged_extracts_core",   ]$filepath

cat("=== KINGS PIPELINE AUDIT ===\n\n")
cat("Raw text dir:   ", raw_dir,     "\n")
cat("Clean text dir: ", clean_dir,   "\n")
cat("Extract dir:    ", extract_dir, "\n\n")

stopifnot(
  "raw_dir not found"     = dir.exists(raw_dir),
  "clean_dir not found"   = dir.exists(clean_dir),
  "extract_dir not found" = dir.exists(extract_dir)
)

# ── 1. Raw inventory ─────────────────────────────────────────────────────────
raw_files <- list.files(raw_dir, pattern = "\\.txt$", full.names = TRUE)
cat("Stage 1 (raw TSVs):     ", length(raw_files), "\n")

raw_summary <- map_dfr(raw_files, function(f) {
  df <- tryCatch(read.delim(f, sep = "\t", quote = "",
                            colClasses = c(page = "integer", text = "character"),
                            stringsAsFactors = FALSE),
                 error = function(e) NULL)
  if (is.null(df)) {
    return(tibble(gsp_id = str_remove(basename(f), "\\.txt$"),
                  raw_n_pages = NA_integer_, raw_nonempty = NA_integer_))
  }
  tibble(gsp_id       = str_remove(basename(f), "\\.txt$"),
         raw_n_pages  = nrow(df),
         raw_nonempty = sum(nchar(trimws(df$text)) > 0, na.rm = TRUE))
})

# ── 2. Clean inventory ───────────────────────────────────────────────────────
clean_files <- list.files(clean_dir, pattern = "\\.parquet$", full.names = TRUE)
cat("Stage 2 (clean parquet):", length(clean_files), "\n")

clean_summary <- map_dfr(clean_files, function(f) {
  df <- tryCatch(arrow::read_parquet(f), error = function(e) NULL)
  if (is.null(df)) {
    return(tibble(gsp_id = str_remove(basename(f), "\\.parquet$"),
                  clean_n_pages = NA_integer_, clean_blanked = NA_integer_,
                  clean_usable = NA_integer_))
  }
  tibble(gsp_id        = str_remove(basename(f), "\\.parquet$"),
         clean_n_pages = nrow(df),
         clean_blanked = sum(nchar(trimws(df$text)) == 0 | is.na(df$text), na.rm = TRUE),
         clean_usable  = sum(nchar(trimws(df$text)) > 200, na.rm = TRUE))
})

# ── 3. Extract inventory ─────────────────────────────────────────────────────
extract_files <- list.files(extract_dir, pattern = "\\.RDS$", full.names = TRUE)
cat("Stage 3 (extract RDS):  ", length(extract_files), "\n\n")

extract_summary <- map_dfr(extract_files, function(f) {
  obj <- tryCatch(readRDS(f), error = function(e) NULL)
  if (is.null(obj)) {
    return(tibble(gsp_id  = str_remove(basename(f), "\\.RDS$"),
                  n_nodes = NA_integer_, n_edges = NA_integer_))
  }
  tibble(gsp_id  = str_remove(basename(f), "\\.RDS$"),
         n_nodes = nrow(obj$nodelist),
         n_edges = nrow(obj$edgelist))
})

# ── 4. Join + status ─────────────────────────────────────────────────────────
audit <- raw_summary %>%
  full_join(clean_summary,   by = "gsp_id") %>%
  full_join(extract_summary, by = "gsp_id") %>%
  mutate(
    pct_usable = round(100 * clean_usable / pmax(raw_n_pages, 1), 1),
    status = case_when(
      is.na(raw_n_pages)   ~ "missing_raw",
      is.na(clean_n_pages) ~ "missing_clean",
      is.na(n_edges)       ~ "missing_extract",
      n_edges == 0         ~ "empty_network",
      clean_usable == 0    ~ "no_usable_text",
      TRUE                 ~ "ok"
    )
  ) %>%
  arrange(gsp_id)

cat("File status summary:\n")
print(as.data.frame(count(audit, status, name = "n_files")), row.names = FALSE)

matched <- filter(audit, !is.na(raw_n_pages), !is.na(clean_n_pages))
total_raw     <- sum(matched$raw_n_pages,   na.rm = TRUE)
total_blanked <- sum(matched$clean_blanked, na.rm = TRUE)
total_usable  <- sum(matched$clean_usable,  na.rm = TRUE)
overall_pct   <- round(100 * total_usable / max(total_raw, 1), 1)

cat("\nPage retention (raw -> usable after cleaning):\n")
cat("  Raw pages total:    ", total_raw,                                       "\n")
cat("  Blanked by filters: ", total_blanked,
    sprintf("(%s%%)", round(100 * total_blanked / max(total_raw, 1), 1)),       "\n")
cat("  Usable (>200 chars):", total_usable,
    sprintf("(%s%% of raw)", overall_pct),                                     "\n\n")

cat("GSPs with <10%% usable pages (worth a manual look):\n")
low <- filter(audit, status == "ok", pct_usable < 10) %>%
  select(gsp_id, raw_n_pages, clean_usable, pct_usable) %>%
  arrange(pct_usable)
if (nrow(low) == 0) cat("  None\n") else print(as.data.frame(low), row.names = FALSE)

cat("\nGSPs with extract but no edges:\n")
empty_nets <- filter(audit, !is.na(n_edges), n_edges == 0)
if (nrow(empty_nets) == 0) cat("  None\n") else print(as.data.frame(empty_nets), row.names = FALSE)

# ── 5. Write per-GSP detail CSV ──────────────────────────────────────────────
out_csv <- filekey[filekey$var_name == "audit_csv_core", ]$filepath
dir.create(dirname(out_csv), recursive = TRUE, showWarnings = FALSE)
write.csv(audit, out_csv, row.names = FALSE)
cat(sprintf("\nWrote audit summary to %s\n", out_csv))
