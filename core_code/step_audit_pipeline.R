# step_audit_pipeline.R
# Per-GSP retention audit. Eight metrics across the pipeline:
#   pdf_pages         pages in the source PDF
#   raw_rows          rows in the raw .txt TSV (one per extracted page)
#   clean_nonblank    rows in the clean parquet whose text is not blank
#   extract_nodes     nrow(nondisambig$nodelist)
#   extract_edges     nrow(nondisambig$edgelist)
#   disambig_nodes    nrow(disambig$nodelist)
#   graph_nodes       igraph::vcount(multiplex)
#   graph_edges       igraph::ecount(multiplex)
# Writes per-GSP CSV.

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(tibble)
  library(igraph)
  library(pbapply)
  library(data.table)
  library(arrow)
  library(pdftools)
})

source("core_code/_config.R")   # provides filekey, fk()

pbapply::pboptions(type = "txt", style = 3, char = "=")

audit_pblapply <- function(files, fn, label) {
  if (length(files) == 0L) return(tibble())
  cat(sprintf("  scanning %s (%d file(s))...\n", label, length(files)))
  rows <- pbapply::pblapply(files, fn)
  data.table::rbindlist(rows, fill = TRUE) %>% tibble::as_tibble()
}

list_or_empty <- function(d, pat) {
  if (dir.exists(d)) list.files(d, pattern = pat, full.names = TRUE)
  else character(0)
}

# Build the same stem step1 would produce from this PDF basename. Under
# the docId convention (step0), gsp_doc_id_<gspDocId>.pdf -> "<gspDocId>".
# Older legacy names — (v<N>_)?gsp_num_id_<NNNN>.pdf — still parse via the
# trailing-digits extraction, producing "v1_0007" or "0007".
pdf_stem <- function(basename_pdf) {
  s        <- str_remove(basename_pdf, "\\.pdf$")
  vsn      <- str_extract(s, "^v[0-9]+")
  trailing <- str_extract(s, "[0-9]+$")
  ifelse(is.na(vsn), trailing, paste(vsn, trailing, sep = "_"))
}

# Fast TSV row count: scan raw bytes, count newlines, subtract header.
count_data_rows <- function(f, chunk = 1L * 1024L * 1024L) {
  con <- tryCatch(file(f, "rb"), error = function(e) NULL)
  if (is.null(con)) return(NA_integer_)
  on.exit(close(con))
  n <- 0L
  repeat {
    buf <- readBin(con, what = "raw", n = chunk)
    if (length(buf) == 0L) break
    n <- n + sum(buf == as.raw(10L))
  }
  max(n - 1L, 0L)
}

# Directories ---------------------------------------------------------------
pdf_dir       <- fk("core_data_source_pdfs")
raw_dir       <- fk("plan_txts_raw_core")
clean_dir     <- fk("plan_txts_clean_core")
extract_dir   <- fk("nondisambiged_extracts_core")
disambig_dir  <- fk("disambiged_extracts_core")
multiplex_dir <- fk("multiplex_directed_graphs_core")

cat("=== KINGS PIPELINE AUDIT ===\n\n")
cat("Source PDFs:    ", pdf_dir,       "\n")
cat("Raw .txt:       ", raw_dir,       "\n")
cat("Clean parquet:  ", clean_dir,     "\n")
cat("Extract RDS:    ", extract_dir,   "\n")
cat("Disambig RDS:   ", disambig_dir,  "\n")
cat("Multiplex RDS:  ", multiplex_dir, "\n\n")

# 1. PDF page counts --------------------------------------------------------
pdf_files <- list_or_empty(pdf_dir, "\\.pdf$")
cat(sprintf("PDFs:            %d\n", length(pdf_files)))
pdf_summary <- audit_pblapply(pdf_files, function(f) {
  info <- tryCatch(pdftools::pdf_info(f), error = function(e) NULL)
  tibble::tibble(gsp_id    = pdf_stem(basename(f)),
                 pdf_pages = if (is.null(info)) NA_integer_ else as.integer(info$pages))
}, label = "PDF page counts")

# 2. Raw TSV row counts -----------------------------------------------------
raw_files <- list_or_empty(raw_dir, "\\.txt$")
cat(sprintf("Raw .txt:        %d\n", length(raw_files)))
raw_summary <- audit_pblapply(raw_files, function(f) {
  tibble::tibble(gsp_id   = str_remove(basename(f), "\\.txt$"),
                 raw_rows = count_data_rows(f))
}, label = "raw .txt row counts")

# 3. Clean parquet non-blank rows -------------------------------------------
clean_files <- list_or_empty(clean_dir, "\\.parquet$")
cat(sprintf("Clean parquet:   %d\n", length(clean_files)))
clean_summary <- audit_pblapply(clean_files, function(f) {
  df <- tryCatch(arrow::read_parquet(f, col_select = "text"),
                 error = function(e) NULL)
  tibble::tibble(
    gsp_id         = str_remove(basename(f), "\\.parquet$"),
    clean_nonblank = if (is.null(df)) NA_integer_
                     else sum(!is.na(df$text) & nchar(trimws(df$text)) > 0L)
  )
}, label = "clean parquet non-blank rows")

# 4. Nondisambig extract sizes ----------------------------------------------
extract_files <- list_or_empty(extract_dir, "\\.RDS$")
cat(sprintf("Extract RDS:     %d\n", length(extract_files)))
extract_summary <- audit_pblapply(extract_files, function(f) {
  obj <- tryCatch(readRDS(f), error = function(e) NULL)
  tibble::tibble(
    gsp_id        = str_remove(basename(f), "\\.RDS$"),
    extract_nodes = if (is.null(obj)) NA_integer_ else nrow(obj$nodelist),
    extract_edges = if (is.null(obj)) NA_integer_ else nrow(obj$edgelist)
  )
}, label = "extract RDS sizes")

# 5. Disambig nodes ---------------------------------------------------------
disambig_files <- list_or_empty(disambig_dir, "\\.RDS$")
cat(sprintf("Disambig RDS:    %d\n", length(disambig_files)))
disambig_summary <- audit_pblapply(disambig_files, function(f) {
  obj <- tryCatch(readRDS(f), error = function(e) NULL)
  tibble::tibble(
    gsp_id         = str_remove(basename(f), "\\.RDS$"),
    disambig_nodes = if (is.null(obj)) NA_integer_ else nrow(obj$nodelist)
  )
}, label = "disambig RDS sizes")

# 6. Multiplex graph sizes --------------------------------------------------
multiplex_files <- list_or_empty(multiplex_dir, "\\.RDS$")
cat(sprintf("Multiplex RDS:   %d\n\n", length(multiplex_files)))
graph_summary <- audit_pblapply(multiplex_files, function(f) {
  g <- tryCatch(readRDS(f), error = function(e) NULL)
  tibble::tibble(
    gsp_id      = str_remove(basename(f), "\\.RDS$"),
    graph_nodes = if (is.null(g)) NA_integer_ else igraph::vcount(g),
    graph_edges = if (is.null(g)) NA_integer_ else igraph::ecount(g)
  )
}, label = "multiplex graph sizes")

# Join ----------------------------------------------------------------------
audit <- pdf_summary %>%
  full_join(raw_summary,      by = "gsp_id") %>%
  full_join(clean_summary,    by = "gsp_id") %>%
  full_join(extract_summary,  by = "gsp_id") %>%
  full_join(disambig_summary, by = "gsp_id") %>%
  full_join(graph_summary,    by = "gsp_id") %>%
  mutate(status = case_when(
    is.na(pdf_pages)      ~ "missing_pdf",
    is.na(raw_rows)       ~ "missing_raw",
    is.na(clean_nonblank) ~ "missing_clean",
    is.na(extract_nodes)  ~ "missing_extract",
    is.na(disambig_nodes) ~ "missing_disambig",
    is.na(graph_nodes)    ~ "missing_graph",
    TRUE                  ~ "ok"
  )) %>%
  arrange(gsp_id)

cat("File status summary:\n")
print(as.data.frame(count(audit, status, name = "n_files")), row.names = FALSE)

out_csv <- fk("audit_csv_core")
dir.create(dirname(out_csv), recursive = TRUE, showWarnings = FALSE)
atomic_write(out_csv, function(p) write.csv(audit, p, row.names = FALSE))
cat(sprintf("\nWrote audit summary to %s\n", out_csv))
