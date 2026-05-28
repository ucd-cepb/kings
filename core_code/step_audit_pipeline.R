# step_audit_pipeline.R
# Cross-stage retention audit for the kings/GSP pipeline.
# Joins five stages on gsp_id:
#   (1) plan_txts_raw_core/<stem>.txt                          — step1
#   (2) plan_txts_clean_core/<stem>.parquet                    — step2
#   (3) nondisambiged_extracts_core/<stem>.RDS                 — step3
#   (4) disambiged_extracts_core/<stem>.RDS                    — step4
#   (5) multiplex/uniplex graphs/<stem>.RDS                    — step5
#
# Reports file-level status, page retention totals, low-yield documents,
# disambiguation row-count deltas, and writes a per-GSP CSV to
# pipeline_audit.csv.
# Port of tijuana/code/_XX_helpers/audit_pipeline_correspondence.R.

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(stringr)
  library(purrr)
  library(tibble)
  library(igraph)
})

source("core_code/_config.R")   # provides MIN_PAGE_CHARS, filekey, fk()

raw_dir       <- fk("plan_txts_raw_core")
clean_dir     <- fk("plan_txts_clean_core")
extract_dir   <- fk("nondisambiged_extracts_core")
disambig_dir  <- fk("disambiged_extracts_core")
multiplex_dir <- fk("multiplex_directed_graphs_core")
uniplex_dir   <- fk("uniplex_weighted_graphs_core")

cat("=== KINGS PIPELINE AUDIT ===\n\n")
cat("Raw text dir:   ", raw_dir,       "\n")
cat("Clean text dir: ", clean_dir,     "\n")
cat("Extract dir:    ", extract_dir,   "\n")
cat("Disambig dir:   ", disambig_dir,  "\n")
cat("Multiplex dir:  ", multiplex_dir, "\n")
cat("Uniplex dir:    ", uniplex_dir,   "\n\n")

# Only steps 1-3 dirs are hard-required (audit's been around since then).
# Steps 4-5 are optional in the audit: missing dirs are reported, not fatal.
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
         clean_usable  = sum(nchar(trimws(df$text)) > MIN_PAGE_CHARS, na.rm = TRUE))
})

# ── 3. Extract inventory ─────────────────────────────────────────────────────
extract_files <- list.files(extract_dir, pattern = "\\.RDS$", full.names = TRUE)
cat("Stage 3 (extract RDS):  ", length(extract_files), "\n")

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

# ── 4. Disambiguated extracts (step4) ────────────────────────────────────────
disambig_files <- if (dir.exists(disambig_dir)) {
  list.files(disambig_dir, pattern = "\\.RDS$", full.names = TRUE)
} else character(0)
cat("Stage 4 (disambig RDS): ", length(disambig_files),
    if (!dir.exists(disambig_dir)) "  [dir missing]" else "", "\n")

disambig_summary <- map_dfr(disambig_files, function(f) {
  obj <- tryCatch(readRDS(f), error = function(e) NULL)
  if (is.null(obj)) {
    return(tibble(gsp_id = str_remove(basename(f), "\\.RDS$"),
                  n_nodes_disambig = NA_integer_,
                  n_edges_disambig = NA_integer_))
  }
  tibble(gsp_id = str_remove(basename(f), "\\.RDS$"),
         n_nodes_disambig = nrow(obj$nodelist),
         n_edges_disambig = nrow(obj$edgelist))
})

# ── 5. Graph objects (step5) ─────────────────────────────────────────────────
# Multiplex and uniplex graphs are paired per GSP. Count both, and read the
# multiplex to surface vertex/edge counts. Uniplex (collapsed) only gets a
# presence flag — its counts are derivable from the multiplex's parallel-edge
# folding, and reading both would double the I/O.
multiplex_files <- if (dir.exists(multiplex_dir)) {
  list.files(multiplex_dir, pattern = "\\.RDS$", full.names = TRUE)
} else character(0)
uniplex_files <- if (dir.exists(uniplex_dir)) {
  list.files(uniplex_dir, pattern = "\\.RDS$", full.names = TRUE)
} else character(0)
cat("Stage 5 (multiplex):    ", length(multiplex_files),
    if (!dir.exists(multiplex_dir)) "  [dir missing]" else "", "\n")
cat("Stage 5 (uniplex):      ", length(uniplex_files),
    if (!dir.exists(uniplex_dir)) "  [dir missing]" else "", "\n\n")

graph_summary <- map_dfr(multiplex_files, function(f) {
  g <- tryCatch(readRDS(f), error = function(e) NULL)
  stem <- str_remove(basename(f), "\\.RDS$")
  has_uniplex <- file.exists(file.path(uniplex_dir, paste0(stem, ".RDS")))
  if (is.null(g)) {
    return(tibble(gsp_id = stem,
                  n_vertices = NA_integer_, n_mp_edges = NA_integer_,
                  has_uniplex = has_uniplex))
  }
  tibble(gsp_id = stem,
         n_vertices  = igraph::vcount(g),
         n_mp_edges  = igraph::ecount(g),
         has_uniplex = has_uniplex)
})

# ── 6. Join + status ─────────────────────────────────────────────────────────
audit <- raw_summary %>%
  full_join(clean_summary,    by = "gsp_id") %>%
  full_join(extract_summary,  by = "gsp_id") %>%
  full_join(disambig_summary, by = "gsp_id") %>%
  full_join(graph_summary,    by = "gsp_id") %>%
  mutate(
    pct_usable = round(100 * clean_usable / pmax(raw_n_pages, 1), 1),
    # disambig collapses surface variants of the same entity, so node count
    # should drop by >= 0 (never grow). pct_disambig_kept shows the fraction
    # of step3 nodes that survived disambiguation as distinct vertices.
    pct_disambig_kept = round(100 * n_nodes_disambig / pmax(n_nodes, 1), 1),
    status = case_when(
      is.na(raw_n_pages)         ~ "missing_raw",
      is.na(clean_n_pages)       ~ "missing_clean",
      is.na(n_edges)             ~ "missing_extract",
      is.na(n_edges_disambig)    ~ "missing_disambig",
      is.na(n_mp_edges)          ~ "missing_graph",
      !isTRUE(has_uniplex)       ~ "missing_uniplex",
      n_mp_edges == 0            ~ "empty_graph",
      n_edges == 0               ~ "empty_extract",
      clean_usable == 0          ~ "no_usable_text",
      TRUE                       ~ "ok"
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
cat(sprintf("  Usable (>%d chars):", MIN_PAGE_CHARS), total_usable,
    sprintf("(%s%% of raw)", overall_pct),                                     "\n\n")

cat("GSPs with <10%% usable pages (worth a manual look):\n")
low <- filter(audit, status == "ok", pct_usable < 10) %>%
  select(gsp_id, raw_n_pages, clean_usable, pct_usable) %>%
  arrange(pct_usable)
if (nrow(low) == 0) cat("  None\n") else print(as.data.frame(low), row.names = FALSE)

cat("\nGSPs with extract but no edges:\n")
empty_nets <- filter(audit, !is.na(n_edges), n_edges == 0)
if (nrow(empty_nets) == 0) cat("  None\n") else print(as.data.frame(empty_nets), row.names = FALSE)

# ── 7. Disambig / graph health ───────────────────────────────────────────────
cat("\nDisambiguation node retention (step3 -> step4):\n")
disambig_done <- filter(audit, !is.na(n_nodes), !is.na(n_nodes_disambig))
if (nrow(disambig_done) == 0L) {
  cat("  No GSPs with both step3 and step4 outputs.\n")
} else {
  cat(sprintf("  %d GSP(s) processed by step4. Median nodes kept: %.1f%%; ",
              nrow(disambig_done),
              median(disambig_done$pct_disambig_kept, na.rm = TRUE)))
  cat(sprintf("range %.0f%%–%.0f%%.\n",
              min(disambig_done$pct_disambig_kept, na.rm = TRUE),
              max(disambig_done$pct_disambig_kept, na.rm = TRUE)))
  high_collapse <- filter(disambig_done, pct_disambig_kept < 50)
  if (nrow(high_collapse) > 0L) {
    cat("\n  GSPs with <50% disambig node retention (worth a manual look):\n")
    print(as.data.frame(
      select(arrange(high_collapse, pct_disambig_kept),
             gsp_id, n_nodes, n_nodes_disambig, pct_disambig_kept)),
      row.names = FALSE)
  }
}

cat("\nGraph stage:\n")
graph_done <- filter(audit, !is.na(n_mp_edges))
cat(sprintf("  Built: %d  (uniplex paired: %d)\n",
            nrow(graph_done), sum(graph_done$has_uniplex, na.rm = TRUE)))
empty_graphs <- filter(audit, !is.na(n_mp_edges), n_mp_edges == 0)
if (nrow(empty_graphs) > 0L) {
  cat("\n  GSPs with built graph but zero multiplex edges:\n")
  print(as.data.frame(select(empty_graphs, gsp_id, n_vertices, n_mp_edges)),
        row.names = FALSE)
}

# ── 8. Write per-GSP detail CSV ──────────────────────────────────────────────
out_csv <- fk("audit_csv_core")
dir.create(dirname(out_csv), recursive = TRUE, showWarnings = FALSE)
atomic_write(out_csv, function(p) write.csv(audit, p, row.names = FALSE))
cat(sprintf("\nWrote audit summary to %s\n", out_csv))
