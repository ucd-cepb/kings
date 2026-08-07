#!/usr/bin/env Rscript
#
# run_all.R — regenerate the paper's page-similarity data products from the
# CURRENT core clean-text corpus (one parquet per gspDocId under core_txt_clean()).
#
# This runs the reliably core-runnable regeneration chain end to end:
#
#     [0] 00_ingest_core.R ...................... core -> paper bridge (optional)
#      1  text_preprocessing/preprocess_...R ..... -> data_products/page_metadata.RDS
#      2  text_reuse/hash_and_compare_pages.R ..... -> score_results/portal_page_scores_<date>.rds
#     [3] text_reuse/map_similarity.R + link ...... exploratory maps (optional; no product)
#
# It deliberately STOPS at the page-score products. It does NOT run:
#   - reference_extraction/  (needs anystyle/ruby + OpenAlex; external deps)
#   - knowledge_tree/        (predates the core-parquet refactor)
#   - modeling/              (consumes the two chains above, so only runnable once
#                             those products exist)
# Run those by hand once their inputs are in place — see ../README.md.
#
# Usage (from anywhere):
#     Network_Innovation_Paper/Code/run_all.R
#     RUN_INGEST=1 Network_Innovation_Paper/Code/run_all.R   # also rebuild the core->paper bridge first
#     RUN_MAPS=1   Network_Innovation_Paper/Code/run_all.R   # also draw the exploratory similarity maps
#
# Notes:
#   - Stage 2 (minhash/LSH) is memory-heavy and can take a while. Each stage runs
#     in its own Rscript process, so its memory is reclaimed before the next.
#   - Every path comes from Code/_paths.R (no path is hardcoded here); stages are
#     run from the repo root, matching the paths baked into the scripts.

# ---- Bootstrap: find _paths.R next to this script, then anchor at the repo root
.args <- commandArgs(trailingOnly = FALSE)
.file <- sub("^--file=", "", .args[grep("^--file=", .args)])
.here <- dirname(normalizePath(.file))
source(file.path(.here, "_paths.R"))    # defines REPO_ROOT, nip_*(), core_*()
setwd(REPO_ROOT)                        # stages source _paths.R via a repo-relative path
source(nip_code("_corpus.R"))           # for latest_page_scores()

RUN_INGEST <- Sys.getenv("RUN_INGEST", "0") == "1"  # 1 = re-run 00_ingest_core.R (CLOBBER=TRUE) first
RUN_MAPS   <- Sys.getenv("RUN_MAPS", "0") == "1"     # 1 = run the exploratory map/link scripts after

run <- function(script, env = character()) {
  cat("\n==== ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "  Rscript ", script, "\n", sep = "")
  status <- system2("Rscript", shQuote(script), env = env)
  if (status != 0L) stop("stage failed (exit ", status, "): ", script, call. = FALSE)
}

cat("run_all.R — regenerating page-similarity products from current core\n")
cat("repo root:", REPO_ROOT, "\n")

# ----- Stage 0 (optional): core -> paper bridge -----
# Writes data_products/{id_crosswalk,node_dictionary,all_gsa_edges}. The crosswalk
# is required by everything below, so ensure it exists (or rebuild it on request).
if (RUN_INGEST) {
  run(nip_code("00_ingest_core.R"), env = "CLOBBER=TRUE")
} else if (!file.exists(nip_product("id_crosswalk.csv"))) {
  stop("data_products/id_crosswalk.csv is missing.\n",
       "  Run once with RUN_INGEST=1 to build the core->paper bridge first.",
       call. = FALSE)
}

# ----- Stage 1: page_metadata.RDS from the core parquet corpus -----
run(nip_code("text_preprocessing", "preprocess_portal_texts.R"))

# ----- Stage 2: minhash + LSH page similarity (memory-heavy) -----
run(nip_code("text_reuse", "hash_and_compare_pages.R"))

# ----- Stage 3 (optional): exploratory spatial maps + section linkage -----
# These persist no product; they read the newest score file and draw/plot.
if (RUN_MAPS) {
  run(nip_code("text_reuse", "map_similarity.R"))
  run(nip_code("text_reuse", "link_page_lda_results_to_meta.R"))
}

cat("\n==== done.\n")
newest <- tryCatch(latest_page_scores(), error = function(e) NA_character_)
if (!is.na(newest)) cat("newest page-score file:", newest, "\n")
