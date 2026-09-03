#!/usr/bin/env Rscript
#
# run_all.R — regenerate the paper's CRITICAL-PATH data products from the CURRENT
# core clean-text corpus (one parquet per gspDocId under core_txt_clean()).
#
# "Critical path" = everything that a 04_modeling/ script actually reads. This
# runs the reliably core-runnable part of that chain end to end:
#
#     [0a] 01_entity_classification/build_overrides_from_dicts.R  core dicts -> gazetteer (optional)
#     [0b] 00_ingest_core.R ........................ core -> paper bridge; classifies (optional)
#                                                    -> data_products/{id_crosswalk,node_dictionary,all_gsa_edges}
#      1   02_text_preprocessing/preprocess_...R ... -> data_products/page_metadata.RDS
#      2   03B_text_reuse/compare_project_sections.R -> project_jaccard_results/project_section_jaccard_scores_<date>.rds
#
# 04_modeling/{make_networks,make_valued_networks,make_binary0.9_networks}.R read
# exactly three similarity products — the project-Jaccard file built by Stage 2,
# plus the reference (03A) and knowledge-triple (03C) products — on top of the
# ingest bridge files. This orchestrator produces the 03B modeling input; the
# other two chains and the models themselves are run by hand (see below).
#
# It does NOT run:
#   - 03A_reference_extraction/  (needs anystyle/ruby + OpenAlex; external deps)
#                                -> data_products/gsp_reference_pairs.rds
#   - 03C_knowledge_tree/        (predates the core-parquet refactor)
#                                -> data_products/triple_similarity.csv
#   - 04_modeling/               (consumes all three 03 products, so only runnable
#                                once they exist)
#   - the EXPLORATORY page-score branch: 03B_text_reuse/hash_and_compare_pages.R
#     (-> portal_page_scores_<date>.rds) and its only consumers,
#     map_similarity.R / link_page_lda_results_to_meta.R (which persist no product
#     and feed nothing downstream). This branch is a dead end w.r.t. the models,
#     so it is intentionally left out of run_all — run it by hand if you want the
#     page-level scores or the spatial similarity maps.
# Run any of the above by hand once their inputs are in place — see ../README.md.
#
# Usage (from anywhere):
#     Network_Innovation_Paper/Code/run_all.R
#     RUN_INGEST=1 Network_Innovation_Paper/Code/run_all.R   # refresh the gazetteer + rebuild the core->paper bridge first
#
# Notes:
#   - Each stage runs in its own Rscript process, so its memory is reclaimed
#     before the next.
#   - Every path comes from Code/_paths.R (no path is hardcoded here); stages are
#     run from the repo root, matching the paths baked into the scripts.

# ---- Bootstrap: find _paths.R next to this script, then anchor at the repo root
.args <- commandArgs(trailingOnly = FALSE)
.file <- sub("^--file=", "", .args[grep("^--file=", .args)])
.here <- dirname(normalizePath(.file))
source(file.path(.here, "_paths.R"))    # defines REPO_ROOT, nip_*(), core_*()
setwd(REPO_ROOT)                        # stages source _paths.R via a repo-relative path

RUN_INGEST <- Sys.getenv("RUN_INGEST", "0") == "1"  # 1 = re-run 00_ingest_core.R (CLOBBER=TRUE) first

run <- function(script, env = character()) {
  cat("\n==== ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "  Rscript ", script, "\n", sep = "")
  status <- system2("Rscript", shQuote(script), env = env)
  if (status != 0L) stop("stage failed (exit ", status, "): ", script, call. = FALSE)
}

cat("run_all.R — regenerating critical-path modeling inputs from current core\n")
cat("repo root:", REPO_ROOT, "\n")

# ----- Stage 0 (optional): entity gazetteer + core -> paper bridge -----
# 00_ingest_core.R writes data_products/{id_crosswalk,node_dictionary,all_gsa_edges}
# and runs the entity classifier (01_entity_classification/classify_entities.R)
# internally to build node_dictionary. The classifier reads its authoritative
# override table, inputs/entity_type_overrides.csv, which is baked from the current
# core_code/dicts gazetteers — so refresh that FIRST, then ingest. The crosswalk is
# required by everything below, so ensure it exists (or rebuild it on request).
if (RUN_INGEST) {
  run(nip_code("01_entity_classification", "build_overrides_from_dicts.R"))
  run(nip_code("00_ingest_core.R"), env = "CLOBBER=TRUE")
} else if (!file.exists(nip_product("id_crosswalk.csv"))) {
  stop("data_products/id_crosswalk.csv is missing.\n",
       "  Run once with RUN_INGEST=1 to build the core->paper bridge first.",
       call. = FALSE)
}

# ----- Stage 1: page_metadata.RDS from the core parquet corpus -----
run(nip_code("02_text_preprocessing", "preprocess_portal_texts.R"))

# ----- Stage 2: project-section Jaccard similarity (the 03B modeling input) -----
# Reads page_metadata.RDS directly (independent of the page-score branch) and
# writes project_jaccard_results/project_section_jaccard_scores_<date>.rds, the
# only 03B product any 04_modeling/ script consumes.
run(nip_code("03B_text_reuse", "compare_project_sections.R"))

cat("\n==== done.\n")
# Report the newest project-Jaccard product (Stage 2's output).
.jac_dir <- nip_product("project_jaccard_results")
.jac <- if (dir.exists(.jac_dir))
  sort(list.files(.jac_dir, pattern = "^project_section_jaccard_scores_.*\\.rds$",
                  full.names = TRUE), decreasing = TRUE)[1] else NA_character_
if (!is.na(.jac)) cat("newest project-Jaccard file:", .jac, "\n")
