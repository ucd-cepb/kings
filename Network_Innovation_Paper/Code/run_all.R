#!/usr/bin/env Rscript
#
# run_all.R — regenerate the paper's CRITICAL-PATH data products from the CURRENT
# core clean-text corpus (one parquet per gspDocId under core_txt_clean()).
#
# "Critical path" = everything that a 04_modeling/ script actually reads. This
# runs the reliably core-runnable part of that chain end to end:
#
#     [0]  00_ingest_core.R ........................ core manifest -> id_crosswalk.csv
#                                                    (pure bridge; no derivation, no LLM)
#     [1a] 01_entity_classification/build_overrides_from_dicts.R  core dicts -> gazetteer
#     [1b] 01_entity_classification/build_node_dictionary.R ..... LLM classify -> node_dictionary.csv
#     [1c] 01_entity_classification/build_gsa_edges.R ........... folds core graphs -> all_gsa_edges.csv
#      2   02_text_preprocessing/additional_filter_texts.R ...... -> data_products/page_metadata.RDS
#      3   03B_text_reuse/compare_project_sections.R ............ -> project_jaccard_results/project_section_jaccard_scores_<date>.rds
#
# 04_modeling/{make_networks,make_valued_networks,make_binary0.9_networks}.R read
# exactly three similarity products — the project-Jaccard file built by Stage 3,
# plus the reference (03A) and knowledge-triple (03C) products — on top of the
# bridge (id_crosswalk) and entity products (node_dictionary, all_gsa_edges). This
# orchestrator produces the 03B modeling input plus those bridge/entity products;
# the other two similarity chains and the models themselves are run by hand.
#
# Entity classification (Stage 1) is IN-PROJECT analysis, not ingest: core carries
# spaCy NER tags only, and the paper's semantic taxonomy has no upstream generator,
# so node_dictionary.csv is (re)built here by an LLM classifier and all_gsa_edges.csv
# is folded from the core graphs using those types. Both depend on the classifier,
# so they run AFTER the pure crosswalk bridge — hence 00 (bridge) then 01 (classify).
#
# It does NOT run:
#   - 03A_reference_extraction/  (needs anystyle/ruby + OpenAlex; external deps)
#                                -> data_products/gsp_reference_pairs.rds
#   - 03C_knowledge_tree/        (predates the core-parquet refactor)
#                                -> data_products/triple_similarity.csv
#   - 04_modeling/               (consumes all three 03 products, so only runnable
#                                once they exist)
#   - the EXPLORATORY page-score branch under 03B_text_reuse/explore/ (page scores +
#     the spatial similarity maps), a dead end w.r.t. the models — run it by hand.
# Run any of the above by hand once their inputs are in place — see ../README.md.
#
# Usage (from anywhere):
#     Network_Innovation_Paper/Code/run_all.R
#     RUN_INGEST=1 Network_Innovation_Paper/Code/run_all.R   # rebuild the bridge + entity products first
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

RUN_INGEST <- Sys.getenv("RUN_INGEST", "0") == "1"  # 1 = rebuild bridge + entity products (CLOBBER=TRUE) first

run <- function(script, env = character()) {
  cat("\n==== ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "  Rscript ", script, "\n", sep = "")
  status <- system2("Rscript", shQuote(script), env = env)
  if (status != 0L) stop("stage failed (exit ", status, "): ", script, call. = FALSE)
}

cat("run_all.R — regenerating critical-path modeling inputs from current core\n")
cat("repo root:", REPO_ROOT, "\n")

# ----- Stage 0 + 1 (optional): core bridge + entity classification -----
# Stage 0 is the ONLY ingest: id_crosswalk.csv, read straight from the core
# manifest (no derivation, no LLM). The crosswalk is required by everything below.
#
# Stage 1 is in-project entity analysis, gated the same way because it is the one
# API-touching, infrequently-changing block:
#   1a build_overrides_from_dicts.R bakes the core-dicts gazetteer the classifier
#      pins to, so it MUST run before the classifier;
#   1b build_node_dictionary.R runs the (cached) LLM classifier -> node_dictionary.csv;
#   1c build_gsa_edges.R folds the core weighted graphs down to the GSA-typed
#      entities -> all_gsa_edges.csv, so it reads node_dictionary.csv and runs last.
if (RUN_INGEST) {
  run(nip_code("00_ingest_core.R"), env = "CLOBBER=TRUE")
  run(nip_code("01_entity_classification", "build_overrides_from_dicts.R"))
  run(nip_code("01_entity_classification", "build_node_dictionary.R"), env = "CLOBBER=TRUE")
  run(nip_code("01_entity_classification", "build_gsa_edges.R"),       env = "CLOBBER=TRUE")
} else if (!file.exists(nip_product("id_crosswalk.csv"))) {
  stop("data_products/id_crosswalk.csv is missing.\n",
       "  Run once with RUN_INGEST=1 to build the core bridge + entity products first.",
       call. = FALSE)
}

# ----- Stage 2: page_metadata.RDS from the core parquet corpus -----
run(nip_code("02_text_preprocessing", "additional_filter_texts.R"))

# ----- Stage 3: project-section Jaccard similarity (the 03B modeling input) -----
# Reads page_metadata.RDS directly and writes
# project_jaccard_results/project_section_jaccard_scores_<date>.rds, the only 03B
# product any 04_modeling/ script consumes.
run(nip_code("03B_text_reuse", "compare_project_sections.R"))

cat("\n==== done.\n")
# Report the newest project-Jaccard product (Stage 3's output).
.jac_dir <- nip_product("project_jaccard_results")
.jac <- if (dir.exists(.jac_dir))
  sort(list.files(.jac_dir, pattern = "^project_section_jaccard_scores_.*\\.rds$",
                  full.names = TRUE), decreasing = TRUE)[1] else NA_character_
if (!is.na(.jac)) cat("newest project-Jaccard file:", .jac, "\n")
