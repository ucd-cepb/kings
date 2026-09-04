#!/usr/bin/env Rscript
#
# run_all.R — rebuild the 04_modeling/ inputs from the core clean-text corpus
# (one parquet per gspDocId under core_txt_clean()).
#
# Stages (-> = output written; products live under data_products/<stage>/ mirroring
# the code stage that writes them; results (figures/tables) live under outputs/):
#   [0]  00_ingest_core.R ....................................... -> 00_ingest/id_crosswalk.csv
#   [1a] 01_entity_classification/build_overrides_from_dicts.R .. -> inputs/entity_type_overrides.csv
#   [1b] 01_entity_classification/build_node_dictionary.R ....... -> 01_entity_classification/node_dictionary.csv     (LLM)
#   [1c] 01_entity_classification/build_gsa_edges.R ............. -> 01_entity_classification/all_gsa_edges.csv
#   [2]  02_text_preprocessing/additional_filter_texts.R ....... -> 02_text_preprocessing/page_metadata.RDS
#   [3A] 03A_reference_extraction/01..05 (5 scripts) ........... -> 03A_reference_extraction/gsp_reference_pairs.rds
#   [3B] 03B_text_reuse/compare_project_sections.R ............. -> 03B_text_reuse/project_jaccard_results/project_section_jaccard_scores.rds
#   [3C] 03C_knowledge_tree/01_extract_knowledge_triples.R +
#        03C_knowledge_tree/02_semantic_kg_similarity.ipynb ....... -> 03C_knowledge_tree/triple_similarity.csv
#
# Stage 0 is a bridge: it reads the core manifest into id_crosswalk.csv, no
# derivation, no LLM. Stage 1 is analysis, not ingest — core carries spaCy NER
# tags only, so the paper's semantic types are (re)built here: 1a bakes the
# override table from core dicts, 1b classifies entity names with an LLM, 1c folds
# the core graphs into GSA edges. Stage 1 runs after Stage 0 because 1c keys
# through the crosswalk.
#
# The models read THREE plan-to-plan similarity products, one from each Stage 3
# branch — all now buildable here (each behind its own toggle):
#   - 3A references     -> 03A_reference_extraction/gsp_reference_pairs.rds
#   - 3B project-Jaccard -> 03B_text_reuse/project_jaccard_results/project_section_jaccard_scores.rds
#   - 3C knowledge-tree -> 03C_knowledge_tree/triple_similarity.csv
# Only the 03B_text_reuse/explore/ page-score + map branch stays out (it feeds
# nothing downstream — run it by hand). See ../README.md.
#
# EXTERNAL DEPENDENCIES (why 3A + 3C default OFF — they are heavy and need tools
# beyond R that a plain checkout may lack):
#   - 3A: anystyle/ruby (reference extraction, step 01) + OpenAlex API (step 03) +
#         a Solr title-match index (step 04). Step 01 self-guards with CLOBBER and
#         only re-parses PDFs it has not already cached.
#   - 3C: `jupyter nbconvert` on PATH; the notebook is pinned to the `spacy-env`
#         kernel, which carries its deps (sentence-transformers, scikit-learn,
#         networkx, pandas, numpy). Register it once with
#         `python -m ipykernel install --user --name spacy-env` from that env.
#
# Usage (from anywhere):
#     Network_Innovation_Paper/Code/run_all.R
#     RUN_INGEST=1 Network_Innovation_Paper/Code/run_all.R   # also rebuild stages 0 + 1
#
# ############################################################################
# ##  WHICH STAGES RUN — this is the part you edit.                         ##
# ############################################################################
# Flip a stage TRUE/FALSE in the CONFIG block below, then run the file.
#   * Stages 0 + 1 default OFF: the bridge and the classifier change rarely, and
#     Stage 1 costs Anthropic API calls and needs a key. Turn on when core data
#     or the taxonomy changed.
#   * Stages 2 + 3B default ON: the fast, deterministic path to the model input.
#   * Stages 3A + 3C default OFF: intensive and need the external tools above.
#     Toggle on (per the run at hand) when you want to rebuild those two model
#     inputs — the per-stage toggle is exactly what makes it safe to keep these
#     heavy branches in run_all.
# RUN_INGEST=1 forces stages 0 + 1 on for one run without editing the toggles.
#
# Each stage runs in its own Rscript process (memory freed between stages) from
# the repo root, matching the paths in Code/_paths.R. No paths are hardcoded here.

# ---- Bootstrap: find _paths.R next to this script, then anchor at the repo root.
# Handles both `Rscript run_all.R` and source() in R/RStudio (each exposes the
# script's path differently).
.find_here <- function() {
  # (a) Rscript: the --file= command-line argument
  a <- commandArgs(trailingOnly = FALSE)
  m <- grep("^--file=", a, value = TRUE)
  if (length(m)) return(dirname(normalizePath(sub("^--file=", "", m[1]))))
  # (b) source()'d from a file: path lives as `ofile` in a source() frame
  for (i in seq_len(sys.nframe())) {
    of <- sys.frame(i)$ofile
    if (!is.null(of)) return(dirname(normalizePath(of)))
  }
  # (c) RStudio Source/Run button, no saved frame path
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- tryCatch(rstudioapi::getSourceEditorContext()$path, error = function(e) "")
    if (nzchar(p)) return(dirname(normalizePath(p)))
  }
  # (d) fallback: _paths.R relative to the working directory
  #     (running line-by-line from the repo root or Code/)
  for (cand in c("_paths.R", file.path("Network_Innovation_Paper", "Code", "_paths.R")))
    if (file.exists(cand)) return(dirname(normalizePath(cand)))
  stop("run_all.R: can't locate its own directory. Launch it with Rscript, source() it ",
       "from the file, or setwd() to Network_Innovation_Paper/Code (or the repo root) first.",
       call. = FALSE)
}
.here <- .find_here()
source(file.path(.here, "_paths.R"))    # defines REPO_ROOT, nip_*(), core_*()
setwd(REPO_ROOT)                        # stages source _paths.R via a repo-relative path

# ============================================================================
#  CONFIG — flip a stage TRUE/FALSE to run/skip it.  (edit here)
# ============================================================================
STAGE_0_INGEST      <- FALSE   # 00_ingest_core.R                     -> id_crosswalk.csv        (bridge; no LLM)
STAGE_1_CLASSIFY    <- FALSE # 01_entity_classification/* (3 steps) -> node_dictionary.csv, all_gsa_edges.csv  (LLM; needs API key)
STAGE_2_PREPROCESS  <- FALSE   # 02_text_preprocessing/additional_filter_texts.R -> page_metadata.RDS
STAGE_3A_REFERENCES <- TRUE  # 03A_reference_extraction/01..05      -> gsp_reference_pairs.rds  (anystyle/ruby + OpenAlex + Solr)
STAGE_3B_JACCARD    <- FALSE   # 03B_text_reuse/compare_project_sections.R        -> project_section_jaccard_scores.rds
STAGE_3C_KNOWLEDGE  <- FALSE  # 03C_knowledge_tree/{01_extract_knowledge_triples.R, 02_semantic_kg_similarity.ipynb} -> triple_similarity.csv  (needs jupyter)
# ============================================================================

# RUN_INGEST=1 forces stages 0 + 1 on for this run only.
if (Sys.getenv("RUN_INGEST", "0") == "1") {
  STAGE_0_INGEST   <- TRUE
  STAGE_1_CLASSIFY <- TRUE
}

run <- function(script, env = character()) {
  cat("\n==== ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "  Rscript ", script, "\n", sep = "")
  status <- system2("Rscript", shQuote(script), env = env)
  if (status != 0L) stop("stage failed (exit ", status, "): ", script, call. = FALSE)
}

# Execute a Jupyter notebook headless (Stage 3C's similarity step). The executed
# copy is thrown away in a temp dir so the tracked .ipynb stays clean; the notebook
# writes its real product (triple_similarity.csv) to data_products itself. nbconvert
# runs the kernel in the notebook's own directory, so the notebook's ../../ paths
# resolve to Network_Innovation_Paper/data_products/03C_knowledge_tree/ regardless of getwd().
run_nb <- function(nb) {
  cat("\n==== ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "  jupyter nbconvert ", nb, "\n", sep = "")
  if (!nzchar(Sys.which("jupyter")))
    stop("Stage 3C needs `jupyter` on PATH to execute ", basename(nb),
         ".\n  Install Jupyter (e.g. conda/pip) or set STAGE_3C_KNOWLEDGE <- FALSE.",
         call. = FALSE)
  outdir <- tempfile("nbrun_"); dir.create(outdir)
  on.exit(unlink(outdir, recursive = TRUE), add = TRUE)
  status <- system2("jupyter", c("nbconvert", "--to", "notebook", "--execute",
                                 "--ExecutePreprocessor.timeout=-1",
                                 shQuote(nb), "--output-dir", shQuote(outdir)))
  if (status != 0L) stop("stage failed (exit ", status, "): ", nb, call. = FALSE)
}

# An OFF stage must find its input on disk, else stop and name the toggle that
# builds it — so a skipped stage never feeds missing data downstream.
need <- function(product, toggle_name) {
  if (!file.exists(nip_product(product)))
    stop("data_products/", product, " is missing and its stage is off.\n",
         "  Set ", toggle_name, " <- TRUE at the top of run_all.R (or pass RUN_INGEST=1) to build it.",
         call. = FALSE)
}

cat("run_all.R — rebuilding modeling inputs from current core\n")
cat("repo root:", REPO_ROOT, "\n")
cat(sprintf("stages: 0_ingest=%s  1_classify=%s  2_preprocess=%s  3A_references=%s  3B_jaccard=%s  3C_knowledge=%s\n",
            STAGE_0_INGEST, STAGE_1_CLASSIFY, STAGE_2_PREPROCESS,
            STAGE_3A_REFERENCES, STAGE_3B_JACCARD, STAGE_3C_KNOWLEDGE))

# ----- Stage 0: ingest -----
# id_crosswalk.csv from the core manifest. Required by everything below.
if (STAGE_0_INGEST) {
  run(nip_code("00_ingest_core.R"), env = "CLOBBER=TRUE")
}

# ----- Stage 1: entity classification (LLM) -----
#   1a build_overrides_from_dicts.R -> entity_type_overrides.csv, the table the
#      classifier pins to; runs first.
#   1b build_node_dictionary.R runs the (cached) LLM classifier -> node_dictionary.csv.
#   1c build_gsa_edges.R folds the core graphs to GSA-typed entities ->
#      all_gsa_edges.csv; reads node_dictionary.csv, so runs last.
if (STAGE_1_CLASSIFY) {
  need("00_ingest/id_crosswalk.csv", "STAGE_0_INGEST")  # 1c's edge fold keys through the crosswalk
  run(nip_code("01_entity_classification", "build_overrides_from_dicts.R"))
  run(nip_code("01_entity_classification", "build_node_dictionary.R"), env = "CLOBBER=TRUE")
  run(nip_code("01_entity_classification", "build_gsa_edges.R"),       env = "CLOBBER=TRUE")
}

# ----- Stage 2: page_metadata.RDS from the core parquet corpus -----
# Recovers legacy gsp_id/version via id_crosswalk.csv. Writes a metadata-only
# table (keys + section flags, ~0.24 MB); full page text stays in core and is
# re-attached on demand by 03B via attach_page_text() (_corpus.R).
if (STAGE_2_PREPROCESS) {
  need("00_ingest/id_crosswalk.csv", "STAGE_0_INGEST")
  run(nip_code("02_text_preprocessing", "additional_filter_texts.R"))
}

# ----- Stage 3A: reference-overlap similarity (model input) -----
# The 5-script bibliographic chain, run in numeric order. 01 extracts reference
# strings from the core PDFs (anystyle/ruby; self-guards with CLOBBER so it only
# parses PDFs it has not cached), 02 aggregates/classifies them, 03 queries
# OpenAlex, 04 searches the Solr title-match index, 05 folds matched reference
# sets into plan-to-plan pairs -> gsp_reference_pairs.rds (what the models read).
if (STAGE_3A_REFERENCES) {
  run(nip_code("03A_reference_extraction", "01_extract_GSP_references.R"))
  run(nip_code("03A_reference_extraction", "02_aggregate_references.R"))
  run(nip_code("03A_reference_extraction", "03_query_titles_in_openalex.R"))
  run(nip_code("03A_reference_extraction", "04_search_OA_titlematch_index.R"))
  run(nip_code("03A_reference_extraction", "05_reference_set_similarity.R"))
}

# ----- Stage 3B: project-section Jaccard (the model input) -----
# Reads the metadata-only page_metadata.RDS, re-attaches page text from the core
# corpus (attach_page_text), and writes project_jaccard_results/project_section_jaccard_scores.rds
# (single file, overwritten each run) — the only 03B product the models read.
if (STAGE_3B_JACCARD) {
  need("02_text_preprocessing/page_metadata.RDS", "STAGE_2_PREPROCESS")
  run(nip_code("03B_text_reuse", "compare_project_sections.R"))
}

# ----- Stage 3C: knowledge-tree similarity (model input) -----
# Two heterogeneous steps: (1) 01_extract_knowledge_triples.R reads the core
# dependency parses (parsed_plans/*.parquet) over the sust-criteria pages and
# writes knowledge_triples_sustcrit.csv; (2) 02_semantic_kg_similarity.ipynb embeds
# those triples and scores plan-to-plan similarity -> triple_similarity.csv (what
# the models read). The R step depends on page_metadata.RDS for the sust-criteria
# page filter, so Stage 2 must have run.
if (STAGE_3C_KNOWLEDGE) {
  need("02_text_preprocessing/page_metadata.RDS", "STAGE_2_PREPROCESS")
  run(nip_code("03C_knowledge_tree", "01_extract_knowledge_triples.R"))
  run_nb(nip_code("03C_knowledge_tree", "02_semantic_kg_similarity.ipynb"))
}

cat("\n==== done.\n")
# Report each Stage 3 model input that now exists on disk.
.report <- function(label, ...) {
  p <- nip_product(...)
  if (file.exists(p)) cat(label, ":", p, "\n")
}
.report("reference pairs   (3A)", "03A_reference_extraction", "gsp_reference_pairs.rds")
.report("project-Jaccard   (3B)", "03B_text_reuse", "project_jaccard_results", "project_section_jaccard_scores.rds")
.report("triple similarity (3C)", "03C_knowledge_tree", "triple_similarity.csv")
