#' Text Similarity Analysis for Project Management Sections
#' 
#' This script processes project management action pages to identify similarities between documents
#' using Jaccard similarity with 10-grams.
#' 
#' This version filters for pages with projects_mgmt_actions == T, keeps only the
#' original document per plan (doc_rank == 1, the legacy '^v1' selection), and
#' concatenates those pages to create one document per plan. Then compares every
#' document to every other document using Jaccard similarity.

# ----- Configuration -----
CONFIG <- list(
   # Optimized for M2 Pro - using more cores but leaving some for system processes
   cores = min(8, parallel::detectCores() - 2),
   output_dir = "Network_Innovation_Paper/data_products/project_jaccard_results/"
)

# ----- Setup -----
# Package management
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
   data.table, quanteda, tm, textclean,
   future, future.apply, furrr, textreuse, dplyr, tidyverse,
   progressr, readr
)

# Add Apple Silicon specific optimization
if (Sys.info()["sysname"] == "Darwin" && 
    grepl("arm64", R.version$platform)) {
   pacman::p_load(RcppParallel)
   RcppParallel::setThreadOptions(numThreads = CONFIG$cores)
}

# Increase memory limit for parallel operations
options(future.globals.maxSize = 1024^3 * 16)  # Allow up to 16GB for parallel operations

# Setup parallelization
plan(multisession, workers = CONFIG$cores)
options(future.globals.onReference = "warning")

# Configure progress reporting
progressr::handlers(global = TRUE)
progressr::handlers("progress")

# ----- Main Process -----
source("Network_Innovation_Paper/Code/_paths.R")
source("Network_Innovation_Paper/Code/_corpus.R")
documents <- as.data.table(readRDS(nip_product('page_metadata.RDS')))
# One document per plan: the ORIGINAL (doc_rank == 1, i.e. the legacy '^v1'
# selection). This MUST match every other modeling input so the Jaccard network
# spans the same document universe as the rest: references (03A '^v1' filter),
# knowledge triples (03C doc_rank == 1), and entity edges (select_plan_docs
# 'original'). Without it the resubmitted doc of a plan is pooled into the same
# gsp_id below, silently changing which text this network compares.
documents <- documents[doc_rank == 1,]
project_docs_valid <- documents[projects_mgmt_actions == TRUE,]
# page_metadata is metadata-only; re-attach the clean page text on demand from the
# core corpus, keyed on (gsp_doc_id, page_num). This reproduces exactly the text
# stage 02 (additional_filter_texts.R) used to bundle — every surviving page here
# already passed cleanText, so no re-filtering is needed. attach_page_text()
# preserves row order, so we setorder on the page_num identifier before pasting:
# the concatenation sequence must derive from the identifier, not incidental order.
project_docs_valid <- attach_page_text(project_docs_valid)
setorder(project_docs_valid, gsp_id, page_num)

dim(project_docs_valid)

project_docs_valid[,.N,by=.(gsp_id)][,list(mean(N),median(N))]



project_docs_combined <- project_docs_valid[, .(
   text = paste(text, collapse = " "),
   total_pages = .N,
   gsp_id = first(gsp_id)
), by = gsp_id]

# Create text vector for corpus
text_vec <- project_docs_combined$text
names(text_vec) <- project_docs_combined$gsp_id

if (length(text_vec) == 0) {
   stop("No valid documents after text cleaning")
}

# Create TextReuseCorpus with 10-grams for Jaccard similarity
project_corpus <- TextReuseCorpus(
   text = text_vec,
   tokenizer = tokenize_ngrams, 
   n = 5,  # Using 10-grams as specified
   keep_tokens = TRUE,
   progress = FALSE, 
   skip_short = TRUE
)


# Generate all pairwise combinations
gsp_id <- names(text_vec)
project_candidates <- expand.grid(a = gsp_id, b = gsp_id, stringsAsFactors = FALSE)
# Remove self-comparisons and duplicates (keep only upper triangle)
project_candidates <- project_candidates[project_candidates$a < project_candidates$b, ]


# Score all pairwise combinations using Jaccard similarity
candidate_splits <- split(project_candidates, 
                          ntile(1:nrow(project_candidates), 
                                n = min(CONFIG$cores * 2, nrow(project_candidates) %/% 100)))

score_list <- progressr::with_progress({
   p <- progressr::progressor(steps = length(candidate_splits))
   future_lapply(candidate_splits, function(chunk) {
      # Calculate Jaccard similarity scores
      scores <- mapply(function(doc_a, doc_b) {
         tryCatch({
            jaccard_similarity(project_corpus[[doc_a]], project_corpus[[doc_b]])
         }, error = function(e) {
            0
         })
      }, doc_a = chunk$a, doc_b = chunk$b)
      
      p()
      data.table(a = chunk$a, b = chunk$b, score = scores)
   }, future.seed = TRUE)
})

# Combine and filter results
score_dt <- rbindlist(score_list)

# Add reverse pairs to make matrix symmetric
reverse_pairs <- data.table(a = score_dt$b, b = score_dt$a, score = score_dt$score)
score_dt_full <- rbind(score_dt, reverse_pairs)

# Add self-comparisons with perfect scores (Jaccard = 1.0)
self_pairs <- data.table(a = gsp_id, b = gsp_id, score = 1.0)
score_dt_complete <- rbind(score_dt_full, self_pairs)

# Save results — one file, overwritten each run (no dated versions).
dir.create(CONFIG$output_dir, showWarnings = FALSE, recursive = TRUE)
output_file <- paste0(CONFIG$output_dir, "project_section_jaccard_scores.rds")
saveRDS(score_dt_complete, output_file, compress = TRUE)
