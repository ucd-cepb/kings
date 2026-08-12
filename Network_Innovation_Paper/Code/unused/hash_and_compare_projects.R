#' Text Similarity Analysis for Project Management Actions
#' 
#' This script processes project management action pages to identify similarities between documents
#' using direct pairwise comparison (no LSH).
#' 
#' This version filters for pages with projects_management_actions == T,
#' removes page breaks, and concatenates pages by file to create one document per file.
#' Then compares every document to every other document.

# ----- Configuration -----
CONFIG <- list(
   # Optimized for M2 Pro - using more cores but leaving some for system processes
   cores = min(8, parallel::detectCores() - 2),
   min_text_length = 400,
   max_text_length = 1e10,
   cut_prop = 0.1,
   space_prop_multiplier = 5,
   min_score = 10,
   output_dir = "Network_Innovation_Paper/data_products/project_lda_results/"
)

# ----- Setup -----
# Package management
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
   data.table, quanteda, tm, textclean, stringr,
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

# ----- Functions -----
cleanText <- function(text, cut_prop = CONFIG$cut_prop, 
                      space_prop_multiplier = CONFIG$space_prop_multiplier) {
   text = gsub('\"\"', '', text, fixed = TRUE)
   chars = nchar(text)
   periods = stringr::str_count(text, "\\.")
   numbers = stringr::str_count(text, "[0-9]")
   caps = stringr::str_count(text, '[A-Z]')
   tildes = stringr::str_count(text, '~')
   quotes = stringr::str_count(text, '\\"')
   spaces = stringr::str_count(text, '\\s')
   valid_indices = chars > CONFIG$min_text_length & 
      chars <= CONFIG$max_text_length & 
      (periods/chars) < cut_prop & 
      (quotes/chars) < cut_prop & 
      (tildes/chars) < cut_prop & 
      (numbers/chars) < cut_prop & 
      (caps/chars) < cut_prop & 
      (spaces/chars) < (cut_prop * space_prop_multiplier)
   text[!valid_indices] <- NA
   return(text)
}

documents <- readRDS('Network_Innovation_Paper/data_products/page_metadata.RDS')
project_docs <- documents[projects_mgmt_actions == TRUE,]

# Clean text on per-page basis before concatenating
project_docs$text_cleaned <- cleanText(project_docs$text)
project_docs_valid <- project_docs[!is.na(text_cleaned),]

project_docs_combined <- project_docs_valid[, .(
   text = paste(text_cleaned, collapse = " "),
   total_pages = .N,
   gsp_id = first(gsp_id)
), by = file_name]

# Create text vector for corpus
text_vec <- project_docs_combined$text
names(text_vec) <- project_docs_combined$file_name

if (length(text_vec) == 0) {
   stop("No valid documents after text cleaning")
}

# Generate all pairwise combinations
file_names <- names(text_vec)
project_candidates <- expand.grid(a = file_names, b = file_names, stringsAsFactors = FALSE)
# Remove self-comparisons and duplicates (keep only upper triangle)
project_candidates <- project_candidates[project_candidates$a < project_candidates$b, ]
# Score all pairwise combinations - optimized chunk size
candidate_splits <- split(project_candidates, 
                          ntile(1:nrow(project_candidates), 
                                n = min(CONFIG$cores * 2, nrow(project_candidates) %/% 100)))

score_list <- progressr::with_progress({
   p <- progressr::progressor(steps = length(candidate_splits))
   future_lapply(candidate_splits, function(chunk) {
      # Extract document texts by name
      a_texts <- text_vec[chunk$a]
      b_texts <- text_vec[chunk$b]
      
      # Calculate alignment scores
      scores <- mapply(function(a_text, b_text) {
         tryCatch({
            textreuse::align_local(a = a_text, b = b_text)$score
         }, error = function(e) {
            0
         })
      }, a_text = a_texts, b_text = b_texts)
      
      p()
      data.table(a = chunk$a, b = chunk$b, score = scores)
   }, future.seed = TRUE)
})

# Combine and filter results
score_dt <- rbindlist(score_list)
score_dt <- score_dt[score >= CONFIG$min_score]

# Add reverse pairs to make matrix symmetric
reverse_pairs <- data.table(a = score_dt$b, b = score_dt$a, score = score_dt$score)
score_dt_full <- rbind(score_dt, reverse_pairs)

# Add self-comparisons with perfect scores
self_pairs <- data.table(a = file_names, b = file_names, score = 100)
score_dt_complete <- rbind(score_dt_full, self_pairs)

# Save results
dir.create(CONFIG$output_dir, showWarnings = FALSE, recursive = TRUE)
output_file <- paste0(CONFIG$output_dir, "project_page_scores_", 
                      format(Sys.time(), "%Y%m%d"), ".rds")
saveRDS(score_dt_complete, output_file, compress = TRUE)