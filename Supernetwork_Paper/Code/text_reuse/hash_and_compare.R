#' Text Similarity Analysis for Portal Files
#' 
#' This script processes text files to identify similarities between documents
#' using minhashing and locality-sensitive hashing techniques.
#' Memory requirement: can be pretty significant
#' 


# ----- Configuration -----
CONFIG <- list(
   # Optimized for M2 Pro - using more cores but leaving some for system processes
   cores = min(8, parallel::detectCores() - 2),
   min_text_length = 400,
   max_text_length = 1e10,
   cut_prop = 0.1,
   space_prop_multiplier = 5,
   minhash_size = 240,
   minhash_seed = 40,
   lsh_bands = 60,
   min_score = 50,
   page_delimiter = "<<PAGE_BREAK>>",
   output_dir = "Supernetwork_Paper/data_products/score_results/"
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

# Setup logging
log_file <- file(paste0("similarity_analysis_", format(Sys.time(), "%Y%m%d_%H%M"), ".log"), "w")
log_message <- function(msg, level = "INFO") {
   timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
   message <- paste0(timestamp, " [", level, "] ", msg)
   cat(message, "\n", file = log_file, append = TRUE)
   cat(message, "\n")
}

# Setup parallelization
plan(multisession, workers = CONFIG$cores)
options(future.globals.onReference = "warning")
log_message(paste("Using", CONFIG$cores, "cores for processing"))

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

read_text_file <- function(filepath) {
   tryCatch({
      text <- readr::read_file(filepath)
      pages <- strsplit(text, CONFIG$page_delimiter, fixed = TRUE)[[1]]
      return(pages)
   }, error = function(e) {
      log_message(paste("Error reading file:", filepath, "-", e$message), "ERROR")
      return(character(0))
   })
}

report_memory <- function() {
   mem_used <- gc(reset = TRUE)
   log_message(paste("Memory usage:", format(mem_used[2, 2] / 1024^2, digits = 2), "MB"))
}

# ----- Main Process -----
# File discovery and reading
log_message("Starting file discovery")
text_files <- list.files('Multipurpose_Files/portal_files/', 
                         pattern = 'txt$', full.names = TRUE)

if (length(text_files) == 0) {
   stop("No text files found in the specified directory")
}
log_message(paste("Found", length(text_files), "text files"))

# 2. Reading and processing files
log_message("Reading and processing files")
text_list <- progressr::with_progress({
   p <- progressr::progressor(steps = length(text_files))
   future_lapply(text_files, function(file) {
      result <- read_text_file(file)
      p()
      return(result)
   }, future.seed = TRUE)
})

# 3. Clean texts
log_message("Cleaning texts")
text_list2 <- future_lapply(text_list, cleanText, future.seed = TRUE)

# Added intermediate cleanup
rm(text_list)
gc()


page_info <- readRDS("Multipurpose_Files/gsp_docs_w_meta")
page_info <- page_info[,.(gsp_id,page_num,admin,basin_plan,sust_criteria,monitoring_networks,projects_mgmt_actions,is_comment,is_reference)]

# 4. Prepare text vector
log_message("Preparing text vector")
# Create data structure for document management
documents <- data.table(
   file_path = rep(text_files, sapply(text_list2, length)),
   file_name = rep(basename(text_files), sapply(text_list2, length)),
   page_num = unlist(lapply(sapply(text_list2, length), seq_len)),
   text = unlist(text_list2)
)
documents$gsp_id <- str_extract(documents$file_path,'[0-9]{4}')

documents <- merge(documents,page_info,all.x = T)
documents <- documents[!is.na(text)]
saveRDS(documents,'Supernetwork_Paper/data_products/page_metadata.RDS')
# Added intermediate cleanup
rm(text_list2)
gc()

text_vec <- documents$text
names(text_vec) <- paste0(documents$file_name, '_', documents$page_num)

report_memory()

# 5. Create corpus
log_message("Creating text reuse corpus")
minhash <- minhash_generator(n = CONFIG$minhash_size, seed = CONFIG$minhash_seed)

portal_corpus <- TextReuseCorpus(
   text = text_vec,
   tokenizer = tokenize_ngrams, n = 10,
   minhash_func = minhash, keep_tokens = TRUE,
   progress = FALSE, skip_short = TRUE
)

report_memory()

# 6. Split corpus and apply LSH - optimized chunk size
log_message("Splitting corpus and applying LSH")
split_corpus_ntiles <- dplyr::ntile(x = seq(portal_corpus), n = CONFIG$cores * 5)
split_corpus <- split(portal_corpus, split_corpus_ntiles)
rm(portal_corpus)
gc()

split_buckets <- progressr::with_progress({
   p <- progressr::progressor(steps = length(split_corpus))
   future_map(split_corpus, function(x) {
      result <- tryCatch({
         textreuse::lsh(x, bands = CONFIG$lsh_bands)
      }, error = function(e) {
         log_message(paste("Error in LSH:", e$message), "ERROR")
         NULL
      })
      p()
      return(result)
   }, .options = furrr_options(seed = TRUE))
})

# 7. Retry failed LSH operations
log_message("Processing any failed LSH operations")
retry_count <- 0
while(any(sapply(split_buckets, is.null)) && retry_count < 3) {
   retry_count <- retry_count + 1
   null_fails <- which(sapply(split_buckets, is.null))
   log_message(paste("Retrying", length(null_fails), "failed LSH operations (attempt", retry_count, ")"))
   
   split_buckets[null_fails] <- future_lapply(null_fails, function(x) {
      tryCatch({
         textreuse::lsh(split_corpus[[x]], bands = CONFIG$lsh_bands, progress = FALSE)
      }, error = function(e) {
         log_message(paste("Error in LSH retry:", e$message), "ERROR")
         NULL
      })
   }, future.seed = TRUE)
}

if(any(sapply(split_buckets, is.null))) {
   log_message(paste(sum(sapply(split_buckets, is.null)), "LSH operations permanently failed"), "WARN")
}

# 8. Combine buckets and get candidates
log_message("Combining buckets and finding candidates")
portal_buckets <- do.call(rbind, split_buckets[!sapply(split_buckets, is.null)])
portal_candidates <- lsh_candidates(buckets = portal_buckets)
log_message(paste("Found", nrow(portal_candidates), "candidate pairs"))

report_memory()

# 9. Score candidates - optimized chunk size for processing larger chunks
log_message("Scoring candidate pairs")
candidate_splits <- split(portal_candidates, 
                          ntile(1:nrow(portal_candidates), 
                                n = min(100, nrow(portal_candidates) %/% 20000)))

score_list <- progressr::with_progress({
   p <- progressr::progressor(steps = length(candidate_splits))
   future_lapply(candidate_splits, function(chunk) {
      # Extract document IDs and texts
      a_docs <- documents[match(chunk$a, paste0(file_name, '_', page_num))]
      b_docs <- documents[match(chunk$b, paste0(file_name, '_', page_num))]
      
      # Calculate alignment scores
      scores <- mapply(function(a_text, b_text) {
         tryCatch({
            textreuse::align_local(a = a_text, b = b_text)$score
         }, error = function(e) {
            log_message(paste("Error in alignment:", e$message), "ERROR")
            0
         })
      }, a_text = a_docs$text, b_text = b_docs$text)
      
      p()
      data.table(a = chunk$a, b = chunk$b, score = scores)
   }, future.seed = TRUE)
})

# 10. Combine and filter results
log_message("Combining and filtering results")
score_dt <- rbindlist(score_list)
score_dt <- score_dt[score >= CONFIG$min_score]
log_message(paste("Found", nrow(score_dt), "significant matches with score >=", CONFIG$min_score))

# 11. Save results
log_message("Saving results")
dir.create(CONFIG$output_dir, showWarnings = FALSE, recursive = TRUE)
output_file <- paste0(CONFIG$output_dir, "portal_page_scores_", 
                      format(Sys.time(), "%Y%m%d"), ".rds")
saveRDS(score_dt, output_file, compress = TRUE)

log_message(paste("Results saved to", output_file))
log_message("Process completed successfully")

close(log_file)