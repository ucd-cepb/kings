#' Text Dissimilarity Analysis for Portal Sections
#' 
#' This script processes text files to calculate dissimilarity between document sections
#' using Jaccard dissimilarity. Sections are defined by indicators in the metadata.
#' 

# ----- Configuration -----
CONFIG <- list(
  cores = min(8, parallel::detectCores() - 2),
  min_text_length = 400,
  max_text_length = 1e10,
  cut_prop = 0.1,
  space_prop_multiplier = 5,
  page_delimiter = "<<PAGE_BREAK>>",
  output_dir = "Network_Innovation_Paper/data_products/score_results/",
  section_types = c("admin", "basin_plan", "sust_criteria", "monitoring_networks", "projects_mgmt_actions")
)

# ----- Setup -----
# Package management
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  data.table, quanteda, tm, textclean, stringr,
  future, future.apply, furrr, textreuse, dplyr, tidyverse,
  readr
)

# Add Apple Silicon specific optimization
if (Sys.info()["sysname"] == "Darwin" && 
    grepl("arm64", R.version$platform)) {
  pacman::p_load(RcppParallel)
  RcppParallel::setThreadOptions(numThreads = CONFIG$cores)
}

# Increase memory limit for parallel operations
options(future.globals.maxSize = 1024^3 * 16)


# Setup parallelization
plan(multisession, workers = CONFIG$cores)
options(future.globals.onReference = "warning")
cat(paste("Using", CONFIG$cores, "cores for processing\n"))


# ----- Functions -----


create_section_chunks <- function(documents, section_types) {
  # Create a list to store section chunks
  section_chunks <- list()
  
  # For each GSP document
  for (gsp in unique(documents$gsp_id)) {
    gsp_docs <- documents[gsp_id == gsp]
    
    # For each section type
    for (section in section_types) {
      # Get pages that belong to this section
      section_pages <- gsp_docs[get(section) == TRUE]
      
      if (nrow(section_pages) > 0) {
        # Concatenate text from all pages in this section
        section_text <- paste(section_pages$text[!is.na(section_pages$text)], collapse = " ")
        
        # Store section information
        if (nchar(section_text) > 0) {
          section_id <- paste0(gsp, "_", section)
          section_chunks[[section_id]] <- list(
            gsp_id = gsp,
            section = section,
            text = section_text,
            page_count = nrow(section_pages)
          )
        }
      }
    }
  }
  
  return(section_chunks)
}

# ----- Main Process -----
# Load preprocessed data or create it if needed
if (file.exists('Network_Innovation_Paper/data_products/page_metadata.RDS')) {
  cat("Loading preprocessed page metadata...\n")
  documents <- readRDS('Network_Innovation_Paper/data_products/page_metadata.RDS')
} else {
  cat("Preprocessed data not found. Running preprocessing...\n")
  source('Network_Innovation_Paper/Code/02_text_preprocessing/additional_filter_texts.R')
  documents <- main()
}

# Create section chunks
section_chunks <- create_section_chunks(documents, CONFIG$section_types)
cat(paste("Created", length(section_chunks), "section chunks\n"))

# Convert to named vector for textreuse
section_texts <- sapply(section_chunks, function(x) x$text)
names(section_texts) <- names(section_chunks)

# Create TextReuseCorpus for Jaccard calculation with stopword removal
# First create a custom tokenizer that removes stopwords
tokenize_ngrams_no_stop <- function(string, n = 5) {
  # Tokenize into words first
  words <- tokenize_words(string, lowercase = TRUE)[[1]]
  
  # Remove stopwords (using quanteda's English stopwords)
  words <- words[!words %in% stopwords("english")]
  
  # Create n-grams from the filtered words
  if (length(words) < n) return(character(0))
  
  ngrams <- character(length(words) - n + 1)
  for (i in seq_len(length(words) - n + 1)) {
    ngrams[i] <- paste(words[i:(i + n - 1)], collapse = " ")
  }
  return(ngrams)
}

section_corpus <- TextReuseCorpus(
  text = section_texts,
  tokenizer = tokenize_ngrams_no_stop, n = 5,  # Using 5-grams with stopword removal
  progress = FALSE,
  keep_tokens = TRUE
)

# Calculate Jaccard dissimilarity between all pairs

# Get all pairwise combinations
section_names <- names(section_corpus)
pairs <- expand.grid(doc1 = section_names, doc2 = section_names, stringsAsFactors = FALSE)
pairs <- pairs[pairs$doc1 < pairs$doc2, ]  # Remove duplicates and self-comparisons

# Calculate dissimilarities in parallel
cat(paste("Calculating dissimilarity for", nrow(pairs), "section pairs\n"))

dissimilarity_results <- future_map_dfr(1:nrow(pairs), function(i) {
  pair <- pairs[i, ]
  
  # Calculate Jaccard dissimilarity
  dissim <- tryCatch({
    1 - jaccard_similarity(section_corpus[[pair$doc1]], section_corpus[[pair$doc2]])
  }, error = function(e) {
    cat(paste("Error calculating dissimilarity for", pair$doc1, "and", pair$doc2, ":", e$message, "\n"))
    NA
  })
  
  # Extract metadata from section names
  doc1_parts <- strsplit(pair$doc1, "_")[[1]]
  doc2_parts <- strsplit(pair$doc2, "_")[[1]]
  
  data.frame(
    gsp_id_1 = doc1_parts[1],
    section_1 = doc1_parts[2],
    gsp_id_2 = doc2_parts[1],
    section_2 = doc2_parts[2],
    jaccard_dissimilarity = dissim,
    same_gsp = doc1_parts[1] == doc2_parts[1],
    same_section_type = doc1_parts[2] == doc2_parts[2],
    stringsAsFactors = FALSE
  )
}, .options = furrr_options(seed = TRUE))

# Add self-comparisons (dissimilarity = 0)
self_comparisons <- data.frame(
  gsp_id_1 = sapply(strsplit(section_names, "_"), `[`, 1),
  section_1 = sapply(strsplit(section_names, "_"), `[`, 2),
  gsp_id_2 = sapply(strsplit(section_names, "_"), `[`, 1),
  section_2 = sapply(strsplit(section_names, "_"), `[`, 2),
  jaccard_dissimilarity = 0,
  same_gsp = TRUE,
  same_section_type = TRUE,
  stringsAsFactors = FALSE
)

# Combine all results
all_dissimilarities <- rbind(dissimilarity_results, self_comparisons)

# Add section metadata
section_info <- data.frame(
  section_id = names(section_chunks),
  page_count = sapply(section_chunks, function(x) x$page_count),
  text_length = sapply(section_chunks, function(x) nchar(x$text)),
  stringsAsFactors = FALSE
)

# Save results
dir.create(CONFIG$output_dir, showWarnings = FALSE, recursive = TRUE)

# Save dissimilarity matrix
output_file <- paste0(CONFIG$output_dir, "section_jaccard_dissimilarity_", 
                     format(Sys.time(), "%Y%m%d"), ".rds")
saveRDS(all_dissimilarities, output_file, compress = TRUE)

cat(paste("Results saved to", output_file, "\n"))

