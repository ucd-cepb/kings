#' Text Preprocessing for Portal Files
#' 
#' This script handles common preprocessing tasks for portal text files,
#' creating a standardized page_metadata.RDS file used by downstream analyses.
#' 

# ----- Configuration -----
CONFIG <- list(
  cores = min(8, parallel::detectCores() - 2),
  min_text_length = 400,
  max_text_length = 1e10,
  cut_prop = 0.1,
  space_prop_multiplier = 5,
  page_delimiter = "<<PAGE_BREAK>>",
  output_file = "Network_Innovation_Paper/data_products/page_metadata.RDS"
)

# ----- Setup -----
# Package management
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  data.table, stringr, future, future.apply, readr
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
     cat(paste("Error reading file:", filepath, "-", e$message, "\n"))
     return(character(0))
  })
}

# ----- Main Process -----
main <- function(text_files = NULL) {
  
  # File discovery if not provided
  if (is.null(text_files)) {
    text_files <- list.files('Multipurpose_Files/portal_files/', 
                            pattern = 'txt$', full.names = TRUE)
  }
  
  if (length(text_files) == 0) {
    stop("No text files found")
  }
  cat(paste("Processing", length(text_files), "text files\n"))
  
  # Reading files
  # Pre-allocate list for better performance
  text_list <- vector("list", length(text_files))
  for (i in seq_along(text_files)) {
    text_list[[i]] <- read_text_file(text_files[i])
    if (i %% 10 == 0) cat(paste("Read", i, "of", length(text_files), "files\n"))
  }
  
  # Clean texts in parallel
  cat("Cleaning texts...\n")
  text_list2 <- future_lapply(text_list, cleanText, future.seed = TRUE)
  
  # Cleanup
  rm(text_list)
  gc()
  
  # Load metadata
  page_info <- readRDS("Multipurpose_Files/gsp_docs_w_meta")
  page_info <- page_info[,.(gsp_id, page_num, admin, basin_plan, sust_criteria, 
                            monitoring_networks, projects_mgmt_actions, is_comment, is_reference)]
  
  # Prepare document structure
  cat("Creating document structure...\n")
  documents <- data.table(
    file_path = rep(text_files, sapply(text_list2, length)),
    file_name = rep(basename(text_files), sapply(text_list2, length)),
    page_num = unlist(lapply(sapply(text_list2, length), seq_len)),
    text = unlist(text_list2)
  )
  documents$gsp_id <- str_extract(documents$file_path,'[0-9]{4}')
  
  # Merge with metadata
  documents <- merge(documents, page_info, all.x = TRUE)
  documents <- documents[!is.na(text)]
  
  # Save
  dir.create(dirname(CONFIG$output_file), showWarnings = FALSE, recursive = TRUE)
  saveRDS(documents, CONFIG$output_file)
  cat(paste("Saved preprocessed data to", CONFIG$output_file, "\n"))
  
  # Return documents for immediate use
  return(documents)
}

# Run if executed directly
if (sys.nframe() == 0) {
  documents <- main()
  cat("Preprocessing complete.\n")
}