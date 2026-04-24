# OCR Processing Script - Converted from 04_ocr_if_needed.ipynb
# This script checks PDF files for machine-readable text and performs OCR when needed

# Load required libraries
library(pdftools)
library(tesseract)

# Set up directories
base_dir <- ''
savedir <- file.path('data/Multipurpose_Files/portal_files/')

# Function to check PDF readability
check_pdf_readability <- function(savedir) {
  readable_files <- c()
  ocr_needed_files <- c()
  
  # Get list of PDF files
  pdf_files <- list.files(savedir, pattern = "\\.pdf$", ignore.case = TRUE)
  
  for (filename in pdf_files) {
    filepath <- file.path(savedir, filename)
    
    tryCatch({
      # Read PDF and check first 10 pages for text
      pdf_text <- pdf_text(filepath)
      has_text <- FALSE
      
      # Check up to first 10 pages
      pages_to_check <- min(10, length(pdf_text))
      
      for (i in 1:pages_to_check) {
        if (nchar(trimws(pdf_text[i])) > 0) {
          has_text <- TRUE
          break
        }
      }
      
      if (has_text) {
        readable_files <- c(readable_files, filename)
      } else {
        ocr_needed_files <- c(ocr_needed_files, filename)
      }
      
    }, error = function(e) {
      cat("Error processing", filename, ":", e$message, "\n")
      ocr_needed_files <<- c(ocr_needed_files, filename)
    })
  }
  
  return(list(readable = readable_files, need_ocr = ocr_needed_files))
}

# Check PDFs in the input directory
cat("Checking PDFs in input directory...\n")
result <- check_pdf_readability(savedir)
machine_readable <- result$readable
need_ocr <- result$need_ocr

# Print summary
total_files <- length(machine_readable) + length(need_ocr)
cat("Total files processed:", total_files, "\n")

# Print results
cat("\nFiles with machine-readable text:\n")
for (file in machine_readable) {
  cat("-", file, "\n")
}

cat("\nFiles that need OCR:\n")
for (file in need_ocr) {
  cat("-", file, "\n")
}

# Create directory for machine-readable GSPs
machine_readable_dir <- file.path(base_dir, 'data/Multipurpose_Files/ocrd_pdfs/')
dir.create(machine_readable_dir, recursive = TRUE, showWarnings = FALSE)
cat("\nCreated directory for machine-readable GSPs:", machine_readable_dir, "\n")

# Function to check and delete corrupt files
check_and_delete_corrupt_files <- function(directory) {
  pdf_files <- list.files(directory, pattern = "\\.pdf$", ignore.case = TRUE, full.names = TRUE)
  
  for (file_path in pdf_files) {
    tryCatch({
      pdf_text(file_path)  # Attempt to read the PDF file
    }, error = function(e) {
      filename <- basename(file_path)
      cat("Corrupt file detected:", filename, "\n")
      file.remove(file_path)
      cat("Deleted:", filename, "\n")
    })
  }
}

# Check for corrupt files in the destination directory
check_and_delete_corrupt_files(machine_readable_dir)

# Function to process PDFs (copy readable ones, OCR others)
process_pdfs <- function(savedir, machine_readable_dir, machine_readable, need_ocr, clobber = FALSE) {
  pdf_files <- list.files(savedir, pattern = "\\.pdf$", ignore.case = TRUE)
  
  for (filename in pdf_files) {
    source_path <- file.path(savedir, filename)
    dest_path <- file.path(machine_readable_dir, filename)
    
    # Check if file already exists and we're not clobbering
    if (file.exists(dest_path) && !clobber) {
      next
    }
    
    if (filename %in% machine_readable) {
      # Copy machine-readable PDFs directly
      file.copy(source_path, dest_path, overwrite = clobber)
      cat("Copied", filename, "to", machine_readable_dir, "\n")
      
    } else if (filename %in% need_ocr) {
      # Perform OCR on PDFs that need it
      tryCatch({
        # Use system call to ocrmypdf (requires ocrmypdf to be installed)
        cmd <- paste("ocrmypdf --clean --clean-final --output-type pdf", 
                    shQuote(source_path), shQuote(dest_path))
        result <- system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
        
        if (result == 0) {
          cat("OCR completed for", filename, "and saved to", machine_readable_dir, "\n")
        } else {
          cat("OCR failed for", filename, "\n")
        }
        
      }, error = function(e) {
        cat("Error processing", filename, ":", e$message, "\n")
      })
    }
  }
}

# Set CLOBBER flag
CLOBBER <- FALSE  # Set to TRUE to overwrite existing files

# Process PDFs from input directory
cat("\nProcessing PDFs from input directory...\n")
process_pdfs(savedir, machine_readable_dir, machine_readable, need_ocr, CLOBBER)

cat("\nAll files have been processed and saved to the machine-readable directory.\n")