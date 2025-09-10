library(pdftools)

CLOBBER <- TRUE  # Set this to TRUE or FALSE to control overwriting behavior
# Define a character threshold for filtering pages
MAX_CHAR_THRESHOLD <- 100000  # 100k characters

files <- list.files('Multipurpose_Files/portal_files', pattern = "\\.pdf$", full.names = TRUE)

for (file_path in files) {
   tryCatch({
      text <- pdf_text(file_path)
      
      # Replace pages with too many characters with blank pages
      page_lengths <- nchar(text)
      pages_too_long <- page_lengths > MAX_CHAR_THRESHOLD
      
      # Create a copy of the text vector
      filtered_text <- text
      
      # Replace pages with too many characters with a blank page indicator
      if (any(pages_too_long)) {
         filtered_text[pages_too_long] <- "[BLANK PAGE - ORIGINAL PAGE CONTAINED NON-TEXT CONTENT]"
         cat("Replaced", sum(pages_too_long), "pages with blank pages in", basename(file_path), 
             "\n  Replaced pages:", paste(which(pages_too_long), collapse=", "), "\n")
      }
      
      txt_file_path <- file.path('Multipurpose_Files/portal_files', paste0(tools::file_path_sans_ext(basename(file_path)), '.txt'))
      
      if (CLOBBER || !file.exists(txt_file_path)) {
         page_delimiter <- "<<PAGE_BREAK>>"
         writeLines(paste(filtered_text, collapse = page_delimiter), txt_file_path)
         cat("Processed:", basename(file_path), "\n")
      } else {
         cat("Skipped (file exists):", basename(file_path), "\n")
      }
   }, error = function(e) {
      cat("Error processing", basename(file_path), ":", conditionMessage(e), "\n")
   })
}