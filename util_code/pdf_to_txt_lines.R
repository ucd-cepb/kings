library(pdftools)

CLOBBER <- TRUE  # Set this to TRUE or FALSE to control overwriting behavior

files <- list.files('Multipurpose_Files/portal_files', pattern = "\\.pdf$", full.names = TRUE)

for (file_path in files) {
   text <- pdf_text(file_path)
   txt_file_path <- file.path('Multipurpose_Files/portal_files', paste0(tools::file_path_sans_ext(basename(file_path)), '.txt'))
   
   if (CLOBBER || !file.exists(txt_file_path)) {
      # Use a connection to ensure each page is written as a separate line
      con <- file(txt_file_path, open = "wt")
      writeLines(text, con)
      close(con)
   }
}
