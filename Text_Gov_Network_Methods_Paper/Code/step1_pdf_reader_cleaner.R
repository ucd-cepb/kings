filekey <- read.csv("filekey.csv")
pages <- readRDS(filekey[filekey$var_name=="gsp_docs_lang",]$filepath)
pages <- split(pages, pages$gsp_id)
keep_pages <- lapply(pages, function(i) i$is_comment==FALSE & i$is_reference==FALSE)

pdfs <- list.files(path = filekey[filekey$var_name=="portal_folder_stmpaper",]$filepath, pattern = "pdf", full.names = T)
export_paths <- unname(sapply(pdfs, function(k) paste0(filekey[filekey$var_name=="gsp_text_and_categories_stmpaper",]$filepath,
                                                       stringr::str_extract(k,'[0-9]{1,}'),"_txtnet_format")))
ocr <- F
maxchar <- 10000

pdftexts <- pdf_clean(pdfs, keep_pages, ocr, maxchar, export_paths, return_to_memory=T)

names(pdftexts) <- pdfs

saveRDS(pdftexts, filekey[filekey$var_name=="cleaned_pdfs_for_govnetpaper",]$filepath)
otherpdfs <- readRDS(filekey[filekey$var_name=="cleaned_pdfs_for_govnetpaper",]$filepath)

pdf0089repair <- function(doc){
   texts <- pdf_text(doc)
   #if too many chars, it probably is not an actual page of text but rather a map or figure
   for(i in 1:length(texts)){
      if (nchar(texts[i])> maxchar){
         texts[i] <- NA
      }
   }
   #does not remove header or footer because there are no headers/footers
   #in this pdf, and because the plaintext repair process messed up the
   #original page numbers
   return(texts)
}
pdf89 <- filekey[filekey$var_name=="gsp0089_salvage_preprocessed",]$filepath
#saved this file in a different place so it's available, but not what is 
#currently used in the rest of the process, which is the original pdf.
#GSP 0089 is not preserved anyway in the final statistics, but this file is
#stored elsewhere so it doesn't get confused with the original version
saveRDS(pdf0089repair(pdf89),filekey[filekey$var_name=="gsp0089_salvage_postprocess",]$filepath)


