pages <- readRDS("data/output_large_files/gsp_docs_w_lang")
pages <- split(pages, pages$gsp_id)
keep_pages <- lapply(pages, function(i) i$is_comment==FALSE & i$is_reference==FALSE)

pdfs <- list.files(path = "data_raw/portal", pattern = "pdf", full.names = T)
export_paths <- unname(sapply(pdfs, function(k) paste0("data_cleaned/",substr(k,17,31),"_txtnet_format")))
ocr <- F
maxchar <- 10000

pdftexts <- pdf_clean(pdfs, keep_pages, ocr, maxchar, export_paths, return_to_memory=T)

names(pdftexts) <- pdfs

saveRDS(pdftexts, "data/output_large_files/cleaned_pdfs")
otherpdfs <- readRDS("data/output_large_files/cleaned_pdfs")

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
pdf89 <- "data_raw/gsp_num_id_0089_word_from_plaintext.pdf"
#saved this file in a different place so it's available, but not what is 
#currently used in the rest of the process, which is the original pdf.
#GSP 0089 is not preserved anyway in the final statistics, but this file is
#stored elsewhere so it doesn't get confused with the original version
saveRDS(pdf0089repair(pdf89),"data/output_large_files/cleaned_plainrepair_0089")


