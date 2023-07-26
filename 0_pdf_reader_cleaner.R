pages <- readRDS("data_output/gsp_docs_w_lang")
pages <- split(pages, pages$gsp_id)
keep_pages <- sapply(pages, function(i) i$is_comment==FALSE & i$is_reference==FALSE)

pdfs <- list.files(path = "data_raw/portal", pattern = "pdf", full.names = T)
export_paths <- unname(sapply(pdfs, function(k) paste0("data_cleaned/",substr(k,17,31),"_txtnet_format")))
ocr <- F
maxchar <- 10000

pdftexts <- pdf_clean(pdfs, keep_pages, ocr, maxchar, export_paths, return_to_memory=T)

names(pdftexts) <- pdfs

saveRDS(pdftexts, "data_output/cleaned_pdfs")



