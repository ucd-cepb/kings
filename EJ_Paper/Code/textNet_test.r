library(textNet)
pdfs <- c(list.files(path = ".", pattern = "old.pdf", full.names = T),
          list.files(path = ".", pattern = "new.pdf", full.names = T))
ocr <- F
maxchar <- 10000
old_new_text <- textNet::pdf_clean(pdfs, keep_pages=T, ocr=F, maxchar=10000,
                                   export_paths=NULL, return_to_memory=T, suppressWarn = F)
names(old_new_text) <- c("old","new")