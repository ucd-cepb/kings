#turns each pdf into an array with each page as one line
#these are saved as RDS files
#saves a data table for each pdf with its plan element metadata as RDS files

library(stm)
library(pdftools)

source('functions/create_page_key.R')

pdfs <- list.files(path = "data_raw/portal", pattern = "pdf", full.names = T)
#pdf texts 1-112 successfully saved
#category texts 1-112 successfully saved
for(k in 1:length(pdfs)){
   #if pdf doesn't exist, read in and save pdf text
   if(!file.exists(paste0("data_output/",substr(pdfs[k],10,24),"_text"))){
      text <- pdf_text(pdfs[[k]])
      print(k)
      #TODO check for blank pages and use ocr on them
      for(i in 1:length(text)){
         if (nchar(text[i])> 10000){
            text[i] <- NA
         }
      }
      saveRDS(text, file = paste0("data_output/",substr(pdfs[k],10,24),"_text"))
      #load gsp_xls_cleaner functions
      page_key <- create_page_key(paste0(substr(pdfs[[k]],1,24),".xlsx"))
      saveRDS(page_key, file = paste0("data_output/",substr(pdfs[k],10,24),"_categories"))
      
   }
      
}


#testocr <- pdf_ocr_text(file.path("data_raw/portal/pdftools.pdf"))

#TODO pdf_fonts to find sections

