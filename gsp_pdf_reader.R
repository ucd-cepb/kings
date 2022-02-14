#turns each pdf into a .txt file with each page as one line
#identifies the location of each section header
#creates a folder of .txt files, where each file contains the text under each section

library(pdftools)

pdfs <- list.files(path = "data_raw", pattern = "pdf", full.names = T)
#pdf texts 1-51 successfully saved
#category texts 1-51 successfully saved
for(k in 1:length(pdfs)){
   text <- pdf_text(pdfs[[k]])
   print(k)
   for(i in 1:length(text)){
      if (nchar(text[i])> 10000){
         text[i] <- NA
      }
   }
   saveRDS(text, file = paste0("data_output/",substr(pdfs[k],10,24),"_text"))
   page_key <- create_page_key(paste0(substr(pdfs[[k]],1,24),".xlsx"))
   saveRDS(page_key, file = paste0("data_output/",substr(pdfs[k],10,24),"_categories"))
   
}


#to retrieve:
#example
gsp_num_id <- "0008"

text_of_interest <- readRDS(paste0("data_output/gsp_num_id_",gsp_num_id,"_text"))
key_of_interest <- readRDS(paste0("data_output/gsp_num_id_",gsp_num_id,"_categories"))

#TODO decide input text you want to have matched then go to page_vector of that element
#words[key$page_vector[matched]]


#testocr <- pdf_ocr_text(file.path("data_raw/pdftools.pdf"))

#TODO pdf_fonts to find sections

