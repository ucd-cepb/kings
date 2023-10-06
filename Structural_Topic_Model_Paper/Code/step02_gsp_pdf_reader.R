packs <- c('boxr','stm','pdftools')
need <- packs[!packs %in% installed.packages()[,'Package']]
if(length(need)>0){install.packages(need)}
lapply(packs, require, character.only = TRUE)

filekey <- read.csv("filekey.csv")

source(filekey[filekey$var_name=="page_key_script",]$filepath)


#turns each pdf into an array with each page as one line
#these are saved as RDS files
#saves a data table for each pdf with its plan element metadata as RDS files



pdfs <- list.files(path = filekey[filekey$var_name=="portal_folder_stmpaper",]$filepath, pattern = "pdf", full.names = T)
#pdf texts 1-112 successfully saved
#category texts 1-112 successfully saved
for(k in 1:length(pdfs)){
   #if pdf doesn't exist, read in and save pdf text
   if(!file.exists(paste0(filekey[filekey$var_name=="gsp_text_and_categories_stmpaper",]$filepath,stringr::str_extract(pdfs[k],'[0-9]{1,}'),"_text"))){
      
      #if still doesn't exist after box process
      if(!file.exists(paste0(filekey[filekey$var_name=="gsp_text_and_categories_stmpaper",]$filepath,stringr::str_extract(pdfs[k],'[0-9]{1,}'),"_text"))){
         text <- suppressMessages(pdf_text(pdfs[[k]]))
         #TODO check for blank pages and use ocr on them
         for(i in 1:length(text)){
            if (nchar(text[i])> 10000){
               text[i] <- NA
            }
            
         }
         #remove page numbers: 5+ spaces followed by (a combo of 1-6
         #roman and arabic numerals) or (a letter, hyphen, and
         #set of numbers such as c-28)
         text <- str_remove(text,"\\s{3,}([0-9|x|v|i]{1,6}|([a-z]+\\p{Pd}[0-9]+))\\s*$")
         saveRDS(text, file = paste0(filekey[filekey$var_name=="gsp_text_and_categories_stmpaper",]$filepath,stringr::str_extract(pdfs[k],'[0-9]{1,}'),"_text"))
         print(paste("Text",k,"generated from pdf"))
         
      }
         
   }    
   if(!file.exists(paste0(filekey[filekey$var_name=="gsp_text_and_categories_stmpaper",]$filepath,stringr::str_extract(pdfs[k],'[0-9]{1,}'),"_categories"))){
      
      #if still doesn't exist after box process
      if(!file.exists(paste0(filekey[filekey$var_name=="gsp_text_and_categories_stmpaper",]$filepath,stringr::str_extract(pdfs[k],'[0-9]{1,}'),"_categories"))){
         page_key <- create_page_key(paste0(substr(pdfs[[k]],1,nchar(pdfs[[k]])-4),".xlsx"))
         saveRDS(page_key, file = paste0(filekey[filekey$var_name=="gsp_text_and_categories_stmpaper",]$filepath,stringr::str_extract(pdfs[k],'[0-9]{1,}'),"_categories"))
         print(paste("Categories",k,"generated from spreadsheet"))
         
      }
   }
   
}#end of for

#testocr <- pdf_ocr_text(file.path("data_raw/portal/pdftools.pdf"))
#pdf_fonts() can find sections



