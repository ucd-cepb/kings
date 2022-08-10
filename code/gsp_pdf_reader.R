packs <- c('boxr','stm','pdftools')
need <- packs[!packs %in% installed.packages()[,'Package']]
if(length(need)>0){install.packages(need)}
lapply(packs, require, character.only = TRUE)

source('code/functions/create_page_key.R')

gsp_pdf_reader<- function(box_sync = F){
   #turns each pdf into an array with each page as one line
   #these are saved as RDS files
   #saves a data table for each pdf with its plan element metadata as RDS files

   if(box_sync == T){
      #set up Renviron with Box App permissions specific to your user
      box_auth()
      box_setwd(168119254229)
   }
   
   pdfs <- list.files(path = "data_raw/portal", pattern = "pdf", full.names = T)
   #pdf texts 1-112 successfully saved
   #category texts 1-112 successfully saved
   for(k in 1:length(pdfs)){
      #if pdf doesn't exist, read in and save pdf text
      if(!file.exists(paste0("data_cleaned/",substr(pdfs[k],17,31),"_text"))){
         if(box_sync == T){
            box_texts <- as.data.frame(box_search(paste0(substr(pdfs[k],28,31)," AND text"), 
                                                 content_types = "name", type = "file",
                                                 ancestor_folder_ids = box_getwd()))#searches current box folder
            if(nrow(box_texts) == 1){
               box_dl(file_id = box_texts$id, pb = T, local_dir = './data_cleaned')
               print(paste("text",k,"downloaded from box"))
            }
            if(nrow(box_texts)>1){
               print(paste("more than one Box version of text",k))
            }
         }
         #if still doesn't exist after box process
         if(!file.exists(paste0("data_cleaned/",substr(pdfs[k],17,31),"_text"))){
            text <- pdf_text(pdfs[[k]])
            #TODO check for blank pages and use ocr on them
            for(i in 1:length(text)){
               if (nchar(text[i])> 10000){
                  text[i] <- NA
                  #TODO check for page numbers and repeated header info and remove them
                  #look for >5 spaces /s/s/s/s+  near end $
                  #if it contains a number or x, v, or i and is < 10 words 
                  # [0-9 | x | v | i] & num words(captured group <10)
                  #look at it and then remove
               }
               #remove page numbers: 5+ spaces followed by (a combo of 1-6
               #roman and arabic numerals) or (a letter, hyphen, and
               #set of numbers such as c-28)
               
            }
            text <- str_remove(text,"\\s{3,}([0-9|x|v|i]{1,6}|([a-z]+\\p{Pd}[0-9]+))\\s*$")
            saveRDS(text, file = paste0("data_cleaned/",substr(pdfs[k],17,31),"_text"))
            print(paste("Text",k,"generated from pdf"))
            if(box_sync == T){
               box_ul(dir_id = box_getwd(),
                              paste0("data_cleaned/",substr(pdfs[k],17,31),"_text"),pb = T)
            }
         }
            
      }    
      if(!file.exists(paste0("data_cleaned/",substr(pdfs[k],17,31),"_categories"))){
         if(box_sync == T){
            box_cats <- as.data.frame(box_search(paste0(substr(pdfs[k],28,31)," AND categories"), 
                                                 content_types = "name", type = "file",
                                                 ancestor_folder_ids = box_getwd()))#searches current box folder
            if(nrow(box_cats) == 1){
               box_dl(file_id = box_cats$id, pb = T, local_dir = './data_cleaned')
               print(paste("categories",k,"downloaded from box"))
            }
            if(nrow(box_cats)>1){
               print("more than one Box version of categories",k)
            }
         }
         #if still doesn't exist after box process
         if(!file.exists(paste0("data_cleaned/",substr(pdfs[k],17,31),"_categories"))){
            page_key <- create_page_key(paste0(substr(pdfs[[k]],1,31),".xlsx"))
            saveRDS(page_key, file = paste0("data_cleaned/",substr(pdfs[k],17,31),"_categories"))
            print(paste("Categories",k,"generated from spreadsheet"))
            if(box_sync == T){
               box_ul(dir_id = box_getwd(),
                      paste0("data_cleaned/",substr(pdfs[k],17,31),"_categories"),pb = T)
            }
         }
      }
      
   }#end of for
   
   return(invisible())
   #testocr <- pdf_ocr_text(file.path("data_raw/portal/pdftools.pdf"))
   #pdf_fonts() can find sections
}


