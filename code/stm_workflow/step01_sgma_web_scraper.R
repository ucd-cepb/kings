sgma_web_scraper <- function(box_sync = F, use_repaired = F){
   #takes the site id for each GSP and adds it to GSA_GSP_Basin_Coord.csv
   #using selenium because rvest cannot read the table
   
   packs <- c('stringr','xml2','polite', 'httr', 'tidyverse','robotstxt',
              'boxr','data.table','RSelenium','purrr','rvest','glue')
   need <- packs[!packs %in% installed.packages()[,'Package']]
   if(length(need)>0){install.packages(need)}
   lapply(packs, require, character.only = TRUE)
   
   gsp_url <- "https://sgma.water.ca.gov/portal/gsp/all/"
   paths_allowed(domain = gsp_url)
   #returns TRUE
   
   gsp_session <- bow(gsp_url, force = T, verbose = F)
   gsp_session
   #crawl delay 5 sec
   #15 rules for 4 bots
   
   driver <- rsDriver(port = 4441L, browser = "firefox", chromever = NULL)
   remote_driver <- driver$client
   #remote_driver$open()
   remote_driver$navigate(gsp_url)
   Sys.sleep(5)
   
   links = NULL
   gsp_local_id = NULL
   gsp_approval = NULL
   gsp_basins = NULL
   new_page = T
   
   #for each page of the table
   while(new_page){
      gsp_sel_portal <- remote_driver$getPageSource(gsp_url)
      # reads HTML page:
      gsp_html_readout <- gsp_sel_portal[[1]] %>% read_html() 
      #scrape table
      gsp_table <- gsp_html_readout %>% html_table(header = T)
      #adds this page's gsp local ids to list of all gsp local ids
      gsp_local_id <- append(gsp_local_id, as_vector(gsp_table[[1]]['GSP Local ID']))
      gsp_approval <- append(gsp_approval, as_vector(gsp_table[[1]]['Status']))
      gsp_basins <- append(gsp_basins, as_vector(gsp_table[[1]]['Basin']))
      
      #TODO fix this section to account for new website update
      #finds url links and checks basin names
      gsp_temp_class <- gsp_html_readout %>% html_elements("td")
      gsp_temp_basin <- gsp_temp_class[grepl("<strong>",gsp_temp_class) | 
                                          grepl("preview",gsp_temp_class)]
      
      #adds this page's links to list of all links
      for(i in 1:length(gsp_temp_basin)){
         if(grepl("preview", gsp_temp_basin[i])){
            links <- append(links, gsp_temp_basin[i] %>% html_elements("a") %>% 
                               html_attr("href"))
         }else{
            links <- append(links, NA)
         }
         
      }#end of for
      #if next page exists, scrape next page
      entries <- remote_driver$findElement(using = "id","gsp-tb_info")$getElementAttribute("innerHTML")
      next_button <- remote_driver$findElement(using = "id","gsp-tb_next")
      next_button$clickElement()
      Sys.sleep(5)
      if(entries[[1]][1] == remote_driver$findElement(using = "id","gsp-tb_info")$
         getElementAttribute("innerHTML")){
         new_page = F
      }else{
         new_page = T
      }
   }#end of page specific tasks   
   
   Sys.sleep(5)
   
   
   gsp_attr <- bind_cols(link = links, basin = gsp_basins, approval = gsp_approval, gsp_local_id = gsp_local_id)
   #add numeric id "gsp_num_id" to spreadsheet
   gsp_attr <- gsp_attr %>% 
      mutate(code = ifelse(is.na(link), NA, substr(gsp_attr$link, 21,length(gsp_attr$link))))%>% 
      mutate(num_zeros = ifelse(is.na(code),NA, 4 - str_length(code))) %>% 
      mutate(gsp_num_id = ifelse(is.na(code),NA,paste(ifelse(num_zeros > 0, "0", ""),
                                                      ifelse(num_zeros > 1,"0",""),
                                                      ifelse(num_zeros > 2, "0",""),code,sep = ""))) %>% 
      select(!c(code,num_zeros))
   
   if(box_sync == T){
      #set up Renviron with Box App permissions specific to your user
      box_auth()
      box_setwd(168118574337)
   }
   
   #go to webpage
   pdf_link <- NULL
   xlsx_link <- NULL
   options(timeout=600)
   num_gsas <- rep(NA, length(gsp_attr$basin))
   name_gsas <- vector(mode = "list", length = length(gsp_attr$basin))
   
   if(use_repaired == T){
      gsp_tbl <- readRDS(list.files(path = "data/output_large_files", pattern = "web_repaired", full.names = T)[
         length(list.files(path = "data/output_large_files", pattern = "web_repaired", full.names = T))])
      for(i in 1:length(gsp_attr$gsp_num_id)){
         indx <- which(gsp_tbl$gsp_id == gsp_attr$gsp_num_id[i])
         if(length(indx)==1){
            num_gsas[i] <- ifelse(length(gsp_tbl$name_gsas[[indx]])>=1,length(gsp_tbl$name_gsas[[indx]]),NA)
            name_gsas[i] <- gsp_tbl$name_gsas[indx]
         }
      }
   }
   
   for(i in 1:length(gsp_attr$link)){
      #checks whether pdf and xlsx have been downloaded
      if(!is.na(gsp_attr$link[i]) & 
         (!file.exists(paste('./data_raw/portal/gsp_num_id_',gsp_attr$gsp_num_id[i],'.pdf',sep= "")) | 
          !file.exists(paste('./data_raw/portal/gsp_num_id_',gsp_attr$gsp_num_id[i],'.xlsx',sep= "")) |
          is.na(num_gsas[i]))){
         
         remote_driver$navigate(paste("https://sgma.water.ca.gov",gsp_attr$link[i], sep = ""))
         
         if(is.na(num_gsas[i])){
            Sys.sleep(5)
            gsp_source <- remote_driver$getPageSource(
               paste("https://sgma.water.ca.gov",gsp_attr$link[i], sep = ""))
            # reads HTML page:
            plan_html_readout <- gsp_source[[1]] %>% 
               read_html() 
            #extracting text
            temp_gsas <- plan_html_readout %>% html_elements(".col-md-12") %>% html_text2()
            #isolating list of GSAs from other elements
            temp_gsas <- temp_gsas[grepl("List of GSA",temp_gsas)]
            #splits on \n to determine number of GSAs (subtracts 1 for header row)
            name_gsas[[i]] <- strsplit(temp_gsas, "\n")[[1]][2:length(
               strsplit(temp_gsas, "\n")[[1]])]
            num_gsas[i] <- length(strsplit(temp_gsas, "\n")[[1]]) - 1
         }
         
         if(!is.na(gsp_attr$link[i]) & 
            (!file.exists(paste('./data_raw/portal/gsp_num_id_',gsp_attr$gsp_num_id[i],'.pdf',sep= "")) | 
             !file.exists(paste('./data_raw/portal/gsp_num_id_',gsp_attr$gsp_num_id[i],'.xlsx',sep= "")))){
            
            #pdf_download
            # Specify URL where file is stored
            dropdown <- remote_driver$findElement(using = "class name", "panel-title")
            dropdown$clickElement()
            
            pdf_link <- remote_driver$findElement(using = "link text", "Groundwater Sustainability Plan")$getElementAttribute("href")
            # Specify destination where file should be saved
            destfilepdf <- paste('./data_raw/portal/gsp_num_id_',gsp_attr$gsp_num_id[i],'.pdf',sep= "")
            if(box_sync == T){
               box_pdfs <- as.data.frame(box_search(gsp_attr$gsp_num_id[i], 
                                       content_types = "name", type = "file", file_extensions = "pdf",
                                       ancestor_folder_ids = box_getwd()))#searches current box folder
               if(!file.exists(paste('./data_raw/portal/gsp_num_id_',gsp_attr$gsp_num_id[i],'.pdf',sep= ""))&
                  nrow(box_pdfs) == 1){
                  box_dl(file_id = box_pdfs$id, pb = T, local_dir = './data_raw/portal')
                  print(paste("pdf",i,"downloaded from box"))
               }
               if(nrow(box_pdfs)>1){
                  print(paste("more than one Box version of pdf",k))
               }
            }
            if(!file.exists(paste('./data_raw/portal/gsp_num_id_',gsp_attr$gsp_num_id[i],'.pdf',sep= ""))){
               download.file(pdf_link[[1]], destfilepdf, timeout = 600) 
               if(box_sync == T){box_ul(dir_id = box_getwd(),file = destfilepdf,pb = T)}
               print(paste("pdf",i,"downloaded from portal"))
               Sys.sleep(5)
            } else{
               print(paste("pdf",i,"already downloaded"))
            }
            #xlsx_download
            xlsx_link <- remote_driver$findElement(using = "link text", "Elements of the Plan")$getElementAttribute("href")
            destfilexlsx <- paste('./data_raw/portal/gsp_num_id_',gsp_attr$gsp_num_id[i],'.xlsx',sep= "")
            
            if(box_sync == T){
               box_xls <- as.data.frame(box_search(gsp_attr$gsp_num_id[i], 
                                                    content_types = "name", type = "file", file_extensions = "xlsx",
                                                    ancestor_folder_ids = box_getwd()))#searches current box folder
               if(!file.exists(paste('./data_raw/portal/gsp_num_id_',gsp_attr$gsp_num_id[i],'.xlsx',sep= ""))&
                  nrow(box_xls) == 1){
                  box_dl(file_id = box_xls$id, pb = T, local_dir = './data_raw/portal')
                  print(paste("spreadsheet",i,"downloaded from box"))
               }
               if(nrow(box_xls)>1){
                  print(paste("more than one Box version of spreadsheet",k))
               }
            }
            if(!file.exists(paste('./data_raw/portal/gsp_num_id_',gsp_attr$gsp_num_id[i],'.xlsx',sep= ""))){
               download.file(xlsx_link[[1]], destfilexlsx)
               if(box_sync == T){box_ul(dir_id = box_getwd(),file = destfilexlsx,pb = T)}
               print(paste("spreadsheet",i,"downloaded from portal"))
               Sys.sleep(5)
            } else {
               print(paste("spreadsheet",i,"already downloaded"))
            }
         }#end of download if statement
      }#end of navigate if statement
   }
   
   remote_driver$close()
   driver[["server"]]$stop()
   rm(driver)
   
   #are there multiple gsas collaborating on this gsp? T/F Var
   mult_gsas <- sapply(num_gsas, function(x){ifelse(x > 1,T,F)})
   gsp_attr <- cbind(gsp_attr, "mult_gsas" = mult_gsas)
   gsp_attr <- as.data.table(gsp_attr)
   gsp_attr <- cbind(gsp_attr, name_gsas)
   
   if("0150" %in% gsp_attr$gsp_num_id){#proxy for new pull, since 2023 pull
      #manual repair, because "Incomplete" refers to the first draft
      #whereas "Inadequate" refers to the second draft
      gsp_attr[gsp_num_id=="0150"]$approval <- "Incomplete"
      
      #the "review in progress" plans from 2022 have now been reviewed
      rip_ids <- gsp_tbl[approval=="Review In Progress"]$gsp_id
      rip_ids <- rip_ids[!is.na(rip_ids)]
      
      gsp_tbl$gsp_id <- sapply(1:length(gsp_tbl$gsp_id), function (z){
         ifelse(gsp_tbl$gsp_id[z] %in% rip_ids, gsp_attr[gsp_num_id==gsp_tbl$gsp_id[z]], gsp_tbl$gsp_id[z])
      })
      
      gsp_new <- gsp_attr[!(gsp_attr$gsp_num_id %in% gsp_tbl$gsp_id),]
      gsp_tbl$gsp_num_id <- gsp_tbl$gsp_id
      gsp_tbl <- subset(gsp_tbl, select = -c(gsp_id, basin_id))
      gsp_tbl$mult_gsas <- as.logical(gsp_tbl$mult_gsas)
      gsp_tbl <- gsp_tbl[,c("link","basin","approval","gsp_local_id","gsp_num_id","mult_gsas","name_gsas")]
      gsp_comb <- rbind(gsp_tbl, gsp_new)
      gsp_comb <- gsp_comb[!is.na(gsp_comb$gsp_num_id)]
      
      gsp_attr <- gsp_comb
   }
   
   saveRDS(gsp_attr, file = paste0('./data/output_large_files/gsp_web_vars_', format(Sys.time(), "%Y%m%d-%H:%M")))
   
   return(invisible())
}
