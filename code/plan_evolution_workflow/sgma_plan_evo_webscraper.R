#this should be a mod to the sgma_web_scraper that collects 
#each version of the plan 
#as well as the determination letters
#and figures out whether each version was approved or rejected
#based on whether there is a later version than the one in question
#(which means the version in question was rejected)
#or if not, what the current approval status states


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
   
   driver <- rsDriver(port = 4440L, browser = "firefox", chromever = NULL)
   remote_driver <- driver$client
   #remote_driver$open()
   remote_driver$navigate(gsp_url)
   Sys.sleep(5)
   
   links = NULL
   gsp_local_id = NULL
   gsp_latest_approval = NULL
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
      gsp_latest_approval <- append(gsp_latest_approval, as_vector(gsp_table[[1]]['Status']))
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
   
   determination_letters <- str_replace(links,"preview","assessments")
   
   gsp_attr <- bind_cols(link = links, basin = gsp_basins, 
                         latest_approval = gsp_latest_approval, 
                         gsp_local_id = gsp_local_id,
                         determination_letters = determination_letters)
   #add numeric id "gsp_num_id" to spreadsheet
   gsp_attr <- gsp_attr %>% 
      mutate(code = ifelse(is.na(link), NA, substr(gsp_attr$link, 21,length(gsp_attr$link))))%>% 
      mutate(num_zeros = ifelse(is.na(code),NA, 4 - str_length(code))) %>% 
      mutate(gsp_num_id = ifelse(is.na(code),NA,paste(ifelse(num_zeros > 0, "0", ""),
                                                      ifelse(num_zeros > 1,"0",""),
                                                      ifelse(num_zeros > 2, "0",""),code,sep = ""))) %>% 
      select(!c(code,num_zeros))

   
   #go to webpage
   pdf_link <- NULL
   xlsx_link <- NULL
   options(timeout=6000)
   num_gsas <- rep(NA, length(gsp_attr$basin))
   name_gsas <- vector(mode = "list", length = length(gsp_attr$basin))
   
   version_ctrls <- vector(mode = "list", length = length(gsp_attr$link))
   
   
   for(i in 1:length(gsp_attr$link)){
      #we assume only 2 versions of the plan max, because at that point the State Board takes over
      version_ctrl_tbl <- data.frame(gsp_num_id = gsp_attr$gsp_num_id[i], 
                                     version = -1,#temporary marker for the "latest version" as noted in the main page table
                                     version_approval = gsp_attr$latest_approval[i],
                                     latest_version = T) 
      
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
            
            #resubmitted section
            dropdown <- remote_driver$findElements(using = "css selector", "#incomplete-plan-content > div > dl > dd > a")
            if(length(dropdown)>0){
               for(j in 1:length(dropdown)){
                  if(grepl("Groundwater Sustainability Plan", dropdown[[j]]$getElementAttribute(attrName="text")) ==T &
                     grepl("Redline", dropdown[[j]]$getElementAttribute(attrName="text"),ignore.case = T) ==F){
                     print("Found a resubmitted plan version.")
                     pdf_link <- dropdown[[j]]$getElementAttribute("href")
                     
                     destfilepdf <- paste('./data/raw_large_files/planevolution/v2_gsp_num_id_',gsp_attr$gsp_num_id[i],'.pdf',sep= "")
                     if(!file.exists(paste('./data/raw_large_files/planevolution/v2_gsp_num_id_',gsp_attr$gsp_num_id[i],'.pdf',sep= ""))){
                        download.file(pdf_link[[1]], destfilepdf, timeout = 6000) 
                        print(paste("pdf ",i," v2 downloaded from portal"))
                        Sys.sleep(5)
                     } else{
                        print(paste("pdf ",i," v2 already downloaded"))
                     }
                  }#end of resubmitted pdf download
                  if(grepl("Elements of the Plan", dropdown[[j]]$getElementAttribute(attrName="text")) ==T ){
                     
                     xlsx_link <- dropdown[[j]]$getElementAttribute("href")
                     
                     destfilexlsx <- paste('./data/raw_large_files/planevolution/v2_gsp_num_id_',gsp_attr$gsp_num_id[i],'.xlsx',sep= "")
                     if(!file.exists(paste('./data/raw_large_files/planevolution/v2_gsp_num_id_',gsp_attr$gsp_num_id[i],'.xlsx',sep= ""))){
                        download.file(xlsx_link[[1]], destfilexlsx)
                        print(paste("spreadsheet ",i," v2 downloaded from portal"))
                        Sys.sleep(5)
                     } else {
                        print(paste("spreadsheet ",i," v2 already downloaded"))
                     }
                  }#end of resubmitted xls download   
                  
               }#end of resubmitted section
               #set version = 2 for the latest version if there is a resubmitted plan
               #and add a row where version 1 = incomplete, since the fact that they had to resubmit means the first one was rejected
               version_ctrl_tbl[version_ctrl_tbl$gsp_num_id==gsp_attr$gsp_num_id[i] & 
                                   version_ctrl_tbl$version==-1,]$version <- 2 
               version_ctrl_tbl <- rbind(version_ctrl_tbl, c(gsp_attr$gsp_num_id[i], 1, "Incomplete", F))
               
            }else{
               print("There is no resubmitted version of this plan.")
               #set version = 1 for the latest version since there is no resubmitted version
               version_ctrl_tbl[version_ctrl_tbl$gsp_num_id==gsp_attr$gsp_num_id[i] & 
                                   version_ctrl_tbl$version==-1,]$version <- 1
               
            }
            
            
            #original plan version section
            dropdown <- remote_driver$findElements(using = "css selector", "#plan-content > div > dl > dd > a")
            if(length(dropdown)>0){
               for(j in 1:length(dropdown)){
                  if(grepl("Groundwater Sustainability Plan", dropdown[[j]]$getElementAttribute(attrName="text")) ==T &
                     grepl("Redline", dropdown[[j]]$getElementAttribute(attrName="text"),ignore.case = T) ==F){
                     print("Found an original plan version.")
                     pdf_link <- dropdown[[j]]$getElementAttribute("href")
                     
                     destfilepdf <- paste('./data/raw_large_files/planevolution/v1_gsp_num_id_',gsp_attr$gsp_num_id[i],'.pdf',sep= "")
                     if(!file.exists(paste('./data/raw_large_files/planevolution/v1_gsp_num_id_',gsp_attr$gsp_num_id[i],'.pdf',sep= ""))){
                        download.file(pdf_link[[1]], destfilepdf, timeout = 6000) 
                        print(paste("pdf ",i," v1 downloaded from portal"))
                        Sys.sleep(5)
                     } else{
                        print(paste("pdf ",i," v1 already downloaded"))
                     }
                  }#end of original pdf download
                  if(grepl("Elements of the Plan", dropdown[[j]]$getElementAttribute(attrName="text")) ==T ){
                     
                     xlsx_link <- dropdown[[j]]$getElementAttribute("href")
                     
                     destfilexlsx <- paste('./data/raw_large_files/planevolution/v1_gsp_num_id_',gsp_attr$gsp_num_id[i],'.xlsx',sep= "")
                     if(!file.exists(paste('./data/raw_large_files/planevolution/v1_gsp_num_id_',gsp_attr$gsp_num_id[i],'.xlsx',sep= ""))){
                        download.file(xlsx_link[[1]], destfilexlsx)
                        print(paste("spreadsheet ",i," v1 downloaded from portal"))
                        Sys.sleep(5)
                     } else {
                        print(paste("spreadsheet ",i," v1 already downloaded"))
                     }
                  }#end of original xls download   
                  
               }#end of original plan version section
            }else{
               print("There is no original version of this plan.")
            }
            
            
            
            
            
         #end of download if statement
      
      version_ctrls[[i]] <- version_ctrl_tbl
   }
   
   remote_driver$close()
   driver[["server"]]$stop()
   rm(driver)
   
   #are there multiple gsas collaborating on this gsp? T/F Var
   mult_gsas <- sapply(num_gsas, function(x){ifelse(x > 1,T,F)})
   gsp_attr <- cbind(gsp_attr, "mult_gsas" = mult_gsas)
   gsp_attr <- as.data.table(gsp_attr)
   gsp_attr <- cbind(gsp_attr, name_gsas)
   
   saveRDS(gsp_attr, file = paste0('./data/raw_large_files/planevolution/gsp_web_vars_', format(Sys.time(), "%Y%m%d-%H:%M")))
   