#takes the site id for each GSP and adds it to GSA_GSP_Basin_Coord.csv
#using selenium because rvest cannot read the table
library(stringr)
library("xml2")
library(polite)
library(httr)
library(tidyverse)
library(robotstxt)
pacman::p_load(RSelenium, purrr, rvest, glue)

gsp_url <- "https://sgma.water.ca.gov/portal/gsp/all/"
paths_allowed(domain = gsp_url)
#returns TRUE

gsp_session <- bow(gsp_url, force = T)
gsp_session
#crawl delay 5 sec
#15 rules for 4 bots

driver <- rsDriver(port = 4442L, browser = "firefox")
remote_driver <- driver$client
#remote_driver$open()
remote_driver$navigate(gsp_url)
Sys.sleep(5)

#Website update breaks this section. Not necessary to run.
#find num entries button; changes to 100 entries per page
#num_entries_tmp <-remote_driver$findElement(using = "name", "gsp-tb_length")
#move pointer to dropdown
#remote_driver$mouseMoveToLocation(webElement = num_entries_tmp)
#remote_driver$click(buttonId = 'LEFT')
#Sys.sleep(5)
#TODO fix this to accomodate website's new update
#num_entries_btn <- remote_driver$findElement(using = "css selector","option[value=\"100\"]")
#move pointer to button
#remote_driver$mouseMoveToLocation(webElement = num_entries_btn)
#click on num entries button
#num_entries_btn$clickElement()
#Sys.sleep(5)

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

#go to webpage
pdf_link <- NULL
xlsx_link <- NULL
options(timeout=600)
#checks whether pdf has been downloaded
for(i in 1:length(gsp_attr$link)){
   #checks whether pdf and xlsx have been downloaded
   if(!is.na(gsp_attr$link[i]) & 
      (!file.exists(paste('./data_raw/gsp_num_id_',gsp_attr$gsp_num_id[i],'.pdf',sep= "")) | 
       !file.exists(paste('./data_raw/gsp_num_id_',gsp_attr$gsp_num_id[i],'.xlsx',sep= "")))){
      remote_driver$navigate(paste("https://sgma.water.ca.gov",gsp_attr$link[i], sep = ""))
      #pdf_download
      # Specify URL where file is stored
      dropdown <- remote_driver$findElement(using = "class name", "panel-title")
      dropdown$clickElement()
      
      pdf_link <- remote_driver$findElement(using = "link text", "Groundwater Sustainability Plan")$getElementAttribute("href")
      # Specify destination where file should be saved
      destfilepdf <- paste('./data_raw/gsp_num_id_',gsp_attr$gsp_num_id[i],'.pdf',sep= "")
      if(!file.exists(paste('./data_raw/gsp_num_id_',gsp_attr$gsp_num_id[i],'.pdf',sep= ""))){
         download.file(pdf_link[[1]], destfilepdf, timeout = 600)  
         print(paste("pdf",i,"downloaded"))
         Sys.sleep(5)
      } else{
         print(paste("pdf",i,"already downloaded"))
      }
      #xlsx_download
      xlsx_link <- remote_driver$findElement(using = "link text", "Elements of the Plan")$getElementAttribute("href")
      destfilexlsx <- paste('./data_raw/gsp_num_id_',gsp_attr$gsp_num_id[i],'.xlsx',sep= "")
      if(!file.exists(paste('./data_raw/gsp_num_id_',gsp_attr$gsp_num_id[i],'.xlsx',sep= ""))){
         download.file(xlsx_link[[1]], destfilexlsx)
         print(paste("spreadsheet",i,"downloaded"))
         Sys.sleep(5)
      } else {
         print(paste("spreadsheet",i,"already downloaded"))
      }
   }
}

remote_driver$close()
rm(driver)

write_csv(x =gsp_attr, file = './data_output/gsp_ids.csv')
