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

driver <- rsDriver(port = 4445L, browser = "firefox")
remote_driver <- driver$client
#remote_driver$open()
remote_driver$navigate(gsp_url)
Sys.sleep(5)

#find num entries button; changes to 100 entries per page
num_entries_tmp <-remote_driver$findElement(using = "name", "gsp-tb_length")
#move pointer to dropdown
remote_driver$mouseMoveToLocation(webElement = num_entries_tmp)
remote_driver$click(buttonId = 'LEFT')
Sys.sleep(5)
num_entries_btn <- remote_driver$findElement(using = "css selector","option[value='100']")
#move pointer to button
remote_driver$mouseMoveToLocation(webElement = num_entries_btn)
#click on num entries button
num_entries_btn$clickElement()
Sys.sleep(5)

links = NULL
basin_names = NULL
gsp_local_id = NULL
new_page = T

#for each page
while(new_page){
   gsp_sel_portal <- remote_driver$getPageSource(gsp_url)
   # reads HTML page:
   gsp_html_readout <- gsp_sel_portal[[1]] %>% read_html() 
   #scrape table
   gsp_table <- gsp_html_readout %>% html_table(header = T)
   #adds this page's gsp local ids to list of all gsp local ids
   gsp_local_id <- append(gsp_local_id, as_vector(gsp_table[[1]]['GSP Local ID']))
   #finds the name of each type of basin class, which is where the url links are found
   gsp_td_names <- gsp_html_readout %>% html_elements("td") %>% html_attr("class")
   gsp_td_unique <- unique(gsp_td_names[!is.na(gsp_td_names)])
   
   gsp_basin_href <- NULL
   for(index in 1:length(gsp_td_unique)){
      gsp_basin_href[[index]] <-append(gsp_basin_href, gsp_html_readout %>% html_elements(paste(".",gsp_td_unique[index], sep = "")))
   }
   #adds this page's basin names and links to list of all basin names and links
   for(i in 1:length(gsp_basin_href)){
      for(j in 1:length(gsp_basin_href[[i]])){
         try_link = gsp_basin_href[[i]][[j]] %>% html_elements("a") %>% html_attr("href")
         if(length(try_link)>0){
            links <- append(links, gsp_basin_href[[i]][[j]] %>% html_elements("a") %>% html_attr("href"))
            basin_names <- append(basin_names, gsp_basin_href[[i]][[j]] %>% html_elements("a") %>% html_text())
         }else{
            links <- append(links, NA)
            basin_names <- append(basin_names, gsp_basin_href[[i]][[j]] %>% html_elements("strong") %>% html_text())
         }#end of else
      }#end of inner for
   }#end of outer for
   #if next page exists, scrape next page
   entries <- remote_driver$findElement(using = "id","gsp-tb_info")$getElementAttribute("innerHTML")
   next_button <- remote_driver$findElement(using = "id","gsp-tb_next")
   next_button$clickElement()
   Sys.sleep(5)
   if(entries[[1]][1] == remote_driver$findElement(using = "id","gsp-tb_info")$getElementAttribute("innerHTML")){
      new_page = F
   }else{
      new_page = T
   }
}#end of page specific tasks   

Sys.sleep(5)
   
   
links_and_basin_names <- bind_cols(link = links, basin = basin_names, gsp_local_id = gsp_local_id)
#add numeric id "gsp_num_id" to spreadsheet
links_and_basin_names <- links_and_basin_names %>% 
   mutate(code = (substr(links_and_basin_names$link, 21,length(links_and_basin_names$link))))%>% 
   mutate(num_zeros = 4 - str_length(code)) %>% 
   mutate(gsp_num_id = paste(ifelse(num_zeros > 0, "0", ""),ifelse(num_zeros > 1,"0",""),ifelse(num_zeros > 2, "0",""),code,sep = "")) %>% 
   select(!c(code,num_zeros))

#go to webpage
pdf_link <- NULL
xlsx_link <- NULL
options(timeout=600)
#1-end successfully downloaded
for(i in 1:length(links_and_basin_names$link)){
   if(!is.na(links_and_basin_names$link[i])){
      remote_driver$navigate(paste("https://sgma.water.ca.gov",links_and_basin_names$link[i], sep = ""))
      #pdf_download
      # Specify URL where file is stored
      pdf_link <- remote_driver$findElement(using = "link text", "Groundwater Sustainability Plan")$getElementAttribute("href")
      # Specify destination where file should be saved
      destfilepdf <- paste('./data_raw/gsp_num_id_',links_and_basin_names$gsp_num_id[i],'.pdf',sep= "")
      download.file(pdf_link[[1]], destfilepdf, timeout = 600)
      Sys.sleep(5)
      print(paste("pdf",i,"downloaded"))
      #xlsx_download
      xlsx_link <- remote_driver$findElement(using = "link text", "Elements of the Plan")$getElementAttribute("href")
      destfilexlsx <- paste('./data_raw/gsp_num_id_',links_and_basin_names$gsp_num_id[i],'.xlsx',sep= "")
      download.file(xlsx_link[[1]], destfilexlsx)
      print(paste("spreadsheet",i,"downloaded"))
      Sys.sleep(5)
   }
}

remote_driver$close()
rm(driver)

write_csv(x =links_and_basin_names, file = './data_output/gsp_ids.csv')
