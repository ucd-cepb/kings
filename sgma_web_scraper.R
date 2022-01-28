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

#requires rtools or xquartz
#file_path <- getwd() %>% str_replace_all("/", "\\\\\\\\")


pdf_xls_prof <- makeFirefoxProfile(list(
   "pdfjs.disabled"=TRUE,
   "brower.download.folderList" = 2L,
   "browser.helperApps.alwaysAsk.force" = F,
   "browser.download.manager.showWhenStarting" = F,
   "browser.download.manager.showAlertOnComplete" = F,
   "plugin.scan.plid.all"=FALSE,
   "plugin.scan.Acrobat" = "99.0",
   #"browser.helperApps.neverAsk.saveToDisk" = c('application/pdf','application/vnd.openxmlformats-officedocument.spreadsheetml.sheet','application/excel','application/x-excel'),
   "browser.helperApps.neverAsk.saveToDisk" = 'application/pdf',
   "browser.download.dir" = './data_raw'))
driver <- rsDriver(port = 4445L, browser = "firefox", extraCapabilities = pdf_xls_prof)
#driver <- remoteDriver$new(port = 4444L, browser = "firefox", extraCapabilities = pdf_xls_prof)
remote_driver <- driver$client
#remote_driver$open()
remote_driver$navigate(gsp_url)
Sys.sleep(5)

#find num entries button; changes to 100 entries per page
#TODO: switch methods to using button at bottom of page
num_entries_tmp <-remote_driver$findElement(using = "name", "gsp-tb_length")
#move pointer to dropdown
remote_driver$mouseMoveToLocation(webElement = num_entries_tmp)
remote_driver$click(buttonId = 'LEFT')
num_entries_btn <- remote_driver$findElement(using = "css selector","option[value='100']")
#move pointer to button
remote_driver$mouseMoveToLocation(webElement = num_entries_btn)
num_entries_btn$highlightElement()
#click on num entries button
num_entries_btn$clickElement()
Sys.sleep(5)


gsp_sel_portal <- remote_driver$getPageSource(gsp_url)

# reads HTML page:
gsp_html_readout <- gsp_sel_portal[[1]] %>% read_html() 
#scrape table
gsp_table <- gsp_html_readout %>% html_table(header = T)


gsp_td_names <- gsp_html_readout %>% html_elements("td") %>% html_attr("class")
gsp_td_unique <- unique(gsp_td_names[!is.na(gsp_td_names)])
gsp_basin_href <- NULL
for(index in 1:length(gsp_td_unique)){
   gsp_basin_href[[index]] <-append(gsp_basin_href, gsp_html_readout %>% html_elements(paste(".",gsp_td_unique[index], sep = "")))
}

links = NULL
basin_names = NULL
linkless_names = NULL
#works for <=100 entries. If >100 entries, must cycle through each page using a class = "paginate_button next" while it exists
for(i in 1:length(gsp_basin_href)){
   for(j in 1:length(gsp_basin_href[[i]])){
      linkless_names <- append(linkless_names, gsp_basin_href[[i]][[j]] %>% html_elements("strong") %>% html_text())
      links <- append(links, gsp_basin_href[[i]][[j]] %>% html_elements("a") %>% html_attr("href"))
      basin_names <- append(basin_names, gsp_basin_href[[i]][[j]] %>% html_elements("a") %>% html_text())
   }
}

if(length(linkless_names)>0){
   for(i in 1:length(linkless_names)){
      links <- append(links, NA)
   }
}

links_and_basin_names <- bind_cols(link = links, basin = c(basin_names, linkless_names))
#add numeric id "gsp_num_id" to spreadsheet
links_and_basin_names <- links_and_basin_names %>% 
   mutate(code = (substr(links_and_basin_names$link, 21,length(links_and_basin_names$link))))%>% 
   mutate(num_zeros = 4 - str_length(code)) %>% 
   mutate(gsp_num_id = paste(ifelse(num_zeros > 0, "0", ""),ifelse(num_zeros > 1,"0",""),ifelse(num_zeros > 2, "0",""),code,sep = "")) %>% 
   select(!c(code,num_zeros)) %>% 
   #find GSP_local_id and add it to spreadsheet
   mutate(gsp_local_id = gsp_table[[1]]['GSP Local ID'])

#go to webpage
pdf_link <- NULL
xlsx_link <- NULL
#1-3 successfully downloaded
for(i in 4:4){
   if(!is.na(links_and_basin_names$link[i])){
      remote_driver$navigate(paste("https://sgma.water.ca.gov",links_and_basin_names$link[i], sep = ""))
      #pdf_download
      # Specify URL where file is stored
      pdf_link <- remote_driver$findElement(using = "link text", "Groundwater Sustainability Plan")$getElementAttribute("href")
      # Specify destination where file should be saved
      destfilepdf <- paste('./data_raw/gsp_num_id_',links_and_basin_names$gsp_num_id[i],'.pdf',sep= "")
      download.file(pdf_link[[1]], destfilepdf, timeout = 300)
      Sys.sleep(5)
      print(paste("pdf",i,"downloaded"))
      #xlsx_download
      xlsx_link <- remote_driver$findElement(using = "link text", "Elements of the Plan")$getElementAttribute("href")
      destfilexlsx <- paste('./data_raw/gsp_num_id_',links_and_basin_names$gsp_num_id[i],'.xlsx',sep= "")
      download.file(xlsx_link[[1]], destfilexlsx, timeout = 120)
      print(paste("spreadsheet",i,"downloaded"))
      Sys.sleep(5)
   }
}

remote_driver$close()
rm(driver)