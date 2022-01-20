#takes the site id for each GSP and adds it to GSA_GSP_Basin_Coord.csv
#using selenium because rvest cannot read the table
library(stringr)
library("xml2")
library(polite)
library(httr)
library(tidyverse)
pacman::p_load(RSelenium, purrr, rvest, glue)

gsp_url <- "https://sgma.water.ca.gov/portal/gsp/all/"
paths_allowed(domain = gsp_url)
#returns TRUE
gsp_session <- bow(gsp_url, force = T)
#crawl delay 5 sec
#15 rules for 4 bots

driver <- rsDriver(port = 4444L, browser = "firefox")
remote_driver <- driver$client
remote_driver$open()
remote_driver$navigate(gsp_url)
Sys.sleep(5)

#find num entries button; changes to 100 entries per page
#TODO: switch methods to using button at bottom of page
#there is a bug here
num_entries_tmp <-remote_driver$findElement(using = "name", "gsp-tb_length")
num_entries_btn <- remote_driver$findElement(using = "css selector","option[value='100']")
#move pointer to button
remote_driver$mouseMoveToLocation(webElement = num_entries_btn)

#click on num entries button
num_entries_btn$click()
Sys.sleep(5)

#alternate method for code below
#bug:
#gsp_sel_portal <- remote_driver$getPageSource(gsp_url)

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
for(i in 1:length(gsp_basin_href)){
   for(j in 1:length(gsp_basin_href[[i]])){
      linkless_names <- append(linkless_names, gsp_basin_href[[i]][[j]] %>% html_elements("strong") %>% html_text())
      links <- append(links, gsp_basin_href[[i]][[j]] %>% html_elements("a") %>% html_attr("href"))
      basin_names <- append(basin_names, gsp_basin_href[[i]][[j]] %>% html_elements("a") %>% html_text())
   }
}

for(i in 1:length(linkless_names)){
   links <- append(links, NA)
}
   
links_and_basin_names <- bind_cols(links, c(basin_names, linkless_names))

#TODO: navigate to each page
#TODO: add id number from url to GSP ID in spreadsheet
#TODO: find GSP_local_id from each page and add it to spreadsheet
#TODO: find gsp pdfs and spreadsheets from each page

#all hyperlinks in table
#gsp_all_tbl_links <- gsp_html_readout %>% html_elements("td a") 


remote_driver$close()
rm(driver)

Sys.sleep(1)




#table is found at xml_child(xml_child(xml_child(xml_child(gsp_web_portal, 2), 1), 3), 6)

#gsp-tb is found at xml_attrs(xml_child(xml_child(xml_child(xml_child(gsp_web_portal, 2), 1), 3), 6))[["id"]]

#table-condensed... is found at xml_attrs(xml_child(xml_child(xml_child(xml_child(gsp_web_portal, 2), 1), 3), 6))[["class"]]

#syntax notes
# #is for ids
# p is for elements p
# . is for classes
# p.certain selects p elements of a class "certain"
# Then find elements that match a css selector or XPath expression
# using html_elements(). 
# p a selects all a elements that are a child of p

#body class chrome GSP > div class container > div class content > div id gsp-tb-wrapper >
   #table id gsp-tb > thead is the header

#body class chrome GSP > div class container > div class content > div id gsp-tb-wrapper >
#table id gsp-tb > tbody is the body of the table, and ...tbody > tr role = "row">
# td class = "sorting_2" > a href "/portal/gsp/preview/[IDnum]" is the portal to the gsp page

#body class chrome GSP > div class container > div class content > div id gsp-tb-wrapper >
#div class bottom is the page switcher



#saves each xls locally
#saves each pdf locally