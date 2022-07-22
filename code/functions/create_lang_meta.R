library(stm)
library(tm)
library(SnowballC)
library(tidytext)
library(data.table)
library(tidyverse)
library(sf)
library(pbapply)
library(readxl)

source("code/functions/web_data_repair.R")
create_lang_meta <- function(){
   
   #downloaded from https://data.cnra.ca.gov/dataset/sgma-basin-prioritization/resource/6347629e-340d-4faf-ae7f-159efbfbcdc9
   final_515_table <- read_excel("data_raw/final-515-table.xlsx")
   #how much of the basin's total (ag+urban) gw use is due to ag, as a percentage 0-1?
   final_515_table$basin_id <-   final_515_table$"Basin ID"
   final_515_table$basin_name <-final_515_table$"Basin Name"
   final_515_table$ag_gw_af <-   final_515_table$"AG GW \r\nBasin\r\n(AF)"
   final_515_table$urb_gw_af <- final_515_table$"Urban GW Volume (AF)"
   final_515_table$priority <- final_515_table$"SGMA 2019 Basin Prioritization\r\n\r\nBasin\r\nPriority"
   final_515_table <- final_515_table %>% mutate(ag_gw_asfractof_tot_gw = ag_gw_af/
                                                    (ag_gw_af+urb_gw_af))
   final_515_table <- select(final_515_table, 
                             c(basin_id, basin_name, ag_gw_asfractof_tot_gw, priority))

   gsp_tbl <- readRDS(list.files(path = "data_output", pattern = "web_vars", full.names = T)[
      length(list.files(path = "data_output", pattern = "web_vars", full.names = T))])
   #joins gsp vars with basin vars
   #filters out basin ids without plans     
   gsp_tbl <- cbind(gsp_tbl, "basin_id" = sub(" .*", "", gsp_tbl$basin))
   
   setnames(gsp_tbl,old="gsp_num_id",new="gsp_id") 
   
   gsp_tbl <- web_data_repair(new_tbl = gsp_tbl,
                              old_tbl = read_csv(list.files(path = "data_output", pattern = "gsp_ids", full.names = T)[
      length(list.files(path = "data_output", pattern = "gsp_ids", full.names = T))]))
   
   bsn_and_plan_vars <- merge(gsp_tbl, final_515_table, all.x = T, 
                              by = "basin_id") 
   page_num <- integer(0)
   gsp_list <- list.files(path = "data_output", pattern = "_text", full.names = T)
   
   #minimize num of vars initialized at length zero by using page counter
   for(k in 1:length(gsp_list)){
      gsp_k <- readRDS(gsp_list[k])
      page_num <- append(page_num, c(1:length(gsp_k)))
   }
   
   gsp_id <- character(length(page_num))
   all_gsp_text <- character(length(page_num))
   is_comment <- logical(length(page_num))
   is_reference <- logical(length(page_num))
   
   
   all_text_subcat <- vector(mode = "list", length = 0)
   all_text_cat <- vector(mode = "list", length = 0)

   
   for(k in 1:length(gsp_list)){
      #find first page of doc k
      page_k1 <- which(page_num %in% 1)[k]
      gsp_k <- readRDS(gsp_list[k])
      key_k <- readRDS(paste0("data_output/gsp_num_id_",substr(gsp_list[k],24,27),"_categories"))
      gsp_id[page_k1:(page_k1+length(gsp_k)-1)] <- rep.int(c(substr(gsp_list[k],24,27)),times = length(gsp_k))
      #i = page number
      for (i in 1:length(gsp_k)){
         page_cat <- NULL
         page_subcat <- NULL
         #j = subcategory
         for(j in 1:5){
            if(i %in% key_k$page_vector[[j]]){
               page_cat <- append(page_cat, key_k$category[[j]])
            }
         }
         for (j in 6:21){
            if(i %in% key_k$page_vector[[j]]){
               page_subcat <- append(page_subcat, key_k$subcategory[[j]])
            }
         }
         
         if(i %in% key_k$page_vector[[23]]){
            is_reference[page_k1+i-1] <- TRUE
         }else{
            is_reference[page_k1+i-1]  <- FALSE
         }
         
         if(i %in% key_k$page_vector[[46]]){
            is_comment[page_k1+i-1] <- TRUE
         }else{
            is_comment[page_k1+i-1]  <- FALSE
         }
         
         all_text_cat[page_k1+i-1] <- list(page_cat)
         all_text_subcat[page_k1+i-1] <- list(page_subcat)
      }
      all_gsp_text[page_k1:(page_k1+length(gsp_k)-1)] <- gsp_k
   }
   
   #check percentage of pages tagged with more than one subcategory, as decimal
   sum(lengths(all_text_subcat)>1)/length(all_gsp_text)
   #check percentage of pages tagged with more than one category, as decimal
   sum(lengths(all_text_cat)>1)/length(all_gsp_text)
   
   #TODO pick up here
   #dummying out category
   is_admin <- logical(length(page_num))
   is_basin <- logical(length(page_num))
   is_criteria <- logical(length(page_num))
   is_monitoring <- logical(length(page_num))
   is_projects <- logical(length(page_num))
   for(i in 1:length(all_text_cat)){
      is_admin[i] <- ifelse("Administrative Information" %in% all_text_cat[[i]],TRUE,FALSE)
      is_basin[i] <- ifelse("Basin Setting" %in% all_text_cat[[i]],TRUE,FALSE)
      is_criteria[i] <- ifelse("Sustainable Management Criteria" %in% all_text_cat[[i]],TRUE,FALSE)
      is_monitoring[i] <- ifelse("Monitoring Networks" %in% all_text_cat[[i]],TRUE,FALSE)
      is_projects[i] <- ifelse("Projects and Management Actions" %in% all_text_cat[[i]],TRUE,FALSE)
   }
   
   #remove non-visible characters
   all_gsp_text <- stringr::str_replace_all(all_gsp_text,"[^[:graph:]]", " ")
   
   #add cat metadata
   gsp_text_with_lang <- data.table(text = all_gsp_text, admin = is_admin, basin_plan = is_basin,
                                    sust_criteria = is_criteria, monitoring_networks = is_monitoring,
                                    projects_mgmt_actions = is_projects, gsp_id = gsp_id,
                                    is_comment = is_comment, is_reference = is_reference, 
                                    page_num = page_num)
   
     
   bsn_and_plan_vars <- bsn_and_plan_vars[!is.na(bsn_and_plan_vars$gsp_id)]
   gsp_text_with_lang <- full_join(gsp_text_with_lang, bsn_and_plan_vars)
   
    
   
   
   #use to filter out nulls in category
   cat_selector <- !sapply(all_text_cat,is.null)
   #use cat_selector to subset text and all metadata vectors
   
   saveRDS(all_text_subcat, file = paste0("data_temp/","gsp_docs_subcat_",format(Sys.time(), "%Y%m%d-%H:%M")))
   saveRDS(all_text_cat, file = paste0("data_temp/","gsp_docs_cat_",format(Sys.time(), "%Y%m%d-%H:%M")))
   saveRDS(cat_selector, file = paste0("data_temp/","gsp_docs_cat_notnull_",format(Sys.time(), "%Y%m%d-%H:%M")))
   saveRDS(gsp_text_with_lang, file = paste0("data_output/","gsp_docs_w_lang_",format(Sys.time(), "%Y%m%d-%H:%M")))
   
   return(gsp_text_with_lang)
   
}
