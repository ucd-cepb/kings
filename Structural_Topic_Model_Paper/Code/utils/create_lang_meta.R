packs <- c('stm','tm','SnowballC','tidytext','data.table',
           'tidyverse','sf','pbapply','readxl')
need <- packs[!packs %in% installed.packages()[,'Package']]
if(length(need)>0){install.packages(need)}
lapply(packs, require, character.only = TRUE)

filekey <- read.csv("filekey.csv")

source(filekey[filekey$var_name=="web_data_repair_script",]$filepath)
create_lang_meta <- function(run_repair = F){
   
   #downloaded from https://data.cnra.ca.gov/dataset/sgma-basin-prioritization/resource/6347629e-340d-4faf-ae7f-159efbfbcdc9
   final_515_table <- read_excel(filekey[filekey$var_name=="basin_515",]$filepath)
   #how much of the basin's total (ag+urban) gw use is due to ag, as a percentage 0-1?
   final_515_table$basin_id <-   final_515_table$"Basin ID"
   final_515_table$basin_name <-final_515_table$"Basin Name"
   final_515_table$ag_gw_af <-   final_515_table$"AG GW \r\nBasin\r\n(AF)"
   final_515_table$habitat <- final_515_table$"Habitat\r\nExist\r\n\r\n1 = Yes,\r\nBlank = No"
   final_515_table$streamflow <- final_515_table$"Streamflow\r\nExist\r\n\r\n1 = Yes,\r\nBlank = No"
   final_515_table$priority <- final_515_table$"SGMA 2019 Basin Prioritization\r\n\r\nBasin\r\nPriority"
   final_515_table$fract_of_area_in_habitat <- final_515_table$"Total Area of Polygons" / final_515_table$"Basin Area (Acre)"
   final_515_table$urbangw_af <- final_515_table$"Urban GW Volume (AF)"
   final_515_table$declininggw <- final_515_table$"Component 7.a\r\n\r\nDeclining GW Levels\r\nPoints" 
   final_515_table$subsidence <- final_515_table$"Component 7.b\r\n\r\nSubsidence Points\r\n"
   final_515_table$saltintrusion <- final_515_table$"Component 7.c\r\n\r\nSalt Intrusion Points"
   final_515_table$exceedance <- final_515_table$"WQ - Total Number of unique wells with an MCL Excd between 1/1/2000 and 4/1/2017"
   final_515_table <- final_515_table %>% mutate(ag_gw_asfractof_tot_gw = ag_gw_af/
                                                    (ag_gw_af+urbangw_af))
   
   #converting gw severity to binary metrics
   final_515_table$subsidence <- ifelse(final_515_table$subsidence>0,1,0)
   final_515_table$saltintrusion <-ifelse(final_515_table$saltintrusion>0,1,0)
   final_515_table$declininggw <- ifelse(final_515_table$declininggw>0,1,0)
   
   
   final_515_table$gwsum <- final_515_table$declininggw + 
      final_515_table$saltintrusion + 
      final_515_table$subsidence
   final_515_table <- select(final_515_table, 
                             c(basin_id, basin_name, priority, 
                               fract_of_area_in_habitat, urbangw_af, gwsum, exceedance))

   gsp_tblfilename <- filekey[filekey$var_name=="gsp_web_vars_repaired_stmpaper",]$filepath
   gsp_tblfilenamesplits <- unlist(strsplit(gsp_tblfilename,split="/"))
   gsp_tblpath <- paste(gsp_tblfilenamesplits[1:(length(gsp_tblfilenamesplits)-1)],collapse = "/")
   gsp_tblpattern <- gsp_tblfilenamesplits[length(gsp_tblfilenamesplits)]
   
   if(run_repair == T){
      #separating filepath from pattern, according to filekey
      gspwebvarsfilename <- filekey[filekey$var_name=="gsp_web_vars_stmpaper",]$filepath
      gspwebvarsfilenamesplits <- unlist(strsplit(gspwebvarsfilename,split="/"))
      gspwebvarspath <- paste(gspwebvarsfilenamesplits[1:(length(gspwebvarsfilenamesplits)-1)],collapse = "/")
      gspwebvarspattern <- gspwebvarsfilenamesplits[length(gspwebvarsfilenamesplits)]
      
      gsp_tbl <- readRDS(list.files(path = gspwebvarspath, pattern = gspwebvarspattern, full.names = T)[
         length(list.files(path = gspwebvarspath, pattern = gspwebvarspattern, full.names = T))])
      #joins gsp vars with basin vars
      #filters out basin ids without plans     
      gsp_tbl <- cbind(gsp_tbl, "basin_id" = sub(" .*", "", gsp_tbl$basin))
      
      setnames(gsp_tbl,old="gsp_num_id",new="gsp_id") 
      
      
      gsp_tbl <- web_data_repair(new_tbl = gsp_tbl,
                                 old_tbl = read_csv(filekey[filekey$var_name=="gsp_web_vars_2022needs_repair",]$filepath))

      saveRDS(gsp_tbl, file = paste0(gsp_tblpath,gsp_tblpattern,format(Sys.time(), "%Y%m%d-%H:%M")))
      
   }
   gsp_tbl <- readRDS(list.files(path = gsp_tblpath, pattern = gsp_tblpattern, full.names = T)[
      length(list.files(path = gsp_tblpath, pattern = gsp_tblpattern, full.names = T))])
   
   bsn_and_plan_vars <- merge(gsp_tbl, final_515_table, all.x = T, 
                              by = "basin_id") 
   page_num <- integer(0)
   gsp_listfilename <- filekey[filekey$var_name=="gsp_text_and_categories_stmpaper",]$filepath
   gsp_listfilenamesplits <- unlist(strsplit(gsp_listfilename,split="/"))
   gsp_listpath <- paste(gsp_listfilenamesplits[1:(length(gsp_listfilenamesplits)-1)],collapse = "/")
   
   gsp_list <- list.files(path = gsp_listpath, pattern = "_text", full.names = T)
   
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
      key_k <- readRDS(paste0(gsp_listfilename,gsub("\\D", "", gsp_list[k]),"_categories"))
      gsp_id[page_k1:(page_k1+length(gsp_k)-1)] <- rep.int(c(gsub("\\D", "", gsp_list[k])),times = length(gsp_k))
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
   
   saveRDS(all_text_subcat, file = filekey[filekey$var_name=="page_subcategory_designation",]$filepath)
   saveRDS(all_text_cat, file = filekey[filekey$var_name=="page_category_designation",]$filepath)
   saveRDS(cat_selector, file = filekey[filekey$var_name=="page_in_at_least_one_category",]$filepath)
   saveRDS(gsp_text_with_lang, file = filekey[filekey$var_name=="gsp_docs_lang",]$filepath)
   
   return(gsp_text_with_lang)
   
}
