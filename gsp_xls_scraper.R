#takes an xls file and identifies the pdf pages associated with each
#required element of a management plan, returns this info as a data.table
library(tidyverse)
library(readxl)
library(data.table)

#test: file = "data_raw/gsp_num_id_0065.xlsx"

create_page_key <- function(file){
   plan <- read_plan_element(file)
   return(consolidate_pgs(plan))
}
read_plan_element <- function(file){
   gsp_num_id <- substr(file,21,24)
   plan_table <- read_excel(file, 
                        sheet = "Elements of The Plan", 
                        col_names = c("law_loc","article_5_section","subsection","sub-subsection",
                                      "plan_contents","page_num","section_num",
                                      "figure_num","table_num","notes","gis_files_reqd"), skip = 2)
   plan_table <- plan_table %>% mutate(category = NA, subcategory = NA)
   for(i in 1:length(plan_table$article_5_section)){
      j <- i+1
      if(!is.na(plan_table$article_5_section[i])){
         if(substr(plan_table$article_5_section[i],1,1)=="S"){
            plan_table$category[i] <- plan_table$plan_contents[i]
            while(j<=length(plan_table$article_5_section) & 
                  (is.na(plan_table$article_5_section[j]) || 
                   substr(plan_table$article_5_section[j],1,1)!="S")){
               if(j<=length(plan_table$article_5_section)){
                  plan_table$category[j] <-plan_table$plan_contents[i]
               }
               j <- j+1
            }#end of while
         }#end of "S" if
         else if(substr(plan_table$article_5_section[i],1,1)=="§"){
            plan_table$subcategory[i] <- plan_table$plan_contents[i]
            while(j<=length(plan_table$article_5_section) & 
                  (is.na(plan_table$article_5_section[j]) || 
                   ((substr(plan_table$article_5_section[j],1,1)!="S") & 
                    (substr(plan_table$article_5_section[j],1,1)!="§")))){
               if(j<=length(plan_table$article_5_section)){
                  plan_table$subcategory[j] <-plan_table$plan_contents[i]
               }
               j <- j+1
            }#end of while
         }
      }#end of not NA if
   }#end of search
   plan_parts <- plan_table %>% select(plan_contents:table_num,category:subcategory)%>% 
      filter((!is.na(page_num))|(!is.na(section_num))|
                (!is.na(figure_num))|(!is.na(table_num)))
   return(plan_parts)
}






#returns a table with consolidated page numbers for each category and subcat
consolidate_pgs <- function(tbl) {
   #consolidate page numbers by subcategory and by category
   plancontent_pages <- tbl %>% select(plan_contents, category, subcategory, page_num)
   subcat_pages <- tbl %>%  group_by(subcategory) %>%  
      mutate(plan_contents = "all in subcategory", page_num = paste0(unique(page_num), collapse = ", ")) %>% 
      ungroup() %>% select(category, subcategory, page_num) %>% distinct(subcategory, .keep_all = T)
   cat_pages <- tbl %>%  group_by(category) %>%  
      mutate(subcategory = "all in category", plan_contents = "all in category", page_num = paste0(unique(page_num), collapse = ", ")) %>% 
      ungroup() %>% select(category, subcategory, page_num) %>% distinct(category, .keep_all = T)
   #bind_rows to new table
   all_pages <- data.table(bind_rows(cat_pages, subcat_pages, plancontent_pages))
   #create temp column that is a vector of page ranges
   all_pages <- add_column(all_pages, page_ranges = strsplit(all_pages$page_num, split = ", "))
   #split this into two new columns that represent start and end pages
   all_pages <- add_column(all_pages, starts_ends = lapply(all_dt$page_ranges, strsplit, split = ":"))
   all_pages <- add_column(all_pages, page_vector = list(NA))
   
   cvector <- NULL
   for(i in 1:length(all_pages$starts_ends)){
      for(j in 1:length(all_pages$starts_ends[[i]])){
         if(!is.na(all_pages$starts_ends[[i]][[j]][1]) & 
            all_pages$starts_ends[[i]][[j]][1] != "N/A"){
            if(length(all_pages$starts_ends[[i]][[j]])==2){
               cvector <- append(cvector, c(as.integer(
                  all_pages$starts_ends[[i]][[j]][1]):as.integer(
                     all_pages$starts_ends[[i]][[j]][2])))
            }else {
               cvector <- append(cvector, as.integer(all_pages$starts_ends[[i]][[j]][1]))
            }
         }
      }
      if(is.null(cvector)){
         all_pages$page_vector[[i]] <- NA
      }
      else{
         #page_vector is a list of all pages in vector form
         all_pages$page_vector[[i]] <- sort(unique(cvector))         
      }

      cvector <- NULL
   }
   all_pages[,c("page_num","page_ranges","starts_ends"):=NULL]
   return(all_pages)
   
}



