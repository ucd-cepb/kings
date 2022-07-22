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
   all_pages <- add_column(all_pages, page_ranges = strsplit(all_pages$page_num, split = ",\\s*"))
   #split this into two new columns that represent start and end pages
   all_pages <- add_column(all_pages, starts_ends = lapply(all_pages$page_ranges, strsplit, split = ":"))
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
