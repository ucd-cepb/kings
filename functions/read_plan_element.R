
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
