library(dotenv)
library(tidyverse)

load_dot_env()
pages_fp <- paste0(Sys.getenv("BOX_PATH"), 
                     "/Structural_Topic_Model_Paper/gsp_docs_lean")

page_features <- tibble(readRDS(pages_fp))

test_str <- c("data_raw/portal/gsp_num_id_0013.pdf100_17_12",
              "data_raw/portal/gsp_num_id_0015.pdf19_17_12",
              "data_raw/portal/gsp_num_id_0021.pdf25_17_12",
              "data_raw/portal/gsp_num_id_0005.pdf11_17_12",
              "data_raw/portal/gsp_num_id_0088.pdf88_17_12")

parent_loc_to_section <- function(pointer_str){
   # split str after '.pdf' and before '_'
   pointer_str <- strsplit(pointer_str, "_")[[1]][5]
   gsp_id_in <- as.numeric(strsplit(pointer_str, ".pdf")[[1]][1])
   page_num_in <- as.numeric(strsplit(pointer_str, ".pdf")[[1]][2])
   print(gsp_id_in)
   print(page_num_in)
   
   page_sections <- page_features %>% 
      mutate(gsp_id = as.numeric(gsp_id)) %>%
      filter(gsp_id == gsp_id_in) %>% 
      filter(page_num == page_num_in) %>% 
      select(gsp_id, page_num, is_comment, is_reference, 
             admin, basin_plan, sust_criteria, 
             monitoring_networks, projects_mgmt_actions)
   
   return(page_sections)
}
