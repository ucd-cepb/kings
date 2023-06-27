ret_path <- "/Users/elisemiller/miniconda3/envs/spacy_condaenv/bin/python"
generate_phrases <- FALSE
test_data <- FALSE
mini_data <- T
parse_from_file <- T
#mini_data uses only a sample of pages to make testing and editing go faster

if(test_data==T){
   pages <- (c("Invalid_sentence. DWR and DOT collaborate with the GSA to implement CDFA regulations.", 
             "Governor Smith advises and councils Benny Jordan. Only one organization exists. National_Conservation_Service and US_Dept_of_Ag are to work together on providing the GSA the appropriate regulation language. Yolo County is to send the GSA all correspondence related to the basin setting. They shall report to the Board.", 
             "The CDC will collaborate with NASA on the project. NRA's partners will include the FBI and several other agencies. The GSA is to submit their plan to the consultant. When the NSA meets with organizations such as the SWRCB, it is to take care to copy them on all correspondence.", 
             "If they partner together, the GDE plan must be documented. The GSAs may not outsource their work to any other organizations. Sacramento Water Board may work with other partners as deemed necessary. Davis City Council may decide to incorporate the recommendations of BCDC."))
   file_ids <- c("A","B","B","B")
   unique_files <- unique(file_ids)
   parsed_filenames <- paste0("data_temp/test_prs",unique_files)
   
}else{
   if(generate_phrases==T){
      gsp_text_with_meta <- readRDS("data_output/gsp_docs_w_meta")#if generate_phrases == T
   }else{
      gsp_text_with_meta <- readRDS("prepped_for_sna")#if generate_phrases == F
   }
   gsp_planonly <- gsp_text_with_meta[gsp_text_with_meta$is_comment==FALSE & gsp_text_with_meta$is_reference==FALSE,]
   
   if(mini_data==T){
      gsp_planonly <- gsp_planonly[25:31]
   }
   pages <- gsp_planonly$text
   file_ids <- gsp_planonly$gsp_id
   unique_files <- unique(file_ids)
   
   rm(gsp_text_with_meta)
   rm(gsp_planonly)
   
   if(mini_data==T){
      parsed_filenames <- paste0("data_output/parsed_mini_",unique_files)
   }else{
      parsed_filenames <- paste0("data_output/parsed_mini_",unique_files)
   }
}

generate_network(ret_path, generate_phrases, pages, file_ids, parsed_filenames, 
                 parse_from_file=F)


