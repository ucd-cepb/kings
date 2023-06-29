ret_path <- "/Users/elisemiller/miniconda3/envs/spacy_condaenv/bin/python"
generate_phrases <- FALSE
test_data <- FALSE
mini_data <- T
parse_from_file <- F
#mini_data uses only a sample of pages to make testing and editing go faster

if(test_data==T){
   pages <- (c("Invalid_sentence. DWR and DOT collaborate with the GSA to implement CDFA regulations.", 
             "Incorrect sentence. Percy Willis advises Frankie. Governor Smith advises and councils Benny Jordan. Philip Johnson and Sarah McMurray meet and discuss with their collaborators at USDA. National_Conservation_Service and US_Dept_of_Ag are to work together on providing the GSA the appropriate regulation language. Yolo County is to send the GSA all correspondence related to the basin setting. They shall report to the Board.", 
             "The CDC will collaborate with NASA and coordinate with SGMA on the project. The FBI and CIA agree to meet with several other agencies. The GSA is to submit their plan to the consultant. When the NSA meets with organizations such as the SWRCB, it is to take care to copy them on all correspondence.", 
             "If they partner together, the GDE plan must be documented. The GSAs may not outsource their work to any other organizations. Sacramento Water Board may work with other partners and submit documentation as deemed necessary. Davis City Council may decide to incorporate the recommendations of BCDC."))
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

if(mini_data==T | test_data==T){
   nodeedge_filenames <- paste0("network_extr_test/",unique_files)
   
}else{
   nodeedge_filenames <- paste0("network_extracts/",unique_files)
   
}

phrases_to_concatenate <- generate_proper_names(underscore=F,to_lower=F)

generate_networks(ret_path, generate_phrases, phrases_to_concatenate, 
                  concatenator="_",
                  pages, file_ids, parsed_filenames, 
                 nodeedge_filenames, parse_from_file)


