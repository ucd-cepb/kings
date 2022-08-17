gsp_topic_analyzer<- function(dac_corr_check = F, build_meta = F, clean_lex = T, 
                              model_compare = F, run_model = T, viz_results = T, 
                              ntopics = NA){
   
   packs <- c('stm','tm','SnowballC','tidytext','data.table',
              'tidyverse','sf','pbapply','geometry','Rtsne','rsvd',
              'stringi','stringr','scico','boxr')
   need <- packs[!packs %in% installed.packages()[,'Package']]
   if(length(need)>0){install.packages(need)}
   check <- packs[!packs %in% installed.packages()[,'Package']]
   if(length(check>0)){print(check)}else{print("got 'em all")}
   lapply(packs, require, character.only = TRUE)
   
   source('code/functions/lex_clean.R')
   source('code/functions/create_lang_meta.R')
   source('code/functions/create_spat_meta.R')
   source('code/functions/generate_proper_names.R')
   source('code/functions/compare_models.R')
   source('code/functions/visualize_topics.R')
   source('code/functions/dac_svi_analysis.R')
   
   if(dac_corr_check==T){
      #correlation check to determine which spatial metadata to use
      results_tract <- dac_svi_analysis("tract")
      results_place_pop <- dac_svi_analysis("place","pop")
      results_place_area <- dac_svi_analysis("place","area")
   }
   
   if(build_meta==T){
      gsp_text_with_lang <- create_lang_meta(run_repair = F)
      
      #retrieves the latest save of gsp_text_with_lang
      #generated in create_lang_meta, which allows create_lang_meta() to be skipped
      
      gsp_text_with_lang <- readRDS("data_output/gsp_docs_w_lang")
      
      type = "pop"
      #or type = "area"
      scope = "blockgroup"
      gsp_dac <- create_dac_meta(type, scope, box_sync = T,overwrite = T)
  
      #rows = num docs; cols = metadata types
      gsp_text_with_meta <- full_join(gsp_text_with_lang, gsp_dac, by = c("gsp_id"="gsp_num_id"))
      #filtering NA admin = proxy for GSPs whose texts have not been processed
      gsp_text_with_meta <- gsp_text_with_meta %>% filter(!is.na(admin))
      
      saveRDS(gsp_text_with_meta, file = "data_output/gsp_docs_w_meta")
   }
   
   #retrieves the latest save of gsp_text_with_meta
   gsp_text_with_meta <- readRDS("data_output/gsp_docs_w_meta")
   
   if(clean_lex == T){
      gsp_out <- lex_clean(gsp_text_with_meta, rm_plnames = F)
      
   }
   
   gsp_out <- readRDS(list.files(path = "data_temp", pattern = "slam", full.names = T)[length(
      list.files(path = "data_temp", pattern = "slam", full.names = T))])
   
   if(model_compare==T){
      #uses too much memory to do all at once
      k_set <- c(120,160,200)
      models <- compare_models(optimize_K = T, k_set, obj = gsp_out)
      k_set <- c(5,10,20)
      models <- compare_models(optimize_K = T, k_set, obj = gsp_out)
      k_set <- c(40,80)
      models <- compare_models(optimize_K = T, k_set, obj = gsp_out)
      
      #numTopics = selected_model$settings$dim$K
   }else{
      numTopics = ntopics
   }

   if(run_model == T){
      gsp_model <- stm(documents = gsp_out$documents, vocab = gsp_out$vocab,
                       K = numTopics, prevalence =~ admin + 
                          basin_plan +
                          sust_criteria +
                          monitoring_networks + 
                          projects_mgmt_actions + 
                          percent_dac_by_pop+
                          as.factor(approval)+
                          as.factor(priority)+
                          mult_gsas+
                          ag_gw_asfractof_tot_gw,
                       max.em.its = 150,
                       data = gsp_out$meta, init.type = "Spectral")  
      #dummy for how many gsas are involved: multiple or one
      
      saveRDS(gsp_model, file = paste0("data_output/mdl","model_",format(Sys.time(), "%Y%m%d-%H:%M")))
      
   }
   
   gsp_model_saved <- readRDS(list.files(path = "data_output/mdl", pattern = "model", full.names = T)[length(
      list.files(path = "data_output/mdl", pattern = "model", full.names = T))])
   
   if(viz_topics == T){
      visualize_topics(gsp_model_saved, 
                       gsp_out, 
                       gsp_text_with_meta$text[gsp_text_with_meta$is_comment == F &
                                                  gsp_text_with_meta$is_reference == F])
      
   }
   
   return(gsp_model_saved)
}
