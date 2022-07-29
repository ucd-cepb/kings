gsp_topic_analyzer<- function(dac_corr_check = F, build_meta = F, clean_lex = T, 
                              model_compare = F, run_model = T, viz_results = T, 
                              ntopics = NA){
   library(stm)
   library(tm)
   library(SnowballC)
   library(tidytext)
   library(data.table)
   library(tidyverse)
   library(sf)
   library(pbapply)
   library(geometry)
   library(Rtsne)
   library(rsvd)
   library(stringi)
   library(wordcloud)
   library(boxr)
   
   source('code/functions/lex_clean.R')
   source('code/functions/create_lang_meta.R')
   source('code/functions/create_spat_meta.R')
   source('code/functions/generate_place_names.R')
   source('code/functions/compare_models.R')
   source('code/functions/visualize_topics.R')
   
   if(dac_corr_check==T){
      #correlation check to determine which spatial metadata to use
      dac_svi_analysis("tract","area")
      dac_svi_analysis("place","area")
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
      gsp_out <- lex_clean(gsp_text_with_meta)
      
   }
   
   gsp_out <- readRDS(list.files(path = "data_temp", pattern = "slam", full.names = T)[length(
      list.files(path = "data_temp", pattern = "slam", full.names = T))])
   
   if(model_compare==T){
      selected_model <- compare_models(optimize_K = T, obj = gsp_out)
      numTopics = selected_model$settings$dim$K
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
