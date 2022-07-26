gsp_topic_analyzer<- function(build_meta = F, clean_lex = T, 
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
   
   source('code/functions/lex_clean.R')
   source('code/functions/create_lang_meta.R')
   source('code/functions/create_spat_meta.R')
   source('code/functions/generate_place_names.R')
   source('code/functions/compare_models.R')
   source('code/functions/visualize_topics.R')
   
   if(build_meta==T){
      gsp_text_with_lang <- create_lang_meta(run_repair = F)
      
      #retrieves the latest save of gsp_text_with_lang
      #generated in create_lang_meta, which allows create_lang_meta() to be skipped
      
      gsp_text_with_lang <- readRDS(
         list.files(path = "data_output",pattern = "lang", full.names = T)[length(
            list.files(path = "data_output", pattern = "lang", full.names = T))])
      
      
      type = "area"
      #or type = "pop"
      gsp_svi_adjusted <- create_svi_meta(type)
      
      #retrieves lates save of gsp_svi_adjusted of the given type, which allows
      #create_svi_meta to be skipped
      gsp_svi_adjusted <- readRDS(
         list.files(path = "data_output",pattern = type,full.names = T)[length(
            list.files(path = "data_output",pattern = type,full.names = T))])
      
      
      #rows = num docs; cols = metadata types
      
      #gsp_meta <- data.table(matrix(ncol = 4, nrow = 0))
      #colnames(gsp_meta) <- c("GSA","community_attributes","ag_importance","soc_vuln")
      
      gsp_text_with_meta <- full_join(gsp_text_with_lang, gsp_svi_adjusted, by = c("gsp_id"="gsp_num_id"))
      #filtering NA admin = proxy for GSPs whose texts have not been processed
      gsp_text_with_meta <- gsp_text_with_meta %>% filter(!is.na(admin))
      
      saveRDS(gsp_text_with_meta, file = paste0("data_output/","gsp_docs_w_meta_",format(Sys.time(), "%Y%m%d-%H:%M")))
      
   }
   
   #retrieves the latest save of gsp_text_with_meta
   gsp_text_with_meta <- readRDS(
      list.files(path = "data_output",pattern = "meta", full.names = T)[length(
         list.files(path = "data_output", pattern = "meta", full.names = T))])
   
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
                          SVI_na_adj+
                          as.factor(approval)+
                          as.factor(priority)+
                          ag_gw_asfractof_tot_gw,
                       max.em.its = 150,
                       data = gsp_out$meta, init.type = "Spectral")  
      #dummy for how many gsas are involved: multiple or one
      
      saveRDS(gsp_model, file = paste0("data_output/","model_",format(Sys.time(), "%Y%m%d-%H:%M")))
      
   }
   
   gsp_model_saved <- readRDS(list.files(path = "data_output", pattern = "model", full.names = T)[length(
      list.files(path = "data_output", pattern = "model", full.names = T))])
   
   if(viz_topics == T){
      visualize_topics(gsp_model_saved, 
                       gsp_out, 
                       gsp_text_with_meta$text[gsp_text_with_meta$is_comment == F &
                                                  gsp_text_with_meta$is_reference == F])
      
   }
   
   return(gsp_model_saved)
}
