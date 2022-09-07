gsp_topic_analyzer<- function(dac_corr_check = F, build_meta = F, clean_lex = T, 
                              run_model = T, viz_results = T, 
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
   #source('code/functions/compare_models.R')
   source('code/functions/visualize_topics.R')
   source('code/functions/dac_svi_analysis.R')
   source('code/functions/summarize_pws_by_gsp.R')
   
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
      gsp_pws <- summarize_pws_by_gsp() %>% st_drop_geometry() %>% 
         select(gsp_id, service_count,hviol_avg_res,prop_service_gw_source)
      #rows = num docs; cols = metadata types
      gsp_text_with_meta <- full_join(gsp_text_with_lang, gsp_dac, by = c("gsp_id"="gsp_num_id"))
      gsp_text_with_meta <- full_join(gsp_text_with_meta, gsp_pws, by = "gsp_id")
      
      gsp_text_with_meta <- gsp_text_with_meta %>% mutate(
                                 hviol_avg_res = ifelse(is.na(hviol_avg_res),0,hviol_avg_res),
                                 service_count = ifelse(is.na(service_count),0,service_count),
                                 prop_service_gw_source = 
                                    ifelse(is.na(prop_service_gw_source),1,prop_service_gw_source)) 
                             
      #filtering NA admin = proxy for GSPs whose texts have not been processed
      gsp_text_with_meta <- gsp_text_with_meta %>% filter(!is.na(admin))
      
      saveRDS(gsp_text_with_meta, file = "data_output/gsp_docs_w_meta")
   }
   
   #retrieves the latest save of gsp_text_with_meta
   gsp_text_with_meta <- readRDS("data_output/gsp_docs_w_meta")
   
   topic_indicators <- list(ej = c("disadvantaged community", "disadvantaged communities",
                                   "^community$","engagement","outreach","environmental_justice"),
                            dw = c("drinking water", "water quality","safe","^well$","^wells$"),
                            cc = c("climate change","projection","projections"),
                            gde = c("groundwater-dependent ecosystem",
                                    "groundwater dependent ecosystem",
                                    "groundwater-dependent ecosystems",
                                    "groundwater dependent ecosystems",
                                    "^gde$","^gdes$","habitat","species"))

   if(clean_lex == T){
      gsp_out <- lex_clean(gsp_text_with_meta, rm_plnames = F,
                           topic_indicators = unlist(topic_indicators,use.names=F))
      
   }
   
   gsp_out <- readRDS(list.files(path = "data_temp", pattern = "slam", full.names = T)[length(
      list.files(path = "data_temp", pattern = "slam", full.names = T))])
   
   #See compare_models script for our process of selecting a value for K
   
   #numTopics = selected_model$settings$dim$K
   
   numTopics = ntopics

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
                          ag_gw_asfractof_tot_gw+
                          hviol_avg_res+
                          prop_service_gw_source+
                          service_count,
                       max.em.its = 50,
                       data = gsp_out$meta, init.type = "Spectral") 
      
      while (!gsp_model$convergence$converged){
         gsp_model <- stm(documents = gsp_out$documents, vocab = gsp_out$vocab,
                        K = numTopics,
                        prevalence =~ admin + 
                           basin_plan +
                           sust_criteria +
                           monitoring_networks + 
                           projects_mgmt_actions + 
                           percent_dac_by_pop+
                           as.factor(approval)+
                           as.factor(priority)+
                           mult_gsas+
                           ag_gw_asfractof_tot_gw+
                           hviol_avg_res+
                           prop_service_gw_source+
                           service_count,
                        init.type = "Spectral",
                        max.em.its = gsp_model$settings$convergence$max.em.its + 30,
                        data = gsp_out$meta,
                        model = gsp_model)
         saveRDS(gsp_model, paste0("data_temp/gsp_partial"))
      }
      #dummy for how many gsas are involved: multiple or one
      
      saveRDS(gsp_model, file = paste0("data_output/mdl/","model_",format(Sys.time(), "%Y%m%d-%H:%M")))
      
   }
   
   gsp_model_saved <- readRDS(list.files(path = "data_output/mdl", pattern = "model", full.names = T)[length(
      list.files(path = "data_output/mdl", pattern = "model", full.names = T))])
   
   if(viz_topics == T){
      visualize_topics(gsp_model_saved, 
                       gsp_out, 
                       gsp_text_with_meta$text[gsp_text_with_meta$is_comment == F &
                                                  gsp_text_with_meta$is_reference == F],
                       topic_indicators)
      
   }
   
   return(gsp_model_saved)
}
