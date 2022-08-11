compare_models <- function(optimize_K=F, topic_stability = F, obj, numTopics = 0, enterChoice = F){
   
   if(optimize_K==T){
      #let searchK figure out how many topics to generate
      k_options <- searchK(gsp_out$documents, gsp_out$vocab, 
                           K = c(5, 10, 20, 40, 80),
                           prevalence =~ admin + 
                              basin_plan +
                              sust_criteria +
                              monitoring_networks + 
                              projects_mgmt_actions + 
                              SVI_na_adj+
                              as.factor(approval)+
                              as.factor(priority)+
                              ag_gw_asfractof_tot_gw,
                           N = floor(0.05 * length(documents)),
                           data = gsp_out$meta)
      plot(k_options)
      if(enterChoice == T){
         #functionality to choose your favorite model
         sel_model_num <- as.integer(
            readline(prompt = "Enter the number of your preferred model: "))
         selected_model <- k_options$runout[[sel_model_num]]
      }
      
   }
   #can skip this as well as multi-modality check if model uses Spectral
   if(topic_stability==T){
      stab_compare <- selectModel(out$documents, out$vocab, 
                                   K = numTopics,
                                   prevalence =~ admin + 
                                      basin_plan +
                                      sust_criteria +
                                      monitoring_networks + 
                                      projects_mgmt_actions + 
                                      SVI_na_adj+
                                      as.factor(approval)+
                                      as.factor(priority)+
                                      ag_gw_asfractof_tot_gw, 
                                   max.em.its = 75,
                                   data = out$meta, 
                                   runs = 20, 
                                   seed = 12143278)
      plotModels(stab_compare, legend.position = "bottomright")
      #can also add residuals checks and held-out likelihood estimation
      
   }
   
   model_comp <- list("k_options" = ifelse(exists("k_options"),k_options,NA), 
                      "k_plot" = ifelse(exists("k_plot"),k_plot,NA),
                      "my_k_model" = ifelse(exists("selected_model"), selected_model,NA),
                      "stab_compare" = ifelse(exists("stab_compare"),stab_compare,NA),
                      "stability_plot" = ifelse(exists("stability_plot"),stability_plot,NA))
   saveRDS(model_comp, "data_temp/model_comparison")
   return(model_comp)
   
}