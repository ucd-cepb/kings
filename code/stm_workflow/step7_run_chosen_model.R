

gsp_out <- readRDS(list.files(path = "data_temp", pattern = "slam", full.names = T)[length(
list.files(path = "data_temp", pattern = "slam", full.names = T))])

#See step5_compare_models script for our process of selecting a value for K

#numTopics = selected_model$settings$dim$K

numTopics = ntopics


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


   
