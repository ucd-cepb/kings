packs <- c('stm')
need <- packs[!packs %in% installed.packages()[,'Package']]
if(length(need)>0){install.packages(need)}
check <- packs[!packs %in% installed.packages()[,'Package']]
if(length(check>0)){print(check)}else{print("got 'em all")}
lapply(packs, require, character.only = TRUE)

gsp_out <- readRDS(list.files(path = "data/temp_large_files", pattern = "slam", full.names = T)[length(
list.files(path = "data/temp_large_files", pattern = "slam", full.names = T))])

#See step5_compare_models script for our process of selecting a value for K

#numTopics = selected_model$settings$dim$K

numTopics = 67


gsp_model <- stm(documents = gsp_out$documents, vocab = gsp_out$vocab,
                 K = numTopics, prevalence =~ admin + 
                    basin_plan +
                    sust_criteria +
                    monitoring_networks + 
                    projects_mgmt_actions + 
                    urbangw_af_log_scaled +
                    percent_dac_by_pop_scaled+
                    fract_of_area_in_habitat_log_scaled +
                    maxdryspell_scaled +
                    Agr_Share_Of_GDP_scaled +
                    Republican_Vote_Share_scaled +
                    Perc_Bach_Degree_Over25_scaled +
                    local_govs_per_10k_people_log_scaled +
                    mult_gsas +
                    gwsum,
                 max.em.its = 30,
                 data = gsp_out$meta, init.type = "Spectral") 

while (!gsp_model$convergence$converged){
   gsp_model <- stm(documents = gsp_out$documents, vocab = gsp_out$vocab,
                    K = numTopics,
                    prevalence =~ admin + 
                       basin_plan +
                       sust_criteria +
                       monitoring_networks + 
                       projects_mgmt_actions + 
                       urbangw_af_log_scaled +
                       percent_dac_by_pop_scaled+
                       fract_of_area_in_habitat_log_scaled +
                       maxdryspell_scaled +
                       Agr_Share_Of_GDP_scaled +
                       Republican_Vote_Share_scaled +
                       Perc_Bach_Degree_Over25_scaled +
                       local_govs_per_10k_people_log_scaled +
                       mult_gsas +#dummy for how many gsas are involved: multiple or one
                       gwsum,
                    init.type = "Spectral",
                    max.em.its = gsp_model$settings$convergence$max.em.its + 30,
                    data = gsp_out$meta,
                    model = gsp_model)
   saveRDS(gsp_model, paste0("data_temp/gsp_partial"))
}


saveRDS(gsp_model, file = paste0("data/output_large_files/mdl/","model_",format(Sys.time(), "%Y%m%d-%H:%M")))


   
