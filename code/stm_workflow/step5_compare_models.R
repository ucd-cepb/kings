packs <- c('ggplot2','cowplot','furrr','future','stm','tidyverse','stringr')
need <- packs[!packs %in% installed.packages()[,'Package']]
if(length(need)>0){install.packages(need)}
check <- packs[!packs %in% installed.packages()[,'Package']]
if(length(check>0)){print(check)}else{print("got 'em all")}
lapply(packs, require, character.only = TRUE)

obj <- readRDS(list.files(path = "data/temp_large_files", pattern = "slam", full.names = T)[length(
   list.files(path = "data/temp_large_files", pattern = "slam", full.names = T))])

k_saves <- list.files(path = "data_temp", pattern = "k_diag", full.names = T)
# prepare regular expression
dig_regex <- "[[:digit:]]+"
#checks for previously saved models
saved_nums <- str_extract_all(paste0(k_saves, collapse = "_"), dig_regex)[[1]]

k_choice <- case_when(!"5" %in% saved_nums ~ 5,
                      !"10" %in% saved_nums ~ 10,
                      !"20" %in% saved_nums ~ 20,
                      !"40" %in% saved_nums ~ 40,
                      !"80" %in% saved_nums ~ 80,
                      !"140" %in% saved_nums ~ 140,
                      !"200" %in% saved_nums ~ 200,
                      T ~ NA_real_)

k_str <- toString(k_choice)
print(paste0("Now generating model with ",k_str, " topics"))

if(file.exists("data/temp_large_files/heldout")) {
   heldout <- readRDS("data/temp_large_files/heldout")
}else{
   heldout <- make.heldout(obj$documents,
                           obj$vocab, 
                           N=floor(0.05 * length(obj$documents)), 
                           proportion=0.5, 
                           seed=12)
   saveRDS(heldout,file = "data/temp_large_files/heldout")
}


k_model = stm(heldout$documents, heldout$vocab, 
                        K = k_choice,
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
                           mult_gsas +
                           gwsum,
                        init.type = "Spectral",
                        max.em.its = 30,
                        data = obj$meta)
saveRDS(k_model, paste0("data_temp/k_partial_",k_str))
while (!k_model$convergence$converged){
   k_model <- stm(heldout$documents, heldout$vocab, 
           K = k_choice,
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
              mult_gsas +
              gwsum,
           init.type = "Spectral",
           max.em.its = k_model$settings$convergence$max.em.its + 20,
           data = obj$meta,
           model = k_model)
   saveRDS(k_model, paste0("data_temp/k_partial_",k_str))
}
saveRDS(k_model, file = paste0("data_temp/k_model_",k_str))


k_diag <- tibble(K = k_choice) %>%
   mutate(exclusivity = mean(exclusivity(k_model)),
          semantic_coherence = mean(semanticCoherence(k_model, heldout$documents)),
          eval_heldout = eval.heldout(k_model, heldout$missing)$expected.heldout,
          residual = checkResiduals(k_model, heldout$documents)$dispersion,
          bound =  max(k_model$convergence$bound),
          lfact = lfactorial(k_model$settings$dim$K),
          lbound = bound + lfact,
          iterations = length(k_model$convergence$bound))
#TODO thank Julia Silge

saveRDS(k_diag, file = paste0("data_temp/k_diag_",k_str))

