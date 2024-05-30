packs <- c('ggplot2','cowplot','furrr','future','stm','tidyverse','stringr')
need <- packs[!packs %in% installed.packages()[,'Package']]
if(length(need)>0){install.packages(need)}
check <- packs[!packs %in% installed.packages()[,'Package']]
if(length(check>0)){print(check)}else{print("All packages ready")}
lapply(packs, require, character.only = TRUE)

###NOTE
###Before running this file, move any existing k files to the archived comparison runs subfolder
#Otherwise the program will see existing files and not overwrite them

filekey <- read.csv("filekey.csv")

gspoutfilename <- filekey[filekey$var_name=="gsp_out_files",]$filepath
gspoutfilenamesplits <- unlist(strsplit(gspoutfilename,split="/"))
gspoutpath <- paste(gspoutfilenamesplits[1:(length(gspoutfilenamesplits)-1)],collapse = "/")
gspoutpattern <- gspoutfilenamesplits[length(gspoutfilenamesplits)]

finfo <- file.info(list.files(path = gspoutpath, pattern = gspoutpattern, full.names = T))
version_select <- which(finfo$mtime==max(finfo$mtime))

obj <- readRDS(rownames(finfo)[version_select])

counter <- 1

obj$meta$priority_category <- as.factor(obj$meta$priority_category)
kdiagfilename <- filekey[filekey$var_name=="k_diag_files_stmpaper",]$filepath
kdiagfilenamesplits <- unlist(strsplit(kdiagfilename,split="/"))
kdiagpath <- paste(kdiagfilenamesplits[1:(length(kdiagfilenamesplits)-1)],collapse = "/")
kdiagpattern <- kdiagfilenamesplits[length(kdiagfilenamesplits)]

#when running

#set with a higher convergence tolerance to speed estimates for the appropriate number of topics
while(counter <= 14){
   print(counter)
   k_saves <- list.files(path = kdiagpath, pattern = kdiagpattern, full.names = T)
   # prepare regular expression
   dig_regex <- "[[:digit:]]+"
   #checks for previously saved models
   saved_nums <- str_extract_all(paste0(k_saves, collapse = "_"), dig_regex)[[1]]
   
   k_choice <- case_when(!"5" %in% saved_nums ~ 5,
                         !"10" %in% saved_nums ~ 10,
                         !"20" %in% saved_nums ~ 20,
                         !"30" %in% saved_nums ~ 30,
                         !"40" %in% saved_nums ~ 40,
                         !"50" %in% saved_nums ~ 50,
                         !"60" %in% saved_nums ~ 60,
                         !"70" %in% saved_nums ~ 70,
                         !"80" %in% saved_nums ~ 80,
                         !"100" %in% saved_nums ~ 100,
                         !"120" %in% saved_nums ~ 120,
                         !"160" %in% saved_nums ~ 160,
                         !"200" %in% saved_nums ~ 200,
                         T ~ 0)
   
   k_str <- toString(k_choice)
   print(paste0("Now generating model with ",k_str, " topics"))
   
   if(file.exists(filekey[filekey$var_name=="heldout",]$filepath)) {
      ##### this used to just say readRDS("heldout") which didnt' work###
      heldout <- readRDS(filekey[filekey$var_name=="heldout",]$filepath)
   }else{
      heldout <- make.heldout(obj$documents,
                              obj$vocab, 
                              N=floor(0.05 * length(obj$documents)), 
                              proportion=0.5, 
                              seed=12)
      saveRDS(heldout,file = filekey[filekey$var_name=="heldout",]$filepath)
   }
   
   if(!is.na(k_choice)){
      set.seed(43278)
      k_model = stm(heldout$documents, heldout$vocab, 
                    K = k_choice,
                    prevalence =~ admin + 
                       basin_plan +
                       sust_criteria +
                       monitoring_networks + 
                       projects_mgmt_actions + 
                       mult_gsas +
                       priority_category +
                       dsci_scaled +
                       basin_population_log_scaled +
                       percent_dac_by_pop_scaled +
                       log_well_MCL_exceedance_count_by_log_pop_scaled +
                       #drywells were included in the May 2024 run
                       #log_drywell_per_log_person_scaled +
                       fract_of_area_in_habitat_log_scaled +
                       Agr_Share_Of_GDP_scaled +
                       Republican_Vote_Share_scaled,
                    init.type = "Spectral",
                    max.em.its = 30,
                    emtol = 0.001,
                    data = obj$meta)
      saveRDS(k_model, file = paste0(filekey[filekey$var_name=="k_incomplete_modelfits_stmpaper",]$filepath,k_str))
      while (!k_model$convergence$converged){
         k_model <- stm(heldout$documents, heldout$vocab, 
                        K = k_choice,
                        prevalence =~ admin + 
                           basin_plan +
                           sust_criteria +
                           monitoring_networks + 
                           projects_mgmt_actions + 
                           mult_gsas +         
                           priority_category + 
                           dsci_scaled +
                           basin_population_log_scaled +
                           percent_dac_by_pop_scaled+
                           log_well_MCL_exceedance_count_by_log_pop_scaled +
                           log_drywell_per_log_person_scaled +
                           fract_of_area_in_habitat_log_scaled +
                           Agr_Share_Of_GDP_scaled +
                           Republican_Vote_Share_scaled,
                        init.type = "Spectral",
                        max.em.its = k_model$settings$convergence$max.em.its + 20,
                        emtol = 0.001,
                        data = obj$meta,
                        model = k_model)
         saveRDS(k_model, file = paste0(filekey[filekey$var_name=="k_incomplete_modelfits_stmpaper",]$filepath,k_str))
      }
      saveRDS(k_model, file = paste0(filekey[filekey$var_name=="k_fitted_models_stmpaper",]$filepath,k_str))
      
      
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
      saveRDS(k_diag, file = paste0(filekey[filekey$var_name=="k_diag_files_stmpaper",]$filepath,k_str))
   }
   
   counter <- counter+1
}



