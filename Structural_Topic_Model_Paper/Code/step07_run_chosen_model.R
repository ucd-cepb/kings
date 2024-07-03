packs <- c('stm')
need <- packs[!packs %in% installed.packages()[,'Package']]
if(length(need)>0){install.packages(need)}
check <- packs[!packs %in% installed.packages()[,'Package']]
if(length(check>0)){print(check)}else{print("got 'em all")}
lapply(packs, require, character.only = TRUE)

filekey <- read.csv("filekey.csv")


gspoutfilename <- filekey[filekey$var_name=="gsp_out_files",]$filepath
gspoutfilenamesplits <- unlist(strsplit(gspoutfilename,split="/"))
gspoutpath <- paste(gspoutfilenamesplits[1:(length(gspoutfilenamesplits)-1)],collapse = "/")
gspoutpattern <- gspoutfilenamesplits[length(gspoutfilenamesplits)]

slam_files <- list.files(path = gspoutpath, pattern = gspoutpattern, full.names = T)

newest_slam_file <- slam_files[which.max(file.info(slam_files)$mtime)]
gsp_out <- readRDS(newest_slam_file)
        
                                
#numTopics = selected_model$settings$dim$K

numTopics = 30

form <- ~ admin + 
   basin_plan +
   sust_criteria +
   monitoring_networks + 
   projects_mgmt_actions + 
   mult_gsas +
   priority_category +
   basin_population_log_scaled +
   (Agr_Share_Of_GDP_scaled +
       Republican_Vote_Share_scaled) *
   (log_well_MCL_exceedance_count_by_log_pop_scaled +
       percent_dac_by_pop_scaled +
       fract_of_area_in_habitat_log_scaled +
       dsci_scaled)

#run using R 4.3.0. 
set.seed(80000)
gsp_model <- stm(documents = gsp_out$documents, vocab = gsp_out$vocab,
                 K = numTopics, prevalence = form,
                 max.em.its = 30,
                 data = gsp_out$meta, init.type = "Spectral") 
                               
while (!gsp_model$convergence$converged){
   gsp_model <- stm(documents = gsp_out$documents, vocab = gsp_out$vocab,
                    K = numTopics,
                    prevalence = form,
                    init.type = "Spectral",
                    max.em.its = gsp_model$settings$convergence$max.em.its + 30,
                    data = gsp_out$meta,
                    model = gsp_model)
   saveRDS(gsp_model, paste0(filekey[filekey$var_name=="finalmodel_incompletefit_stmpaper",]$filepath,format(Sys.time(), "%Y%m%d-%H:%M")))
}




saveRDS(gsp_model, file = paste0(filekey[filekey$var_name=="finalmodelfits_stmpaper",]$filepath,format(Sys.time(), "%Y%m%d-%H:%M")))

