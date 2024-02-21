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

gsp_out <- readRDS(list.files(path = gspoutpath, pattern = gspoutpattern, full.names = T)[
   length(list.files(path = gspoutpath, pattern = gspoutpattern, full.names = T))])

#See step5_compare_models script for our process of selecting a value for K

#numTopics = selected_model$settings$dim$K

numTopics = 30

form <- ~ admin + 
   basin_plan +
   sust_criteria +
   monitoring_networks + 
   projects_mgmt_actions + 
   (Agr_Share_Of_GDP_scaled + Republican_Vote_Share_scaled) *
   (urbangw_af_log_scaled +
   percent_dac_by_pop_scaled+
   fract_of_area_in_habitat_log_scaled +
   maxdryspell_scaled) +
   # Perc_Bach_Degree_Over25_scaled +
   # local_govs_per_10k_people_log_scaled +
   mult_gsas +
   gwsum


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
   saveRDS(gsp_model, paste0(filekey[filekey$var_name=="finalmodel_incompletefit_stmpaper",]$filepath))
}


saveRDS(gsp_model, file = paste0(filekey[filekey$var_name=="finalmodelfits_stmpaper",]$filepath,format(Sys.time(), "%Y%m%d-%H:%M")))


   
