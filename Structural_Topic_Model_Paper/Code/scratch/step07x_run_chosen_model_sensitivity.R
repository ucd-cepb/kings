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

formX <- ~ admin + 
   basin_plan +
   sust_criteria +
   monitoring_networks + 
   projects_mgmt_actions + 
   Agr_Share_Of_GDP_scaled*percent_dac_by_pop_scaled+
   # Perc_Bach_Degree_Over25_scaled +
   # local_govs_per_10k_people_log_scaled +
   mult_gsas +
   gwsum

gsp_model2 <- stm(documents = gsp_out$documents, vocab = gsp_out$vocab,
                 K = numTopics, prevalence = formX,
                 max.em.its = 30,
                 data = gsp_out$meta, init.type = "Spectral") 
                               
while (!gsp_model2$convergence$converged){
   gsp_model2 <- stm(documents = gsp_out$documents, vocab = gsp_out$vocab,
                    K = numTopics,
                    prevalence = formX,
                    init.type = "Spectral",
                    ### i don't think this is necessary... better to do in batches of 30?
                    ### while loop will just keep doing up to 30 more as needed
                    ### could set a true true max like "hey if you hit 101, then just stop"
                    #max.em.its = gsp_model2$settings$convergence$max.em.its + 30,
                    data = gsp_out$meta,
                    model = gsp_model2)
  }

saveRDS(gsp_model2, file = paste0(filekey[filekey$var_name=="finalmodelfits_stmpaper",]$filepath,format(Sys.time(), "%Y%m%d-%H:%M")))

