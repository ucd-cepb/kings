filekey <- read.csv("filekey.csv")

list_of_network_files <- list.files(filekey[filekey$var_name=="disambiged_unfiltered_extracts_instrumentpaper",]$filepath)

network_files <- lapply(paste0(filekey[filekey$var_name=="disambiged_extracts_govnetpaper",]$filepath,"/",list_of_network_files),
                               function (i) readRDS(i))

gsp_meta <- read.csv(filekey[filekey$var_name == "gsp_planwise_metadata_csv",]$filepath)

policies <- read.csv(filekey[filekey$var_name=="bruno_data",]$filepath) 
