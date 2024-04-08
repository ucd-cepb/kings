library(tidyverse)

#always load the filekey into the script like this
filekey <- read.csv("filekey.csv")

#how to get a filepath for a var
names <- readRDS(filekey[filekey$var_name=="gsp_docs_meta_stmpaper",]$filepath)

names <- names %>% 
    select(c('gsp_id', 'basin_id')) %>%
    distinct() %>%
    write.csv("EJ_DAC_Paper/Data/gsp_basin_ids.csv", row.names = FALSE)
