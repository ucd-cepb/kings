library(dotenv)
library(ggraph)
library(igraph)
library(tidyverse)
library(migraph)
library(data.table)

load_dot_env()

network_fp <- paste0(Sys.getenv("BOX_PATH"), "/EJ_Paper/cleaned_extracts_textgov_paper_version")
extract_list = list.files(network_fp)

locs <- read.csv('EJ_DAC_Paper/Data/locations.csv')
expected_places <- read_csv("EJ_DAC_Paper/Data/expected_places.csv")

gsp_ids <- gsub("^0+", "", gsub("\\.RDS", "", extract_list))

# convenience function for reading in and processing nodes
# extracts have ORG, PERSON, and GPE nodetypes

net_process <- function(file){
   temp <- readRDS(file)
   nl <- temp$nodelist
   return(nl)
}

places <- list()

for (g in seq_along(gsp_ids)) {
    gsp_list <- readRDS(paste0(network_fp, "/", extract_list[g]))$nodelist 
    gsp_id <- paste0("gsp_",gsp_ids[g])
    places[[gsp_id]] <- expected_places %>% 
       filter(GSP_ID == gsp_ids[g]) %>%
       left_join(locs, by = c('NAME20' = 'place_name')) %>% 
       left_join(gsp_list, by = c("NAME20" = "entity_name")) %>%
       mutate(place = 1,
              exists = ifelse(is.na(entity_type), 0, 1)) %>% 
       select(-c('entity_type', 'num_appearances'))
}

all_places <- bind_rows(places)