library(dotenv)
library(ggraph)
library(igraph)
library(tidyverse)
library(migraph)
library(data.table)

load_dot_env()

# Side quest script to identify which expected places are found in each GSP

# path to existing network objects

network_fp <- paste0(Sys.getenv("BOX_PATH"), 
                     "/Supernetwork_Paper/cleaned_unfiltered_extracts")
extract_list <- list.files(network_fp) 
extract_list <- setdiff(extract_list, c('0053.RDS', '0089.RDS'))


processed_nets <- paste0(Sys.getenv("BOX_PATH"),
                     "/EJ_Paper/processed_networks")

# places and expected places
places_raw <- read.csv('EJ_DAC_Paper/Data/places.csv')
expected_places <- read_csv("EJ_DAC_Paper/Data/expected_places.csv")
census <- read_csv("EJ_DAC_Paper/Data/ACS_2020_DP05/ACS_2020_DP05-Data.csv")
census_latino <- census %>% 
   select(GEO_ID,DP05_0071PE) %>% 
   # last 6 digits of geoid
   mutate(geoid = as.integer(substr(GEO_ID, 10, 16))) %>% 
   select(-GEO_ID) %>% 
   rename(per_latino = DP05_0071PE)

gsp_ids <- gsub("^0+", "", gsub("\\.RDS", "", extract_list))
gsp_ids2 <- gsub("^0+", "", gsub("\\.RDS", "", proc_net_list))

places <- list()

for (g in seq_along(gsp_ids)) {
   # read in GSP network RDS
   gsp_list <- tibble(readRDS(paste0(network_fp, "/", extract_list[g]))$nodelist)
   
   processed_gsps <- readRDS(paste0(processed_nets, "/", extract_list[g]))
   
   processed_gsps <- set_vertex_attr(processed_gsps,
                                    'degree',
                                    value = node_deg(processed_gsps))
   
   gsp_list_2 <- igraph::as_data_frame(processed_gsps, what = "vertices") %>% 
      select(name, degree) %>% 
      filter(degree > 0) %>% 
      as_tibble()
   
   gsp_id <- paste0("gsp_",gsp_ids[g])
   # save to list places
   places[[gsp_id]] <- expected_places %>%
      # only keep places for this GSP ID
      filter(GSP_ID == gsp_ids[g]) %>%
      # add MHI, POP, DAC, coords
      left_join(places_raw, by = c('GEOID20' = 'geoid')) %>%
      # add percent latino
      left_join(census_latino, by = c('GEOID20' = 'geoid')) %>%
      # add places showing up in network
      left_join(gsp_list, by = c("NAME20" = "entity_name")) %>%
      # places showing up in connected network
      left_join(gsp_list_2, by = c("NAME20" = "name")) %>% 
      # binary exists column based on if expected place is in network
      mutate(exists = ifelse(is.na(entity_type), 0, 1),
             exists2 = ifelse(is.na(degree), 0, 1),
             per_latino = as.numeric(per_latino) ) %>%
      # remove network info
      select(-c('entity_type', 'degree', 'num_appearances')) %>%
      select(-GSP_ID, GSP_ID)
}

all_places <- bind_rows(places)

saveRDS(places, "EJ_DAC_Paper/Data/place_existance.RDS")
write_csv(all_places, "EJ_DAC_Paper/Data/all_places.csv")
