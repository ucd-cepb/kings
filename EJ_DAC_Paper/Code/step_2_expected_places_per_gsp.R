# step_2_expected_places_per_gsp.R
# Builds the community-plan frame: one row per Census place expected in each GSP,
# with DAC status, MHI, population, incorporation, and percent Latino.
# Inclusion is measured in step_4 on the final networks.
# Run from the project root (kings.Rproj).

library(tidyverse)

data_dir <- "EJ_DAC_Paper/Data/"

# places and expected places
places_raw <- read.csv(paste0(data_dir, "places.csv"))
expected_places <- read_csv(paste0(data_dir, "expected_places.csv"))
census <- read_csv("EJ_DAC_Paper/Data/ACS_2020_DP05/ACS_2020_DP05-Data.csv")
census_latino <- census %>%
   select(GEO_ID,DP05_0071PE) %>%
   # last 6 digits of geoid
   mutate(geoid = as.integer(substr(GEO_ID, 10, 16))) %>%
   select(-GEO_ID) %>%
   rename(per_latino = DP05_0071PE)

all_places <- expected_places %>%
   # add MHI, POP, DAC, coords
   left_join(places_raw %>% select(-place_name), by = c('GEOID20' = 'geoid')) %>%
   # add percent latino
   left_join(census_latino, by = c('GEOID20' = 'geoid')) %>%
   mutate(per_latino = as.numeric(per_latino)) %>%
   select(-GSP_ID, GSP_ID)

write_csv(all_places, paste0(data_dir, "all_places.csv"))
