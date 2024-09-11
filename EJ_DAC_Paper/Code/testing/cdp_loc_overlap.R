library(tidyverse)

# Read the CSV files
expected_places <- read_csv("EJ_DAC_Paper/Data/expected_places.csv")
locations <- read_csv("EJ_DAC_Paper/Data/locations.csv")

# Evaluate which rows from expected_places do not show up in locations
missing_rows <- expected_places %>% anti_join(locations, by = c("NAME20" = "place_name"))

print(paste0("expected places has ", nrow(expected_places), " rows"))
print(paste0("locations has ", 
             nrow(locations), 
             " rows, ", 
             nrow(locations %>% filter(place_type == "cd_place")), 
             " of which are census designated places"))
print(paste0("expected places has ", nrow(missing_rows), " rows that are not in locations"))

