library(sf)
library(tidyverse)
library(terra)
library(googleway)
library(mapsapi)
library(dotenv)

load_dot_env()
api_key <- Sys.getenv("GOOGLE_MAPS_API")
shapefile_fp <- paste0(Sys.getenv("BOX_PATH"),"/EJ_Paper/dac_shapefiles/i16_Census_Place_DisadvantagedCommunities_2020/i16_Census_Place_DisadvantagedCommunities_2020.shp")

census_tracts <- read_sf(shapefile_fp)

# extract centroid coords from shapes designated as DACs
ca_dacs <- vect(census_tracts)
centroids <- centroids(ca_dacs)
coords <- crds(centroids)
dac_df <- data.frame(ca_dacs$GEOID10, ca_dacs$CF, coords[,2], coords[,1]) #recreate as df
colnames(dac_df) <- c('tract', 'county', 'lat', 'lng')

dac_df$place_name_short <- NA
dac_df$place_name_long <- NA
dac_df$place_type <- NA

for (i in 1:nrow(dac_df)){
    loc = c(dac_df[i,3],dac_df[i,4])
    loc_name <- googleway::google_reverse_geocode(location = loc,
                                                  location_type = c('approximate'),
                                                  key = api_key)
    dac_df[i,'place_type'] <- loc_name$results$types[[1]][[1]]
    dac_df[i,'place_name_long'] <- loc_name$results$formatted_address[[1]][[1]]
    dac_df[i,'place_name_short'] <- strsplit(loc_name$results$formatted_address[[1]][[1]], ",")[[1]][1]

    Sys.sleep(0.06) #comply with API request limits
}

write.csv(dac_df, "Data/dacs.csv", row.names = FALSE)

#DAC places: 
#Download data from
#https://data.cnra.ca.gov/dataset/dacs-census

#(either 2018 Places dataset
#https://data.cnra.ca.gov/dataset/dacs-census/resource/b473d0f4-51be-4b40-b647-291f01e2cece

#or 2016 Places dataset)
#https://data.cnra.ca.gov/dataset/dacs-census/resource/f39435b9-a25f-4c9c-bc03-7019acf7ac0e

#DAC neighborhoods: import results from Google Maps API code
#get started at
#https://developers.google.com/maps/faq#usage_apis
