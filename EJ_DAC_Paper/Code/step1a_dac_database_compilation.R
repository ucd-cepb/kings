library(sf)
library(tidyverse)
library(terra)
library(googleway)
library(mapsapi)
library(dotenv)

load_dot_env()
api_key <- Sys.getenv("GOOGLE_MAPS_API")

#block groups to extract neighborhoods
block_group_fp <- paste0(Sys.getenv("BOX_PATH"),"/EJ_Paper/dac_shapefiles/block_group/bgdac20.shp")
block_groups <- read_sf(block_group_fp)

# extract centroid coords from shapes designated as DACs
bg_dacs <- block_groups %>% filter(DAC20 == 'Y') # DAC20 = Y -> Identified as disadvantaged
bg_dacs <- vect(bg_dacs)
bg_dacs <- project(bg_dacs, "EPSG:4326") #convert to WGS84 latlng
centroids <- centroids(bg_dacs)
coords <- crds(centroids)
bg_dac_df <- data.frame(coords[,2], coords[,1]) #recreate as df
colnames(bg_dac_df) <- c('lat', 'lng')
bg_dac_df$place_name <- NA
bg_dac_df$place_type <- NA
# get placename using google reverse geocode
for (i in seq_len(nrow(bg_dac_df))){
    loc = c(bg_dac_df[i,1],bg_dac_df[i,2])
    loc_name <- googleway::google_reverse_geocode(location = loc,
                                                  location_type = c('approximate'),
                                                  key = api_key)
    bg_dac_df[i,'place_type'] <- loc_name$results$types[[1]][[1]]
    # bg_dac_df[i,'place_name_long'] <- loc_name$results$formatted_address[[1]][[1]]
    bg_dac_df[i,'place_name'] <- strsplit(loc_name$results$formatted_address[[1]][[1]], ",")[[1]][1]

    Sys.sleep(0.06) #comply with API request limits
}
# filter to only neighborhood place types
dac_neighborhoods <- bg_dac_df[bg_dac_df['place_type'] == 'neighborhood',]
dac_neighborhoods <- distinct(dac_neighborhoods, place_name, .keep_all = TRUE)

#places to get cities/towns/etc
place_fp <- paste0(Sys.getenv("BOX_PATH"),"/EJ_Paper/dac_shapefiles/place/pdac20.shp")
places <- read_sf(place_fp)
p_dacs <- places %>% filter(DAC20 == 'Y') # DAC20 = Y -> Identified as disadvantaged
p_dacs <- vect(p_dacs)
p_dacs <- project(p_dacs, "EPSG:4326") #convert to WGS84 latlng
centroids <- centroids(p_dacs)
coords <- crds(centroids)


dac_places <- data.frame(coords[,2], coords[,1], p_dacs$NAME20, 'dac_place') #recreate as df
colnames(dac_places) <- c('lat', 'lng', 'place_name', 'place_type')

dacs <- rbind(dac_neighborhoods, dac_places)

write.csv(dacs, "EJ_Paper/Data/dacs.csv", row.names = FALSE)

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
