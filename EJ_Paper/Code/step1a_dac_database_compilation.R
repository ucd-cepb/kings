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
block_groups <- vect(block_groups)
block_groups <- project(block_groups, "EPSG:4326") #convert to WGS84 latlng
centroids <- centroids(block_groups)
coords <- crds(centroids)
bg_df <- data.frame(coords[,2], coords[,1], block_groups$DAC20) #recreate as df
colnames(bg_df) <- c('lat', 'lng', 'DAC')
bg_df$place_name <- NA
bg_df$place_type <- NA

test <- sample(1:25607, 100)

# get placename using google reverse geocode
for (i in seq_len(nrow(bg_df))){
    loc = c(bg_df[i,1],bg_df[i,2])
    loc_name <- googleway::google_reverse_geocode(location = loc,
                                                  location_type = c('approximate'),
                                                  key = api_key)
    bg_df[i,'place_type'] <- loc_name$results$types[[1]][[1]]
    # bg_dac_df[i,'place_name_long'] <- loc_name$results$formatted_address[[1]][[1]]
    bg_df[i,'place_name'] <- strsplit(loc_name$results$formatted_address[[1]][[1]], ",")[[1]][1]

    Sys.sleep(0.06) #comply with API request limits
}
# filter to only neighborhood place types
neighborhoods_df <- bg_df[bg_df['place_type'] == 'neighborhood',]
neighborhoods_unique <- distinct(neighborhoods_df, place_name, DAC, .keep_all = TRUE)

#places to get cities/towns/etc
place_fp <- paste0(Sys.getenv("BOX_PATH"),"/EJ_Paper/dac_shapefiles/place/pdac20.shp")
places <- read_sf(place_fp)
# p_dacs <- places %>% filter(DAC20 == 'Y') # DAC20 = Y -> Identified as disadvantaged
places <- vect(places)
places <- project(places, "EPSG:4326") #convert to WGS84 latlng
p_centroids <- centroids(places)
p_coords <- crds(p_centroids)


places_df <- data.frame(p_coords[,2], p_coords[,1], places$DAC20, places$NAME20, 'dac_place') #recreate as df
colnames(places_df) <- c('lat', 'lng', 'DAC', 'place_name', 'place_type')

dacs <- rbind(neighborhoods_unique, places_df)

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
