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
neighborhood_fp <- paste0(Sys.getenv("BOX_PATH"),"/EJ_Paper/dac_shapefiles/neighborhoods_all.rda")
neighborhoods_df <- bg_df[bg_df['place_type'] == 'neighborhood',]

# save(neighborhoods_df, file = neighborhood_fp) #save to not have to rerun
# load(neighborhood_fp)

neighborhoods_df <- neighborhoods_df %>% 
   filter(DAC != 'Data Not Available')  %>%
   mutate(DAC = ifelse(DAC == 'Y', 1, 0)) %>% 
   group_by(place_name) %>%
   mutate(DAC = ifelse(any(DAC == 1) & any(DAC == 0), 0.5, DAC)) %>%
   ungroup()  %>% 
   distinct(place_name, .keep_all = TRUE)

#places to get cities/towns/etc
place_fp <- paste0(Sys.getenv("BOX_PATH"),"/EJ_Paper/dac_shapefiles/place/pdac20.shp")
places <- read_sf(place_fp)
places <- vect(places)
places <- project(places, "EPSG:4326") #convert to WGS84 latlng
p_centroids <- centroids(places)
p_coords <- crds(p_centroids)
places_df <- data.frame(p_coords[,2], p_coords[,1], places$DAC20, places$NAME20, 'cd_place') #recreate as df
colnames(places_df) <- c('lat', 'lng', 'DAC', 'place_name', 'place_type')
places_df <- places_df %>% 
   filter(DAC != 'Data Not Available')  %>%
   mutate(DAC = ifelse(DAC == 'Y', 1, 0)) 

# neighborhood-place overlaps
neighborhood_place_overlaps <- inner_join(neighborhoods_df, places_df, by = join_by(place_name))%>% arrange(place_name)

# place-place overlaps
multiple_cities <- data.frame(table(places_df$place_name)) %>% filter(Freq >= 2)
colnames(multiple_cities) <- c('place_name', 'freq')
place_place_overlaps <- inner_join(places_df, multiple_cities, by =  join_by(place_name)) %>% arrange(place_name)

# write to csv
locs <- rbind(neighborhoods_df, places_df)
locs$place_name <- tolower(locs$place_name)
locs$place_name <- gsub(" ", "_", locs$place_name)

write.csv(locs, "EJ_Paper/Data/locations.csv", row.names = FALSE)
