library(sf)
library(tidyverse)
library(terra)
library(googleway)
library(mapsapi)
library(dotenv)

load_dot_env()
api_key <- Sys.getenv("GOOGLE_MAPS_API")

#block groups to extract neighborhoods
block_group_fp <- paste0(Sys.getenv("BOX_PATH"),
                         "/EJ_Paper/dac_shapefiles/block_group/bgdac20.shp")
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
call_google <- readline(prompt = "Call Google Geocode API? (yes/no): ")

if(tolower(call_google) == "yes") {
   for (i in seq_len(nrow(bg_df))){
      loc = c(bg_df[i,1],bg_df[i,2])
      loc_name <- googleway::google_reverse_geocode(location = loc,
                                                    location_type = c('approximate'),
                                                    key = api_key)
      bg_df[i,'place_type'] <- loc_name$results$types[[1]][[1]]
      bg_df[i,'place_name'] <- strsplit(loc_name$results$formatted_address[[1]][[1]], ",")[[1]][1]
      
      Sys.sleep(0.06) #comply with API request limits
   }
}

# filter to only neighborhood place types
neighborhoods_df <- bg_df[bg_df['place_type'] == 'neighborhood',]
# save(neighborhoods_df, file = neighborhood_fp) #save to not have to rerun

# load in neighborhoods_df
neighborhood_fp <- paste0(Sys.getenv("BOX_PATH"),
                          "/EJ_Paper/dac_shapefiles/neighborhoods_all.rda")
load(neighborhood_fp)

neighborhoods_df <- neighborhoods_df %>% 
   filter(DAC != 'Data Not Available')  %>%
   mutate(DAC = ifelse(DAC == 'Y', 1, 0)) %>% 
   group_by(place_name) %>%
   mutate(DAC = ifelse(any(DAC == 1) & any(DAC == 0), round(mean(DAC)), DAC)) %>%
   ungroup()  %>% 
   distinct(place_name, .keep_all = TRUE)

# ASSUMPTION: If there are multiple neighborhoods with the same name (which occurs very often because there are often multiple block groups within a single neighborhood), where some are DACs and some are not, the mean is taken for the neighborhood and rounded to 0 or 1

#places to get cities/towns/etc
place_fp <- paste0(Sys.getenv("BOX_PATH"),
                   "/EJ_Paper/dac_shapefiles/place/pdac20.shp")
places <- read_sf(place_fp)
places <- vect(places)
places <- project(places, "EPSG:4326") #convert to WGS84 latlng
p_centroids <- centroids(places)
p_coords <- crds(p_centroids)
places_df <- data.frame(p_coords[,2], 
                        p_coords[,1], 
                        places$DAC20, 
                        places$NAME20, 
                        'cd_place') #recreate as df

colnames(places_df) <- c('lat', 'lng', 'DAC', 'place_name', 'place_type')

places_df <- places_df %>% 
   filter(DAC != 'Data Not Available') %>%
   #deal with place-place overlap
   group_by(place_name) %>%
   mutate(DAC = ifelse(any(DAC == 'Y'), 1, 0)) %>%
   ungroup()  %>%
   distinct(place_name, .keep_all = TRUE)

# ASSUMPTION: If there are two places with the same name and one is a DAC (as is the case in Bayview, Cottonwood, Greenfield, Las Flores, and Live Oak) it is assumed to be a DAC. 

choose_place <- function(place_name_in){
   templocs <- locs %>% 
      filter(place_name == place_name_in, place_type == 'cd_place')
   return(templocs$DAC)
}

# write to csv
locs <- rbind(neighborhoods_df, places_df)
locs <- locs %>% 
   mutate(place_name = tolower(place_name),
          place_name = gsub(" ", "_", place_name)) %>% 
   group_by(place_name) %>% 
   rowwise() %>% 
   mutate(DAC = ifelse(any(DAC == 1) & any(DAC==0),
                       choose_place(place_name),
                       DAC)) %>% 
   ungroup()  %>%
   distinct(place_name, .keep_all = TRUE)

# ASSUMPTION: If there are a neighborhood and a CDP with the same name (about 130 cases), the CDP takes precedence in DAC value. Thus if they are both the same, it will be correct in any case, but if they are different the neighborhood (which is less likely to show up) will take on the CDP DAC status

write.csv(locs, "EJ_DAC_Paper/Data/locations.csv", row.names = FALSE)
