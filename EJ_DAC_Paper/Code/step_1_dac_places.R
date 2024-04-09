library(sf)
library(tidyverse)
library(terra)
library(dotenv)

load_dot_env()
api_key <- Sys.getenv("GOOGLE_MAPS_API")

#places to get cities/towns/etc
place_fp <- paste0(Sys.getenv("BOX_PATH"), "/EJ_Paper/dac_shapefiles/place/pdac20.shp")
places <- project(vect(read_sf(place_fp)), "EPSG:4326") #convert to WGS84 latlng
p_coords <- crds(centroids(places))
places_df <- data.frame(p_coords[,2], 
                        p_coords[,1], 
                        places$DAC20,
                        places$MHI20,
                        places$Pop20,
                        places$NAME20) #recreate as df

colnames(places_df) <- c('lat', 
                         'lng', 
                         'DAC', 
                         'MHI',
                         'POP',
                         'place_name')

locs <- places_df %>% 
   mutate(DAC = ifelse(DAC == 'N', 0, 1),
          MHI = ifelse(MHI == 0, NA, MHI),
          place_name = tolower(place_name),
          place_name = gsub(" ", "_", place_name)) 

write.csv(locs, "EJ_DAC_Paper/Data/locations.csv", row.names = FALSE)
