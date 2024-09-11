library(sf)
library(tidyverse)
library(terra)
library(dotenv)

load_dot_env()

#places to get cities/towns/etc
place_fp <- paste0(Sys.getenv("BOX_PATH"), "/EJ_Paper/dac_shapefiles/place/pdac20.shp")
places <- project(vect(read_sf(place_fp)), "EPSG:4326") #convert to WGS84 latlng
p_coords <- crds(centroids(places))
places_df <- data.frame(p_coords[,2], 
                        p_coords[,1], 
                        places$GEOID20,
                        as.integer(places$LSAD20),
                        places$DAC20,
                        places$MHI20,
                        places$Pop20,
                        places$NAME20) #recreate as df

colnames(places_df) <- c('lat', 
                         'lng', 
                         'geoid',
                         'place_type',
                         'DAC', 
                         'MHI',
                         'POP',
                         'place_name')

places_out <- places_df %>% 
   mutate(DAC = ifelse(DAC == 'N', 0, 1), # 0 = no DAC, 1 OR data not available = DAC
          MHI = ifelse(MHI == 0, NA, MHI), 
          geoid = as.integer(geoid),
          incorporated = ifelse(place_type == 25 | place_type == 43, 1, 0), # 1 = incorporated, 0 = unincorporated
          place_name = tolower(place_name),
          place_name = gsub(" ", "_", place_name)) %>% 
   select(-c(place_name, place_type))
   # distinct(place_name, .keep_all = TRUE) # THIS IS A BAD ASSUMPTION AND NEEDS TO BE FIXED LATER
      
write.csv(places_out, "EJ_DAC_Paper/Data/places.csv", row.names = FALSE)

# ISSUE OF DUPLICATED PLACES NEEDS TO BE FIXED
dups <- duplicated(places_df$place_name) | duplicated(places_df$place_name, fromLast = TRUE)
duplicated_places <- places_df[dups,] %>% arrange(place_name)
duplicated_places
