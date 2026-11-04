# step_1a_dac_places.R
# Census places with DAC status, MHI, population, and incorporation status.
#   DWR "Data Not Available" is coded DAC = NA
#   place_name is kept; step_3 needs every Census place name to rule out false aliases
# Run from the project root (kings.Rproj).

library(sf)
library(tidyverse)
library(terra)
library(dotenv)

load_dot_env()

data_dir <- "EJ_DAC_Paper/Data/"
if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)

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
   mutate(DAC = case_when(DAC == 'N' ~ 0,
                          DAC == 'Y' ~ 1,
                          TRUE ~ NA_real_), # 0 = no DAC, 1 = DAC, NA = data not available
          MHI = ifelse(MHI == 0, NA, MHI),
          geoid = as.integer(geoid),
          incorporated = ifelse(place_type == 25 | place_type == 43, 1, 0), # 1 = incorporated, 0 = unincorporated
          place_name = tolower(place_name),
          place_name = gsub(" ", "_", place_name)) %>%
   select(-place_type)

# place names are not unique (e.g. two "mountain_view"s), so every later join is on geoid
write.csv(places_out, paste0(data_dir, "places.csv"), row.names = FALSE)
