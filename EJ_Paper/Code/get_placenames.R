library(sf)
library(tidyverse)
library(terra)
library(googleway)
library(mapsapi)
library(dotenv)

load_dot_env()
api_key <- Sys.getenv("GOOGLE_MAPS_API")
shapefile_fp <- "/home/aaron/Documents/WORK/CEPB/data/justice40/usa/usa.shp"
# shapefile from justice40

census_tracts <- read_sf(shapefile_fp)

ca_dacs <- census_tracts %>% filter(SF == 'California', SN_C == 1)
ca_dacs <- vect(ca_dacs)
centroids <- centroids(ca_dacs)
coords <- crds(centroids)
dac_df <- data.frame(ca_dacs$GEOID10, ca_dacs$CF, coords[,2], coords[,1])
colnames(dac_df) <- c('tract', 'county', 'lat', 'lng')

dac_df$place_name_short <- NA
dac_df$place_name_long <- NA
dac_df$place_type <- NA

ddt <- dac_df[1:10,]
test_location <- list()

for (i in seq(1,10)){
    loc = c(ddt[i,3],ddt[i,4])
    loc_name <- googleway::google_reverse_geocode(location = loc,
                                                       location_type = c('approximate'),
                                                       key = api_key)
    ddt[i,'place_type'] <- loc_name$results$types[[1]][[1]]
    ddt[i,'place_name_long'] <- loc_name$results$formatted_address[[1]][[1]]
    ddt[i,'place_name_short'] <- strsplit(loc_name$results$formatted_address[[1]][[1]], ",")[[1]][1]
}