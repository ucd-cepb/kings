library(mapsapi)
library(googleway)
library(dotenv)

load_dot_env()
api_key <- Sys.getenv("GOOGLE_MAPS_API")

doc <- mp_geocode(
   addresses = "Oak Park, Sacramento",
   key = api_key,
   quiet = TRUE
)

pnt <- mp_get_points(doc)

places_near_oak_park_latlon <- google_places(search_string = 'community organization',location = as.numeric(pnt$pnt$`1`),radius = 500,key = api_key)
print(places_near_oak_park_latlon$results$name)

### need to flip this 
lng_lat <- as.numeric(pnt$pnt$`1`)
lat_lng <- rev(lng_lat)
## lat/lon to place:
plc <- googleway::google_reverse_geocode(location = lat_lng,key = api_key)
### turns out this neighborhood name is "North Oak Park"...
print(plc$results[7,])
