# step_1b_merge_shapes.R
# Expected places per GSP: Census places that intersect each GSP boundary.
# Also writes the GSA-per-GSP and GSA name tables, and the GSA map.
# Run from the project root (kings.Rproj).

library(sf)
library(tidyverse)
library(dotenv)

load_dot_env()

data_dir <- "EJ_DAC_Paper/Data/"
out_dir <- "EJ_DAC_Paper/Out/"
for (d in c(data_dir, out_dir)) if (!dir.exists(d)) dir.create(d, recursive = TRUE)

# read in everything
shape_fp <- paste0(Sys.getenv("BOX_PATH"), "/EJ_Paper/dac_shapefiles/")
place_bounds_raw <- read_sf(paste0(shape_fp, "place/pdac20.shp"))
gsp_bounds_raw <- read_sf(paste0(shape_fp, "gsp/gsp.shp"))
gsa_bounds_raw <- read_sf(paste0(shape_fp, "gsa/gsa.shp"))
cal <- read_sf(paste0(shape_fp, "ca/CA_State.shp"))

# GSPs with extracted networks
# 53 and 89 are excluded (as in step_3); 157 was never extracted
network_fp <- paste0(Sys.getenv("BOX_PATH"), "/Supernetwork_Paper/cleaned_unfiltered_extracts")
network_gsps <- as.numeric(gsub("\\.RDS", "", list.files(network_fp)))
network_gsps <- setdiff(network_gsps, c(53, 89))

# select relevent columns
place_bounds <- place_bounds_raw %>%
   select(GEOID20, NAME20)

gsp_bounds <- gsp_bounds_raw %>%
   select(GSP_ID, Basin_Subb) %>%
   filter(GSP_ID %in% network_gsps)

# places that share area with a GSP (places that only touch a boundary are dropped)
intersections <- st_intersection(place_bounds, gsp_bounds) %>%
   filter(st_dimension(geometry) == 2)

expected_places <- intersections %>%
   st_drop_geometry() %>%
   distinct(GSP_ID, NAME20, GEOID20, Basin_Subb) %>%
   mutate(NAME20 = tolower(gsub(" ", "_", NAME20)),
          GEOID20 = as.integer(GEOID20)) %>%
   arrange(GSP_ID)

write_csv(expected_places, paste0(data_dir, "expected_places.csv"))

# map: California, GSPs in light blue, places in vermillion, place-GSP intersections in navy
# (light / medium / dark, so the layers stay distinct in grayscale and for colorblind readers)
gsa_map <- ggplot() +
   geom_sf(data = cal, fill = "white", color = "black", linewidth = 0.3) +
   geom_sf(data = gsp_bounds, aes(fill = "GSPs"), color = NA) +
   geom_sf(data = place_bounds_raw, aes(fill = "Places"), color = NA) +
   geom_sf(data = intersections, aes(fill = "Intersections"), color = NA) +
   scale_fill_manual(values = c("GSPs" = "#C6DBEF",
                                "Places" = "#D55E00",
                                "Intersections" = "#08306B"),
                     breaks = c("GSPs", "Places", "Intersections"),
                     name = NULL) +
   theme_void() +
   theme(legend.position = c(0.85, 0.9))

ggsave(paste0(out_dir, "gsa_over_california.png"), gsa_map, width = 6, height = 7, dpi = 300, bg = "white")

# gsas per gsp
gsp_bounds_raw %>%
   st_drop_geometry() %>%
   select(GSP_ID, GSA_IDs) %>%
   write_csv(paste0(data_dir, "gsa_gsp.csv"))

# gsa names
gsa_names <- gsa_bounds_raw %>%
   st_drop_geometry() %>%
   select(GSA_ID, GSA_Name) %>%
   mutate(GSA_Name = str_replace_all(GSA_Name, fixed("GSA"), "Groundwater Sustainability Agency"),
          GSA_Name = str_replace_all(GSA_Name, "[ -]+", "_"),
          GSA_Name = str_remove_all(GSA_Name, fixed(".")),
          GSA_Name = tolower(GSA_Name))

write_csv(gsa_names, paste0(data_dir, "gsa_names.csv"))
