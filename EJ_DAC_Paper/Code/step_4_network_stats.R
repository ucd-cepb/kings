# step_4_network_stats.R
# Calculates node-level statistics for networks.
# Unified script: set network_mode to select input/output directories.
# Same statistics calculated regardless of mode.

# ============================================================================
# CONFIGURATION - Must match the mode used in step_3
# ============================================================================

network_mode <- "tagged"   # Options: "original", "tagged", "agency"

# ============================================================================

library(dotenv)
library(ggraph)
library(igraph)
library(tidyverse)
library(migraph)
library(quanteda)

load_dot_env()

# --- Directory mapping ---
in_dirs <- list(
   original = "/EJ_Paper/processed_networks/",
   tagged   = "/EJ_Paper/processed_networks_tagged/",
   agency   = "/EJ_Paper/processed_networks_agency/"
)
out_dirs <- list(
   original = "/EJ_Paper/cleaned_extracts_2026/",
   tagged   = "/EJ_Paper/cleaned_extracts_tagged/",
   agency   = "/EJ_Paper/cleaned_extracts_agency/"
)

network_fp <- paste0(Sys.getenv("BOX_PATH"), in_dirs[[network_mode]])
out_dir <- paste0(Sys.getenv("BOX_PATH"), out_dirs[[network_mode]])
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

cat("Running in mode:", network_mode, "\n")
cat("Input directory:", network_fp, "\n")
cat("Output directory:", out_dir, "\n")

extract_list <- list.files(network_fp)
gsp_ids <- as.numeric(gsub("^0+", "", gsub("\\.RDS", "", extract_list)))
gsa_gsp <- tibble(read.csv('EJ_DAC_Paper/Data/gsa_gsp.csv'))
gsa_names <- read.csv('EJ_DAC_Paper/Data/gsa_names.csv')

clean_gsa_names <- function(gsa_names) {
   gsa_names_2 <- gsa_names
   gsa_names_2$GSA_Name <- str_replace(gsa_names$GSA_Name,
                                       "groundwater_sustainability_agency",
                                       "gsa")

   gsa_names_3 <- gsa_names %>%
      mutate(GSA_Name = str_replace(gsa_names$GSA_Name,
                                    "_groundwater_sustainability_agency",
                                    "")) %>%
      filter(grepl("groundwater", GSA_Name))

   gsa_names_4 <- data.frame(GSA_ID=c('147','461','457','253','456','106',
                                       '415','418','384','432','163','433',
                                       '434','49','24','403','47','422',
                                       '124','420','419','349'),
                              GSA_Name = c('sacramento_central_groundwater_authority',
                                           'salinas_valley_basin_groundwater_sustainability_agency',
                                           'siskiyou_county_flood_control_and_water_conservation_district_groundwater_sustainability_agency_butte_valley',
                                           'siskiyou_county_flood_control_and_water_conservation_district_groundwater_sustainability_agency_scott_river',
                                           'siskiyou_county_flood_control_and_water_conservation_district_groundwater_sustainability_agency_shasta',
                                           'yuba_water_agency',
                                           'yuba_water_agency',
                                           'tehama_county_flood_control_and_water_conservation_district',
                                           'reclamation_district_no_501_groundwater_sustainability_agency_northern_delta_groundwater_sustainability_agency',
                                           'fox_canyon_groundwater_management_agency',
                                           'fox_canyon_groundwater_management_agency',
                                           'fox_canyon_groundwater_management_agency',
                                           'fox_canyon_groundwater_management_agency',
                                           'arroyo_santa_rosa_groundwater_sustainability_agency',
                                           'mga',
                                           'owens_valley_groundwater_authority',
                                           'madera_co_groundwater_sustainability_agency',
                                           'tehama_county_flood_control_and_water_conservation_district_groundwater_sustainability_agency_antelope',
                                           'tehama_county_flood_control_and_water_conservation_district_groundwater_sustainability_agency_bowman',
                                           'tehama_county_flood_control_and_water_conservation_district_groundwater_sustainability_agency_los_molinos',
                                           'tehama_county_flood_control_and_water_conservation_district_groundwater_sustainability_agency_red_bluff',
                                           'yucaipa_groundwater_sustainability_agency'))

   gsa_names <- rbind(gsa_names, gsa_names_2, gsa_names_3, gsa_names_4)
   return(gsa_names)
}

gsa_names <- clean_gsa_names(gsa_names)

net_stats <- function(network_graph, gsp_id) {

   # get gsa names from gsa_gsp and gsa_names
   gsas <- gsa_gsp %>% filter(GSP_ID == gsp_id)
   gsa_ids <- as.integer(unlist(strsplit(gsas$GSA_IDs, ",")))
   gsa_names2 <- merge(data.frame(GSA_ID = gsa_ids),
                       gsa_names,
                       by = "GSA_ID")$GSA_Name
   gsa_names2 <- c(gsa_names2, 'groundwater_sustainability_agency', 'gsa')
   gsa_ins <- c(which(V(network_graph)$name %in% gsa_names2))

   #distance to gsa(s) - undirected w/ ceiling
   dists <- data.frame(matrix(ncol = length(gsa_ins),
                              nrow = vcount(network_graph)))
   colnames(dists) <- paste0('X', gsa_ins)

   for (i in gsa_ins){
      dist <- distances(network_graph,
                        to=i,
                        mode = 'all'
                        )
      dists[[paste0('X', i)]] <- dist
   }

   colnames(dists) <- V(network_graph)$name[gsa_ins]
   
   leader_dist_min <- apply(dists, 1, min, na.rm = TRUE)
   
   leader_dist_nona <- ifelse(is.infinite(leader_dist_min),
                              diameter(network_graph),
                               leader_dist_min)

   network_graph <- set_vertex_attr(network_graph,
                                    'leader_dist_nona',
                                    value = leader_dist_nona)

   #distance to gsa(s) - UNWEIGHTED, UNDIRECTED, no ceiling
   dists_uw <- data.frame(matrix(ncol = length(gsa_ins),
                                 nrow = vcount(network_graph)))
   colnames(dists_uw) <- paste0('X', gsa_ins)

   for (i in gsa_ins){
      d <- distances(network_graph,
                     to = i,
                     mode = 'all')
      d[is.infinite(d)] <- NA
      d[is.nan(d)] <- NA
      dists_uw[[paste0('X', i)]] <- d
   }

   colnames(dists_uw) <- V(network_graph)$name[gsa_ins]
   leader_dist_min <- apply(dists_uw, 1, min, na.rm = TRUE)

   # Reachable flag: 1 if finite path to any GSA exists, 0 otherwise
   reachable <- ifelse(is.infinite(leader_dist_min), 0, 1)

   # leader_dist: raw shortest path distance (NA if unreachable)
   leader_dist <- ifelse(is.infinite(leader_dist_min), NA, leader_dist_min)

   network_graph <- set_vertex_attr(network_graph,
                                    'reachable',
                                    value = reachable)

   network_graph <- set_vertex_attr(network_graph,
                                    'leader_dist',
                                    value = leader_dist)

   network_graph <- set_vertex_attr(network_graph,
                                    'GSA',
                                    value = ifelse(V(network_graph)$name %in% gsa_names2, 1, 0))

   network_graph <- set_vertex_attr(network_graph,
                                    'is_place',
                                    value = ifelse(is.na(V(network_graph)$GEOID20), 0, 1))

   network_graph <- set_vertex_attr(network_graph,
                                    'in',
                                    value = node_indegree(network_graph,
                                                          normalized=FALSE))

   in_w <- node_indegree(network_graph,
                      normalized=FALSE,
                      alpha=1)
   in_w <- ifelse(is.nan(in_w), 0, in_w)

   network_graph <- set_vertex_attr(network_graph,
                                    'in_w',
                                    value = in_w)

   network_graph <- set_vertex_attr(network_graph,
                                    'out',
                                    value = node_outdegree(network_graph,
                                                           normalized=FALSE))

   out_w <- node_outdegree(network_graph,
                      normalized=FALSE,
                      alpha=1)
   out_w <- ifelse(is.nan(out_w), 0, out_w)

   network_graph <- set_vertex_attr(network_graph,
                                    'out_w',
                                    value = out_w)

   network_graph <- set_vertex_attr(network_graph,
                                    'deg',
                                    value = node_deg(network_graph,
                                                     direction = 'all'))
   deg_w <- node_deg(network_graph,
                      direction = 'all',
                      alpha=1)
   deg_w <- ifelse(is.nan(deg_w), 0, deg_w)

   network_graph <- set_vertex_attr(network_graph,
                                    'deg_w',
                                    value = deg_w)

   return(network_graph)
}

# --- Process stats for each network ---
for (g in seq_along(gsp_ids)) {
   gsp_id <- gsp_ids[g]

   graph_stats <- net_stats(network_graph = readRDS(paste0(network_fp,
                                                           "/",
                                                           extract_list[g])),
                            gsp_id = gsp_ids[g])

   saveRDS(object = graph_stats,
           file = paste0(out_dir, extract_list[g]))

   print(paste0("Finished ", gsp_id))
}

# --- Test ---
# neti <- 15
# net <- net_stats(network_graph = readRDS(paste0(network_fp, "/",extract_list[neti])),
#                    gsp_id = gsp_ids[neti])
# df <- tibble(igraph::as_data_frame(net, what='vertices'))
