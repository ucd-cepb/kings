# step_4_network_stats.R
# Tags GSA nodes, calculates place-level network measures, and builds the community-plan analysis frame.
#   GSA tags: shapefile GSA names plus their common written forms (generic labels such as "gsa" are not tagged)
#   degree = node strength (in + out, repeated ties summed), loops already removed in step_3
#   leader distance = unweighted, undirected shortest path to any GSA node
#      leader_cat: 1 / 2 / 3+ / unreachable (isolated, or no path to a GSA); NA for places not in the network
#      leader_dist_nona: unreachable set to the unweighted, undirected diameter (robustness only)
#   plan size: main-text pages (no comment or reference pages) and edge count
# Outputs: Data/community_plan.csv, Data/gsa_manual_list.csv
# Run from the project root (kings.Rproj).

library(dotenv)
library(igraph)
library(tidyverse)

load_dot_env()

data_dir <- "EJ_DAC_Paper/Data/"
network_fp <- paste0(Sys.getenv("BOX_PATH"), "/EJ_Paper/processed_networks_final/")
extract_list <- list.files(network_fp)
gsp_ids <- as.numeric(gsub("^0+", "", gsub("\\.RDS", "", extract_list)))

all_places <- read_csv(paste0(data_dir, "all_places.csv"))
gsa_gsp <- read.csv(paste0(data_dir, "gsa_gsp.csv"), colClasses = "character")
gsa_names <- read.csv(paste0(data_dir, "gsa_names.csv"), colClasses = "character")


# ============================================================================
# GSA names: shapefile names plus their common written forms
# ============================================================================

# manual entries for DWR names that never appear verbatim in plans
# (management-area suffixes, names truncated at 64 characters, agencies written without "GSA", acronyms)
gsa_manual <- data.frame(GSA_ID=c('147','461','457','253','456','106',
                                  '418','384','432','163','433',
                                  '434','49','24','403','422',
                                  '124','420','419','349'),
                         GSA_Name = c('sacramento_central_groundwater_authority',
                                      'salinas_valley_basin_groundwater_sustainability_agency',
                                      'siskiyou_county_flood_control_and_water_conservation_district_groundwater_sustainability_agency_butte_valley',
                                      'siskiyou_county_flood_control_and_water_conservation_district_groundwater_sustainability_agency_scott_river',
                                      'siskiyou_county_flood_control_and_water_conservation_district_groundwater_sustainability_agency_shasta',
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
                                      'tehama_county_flood_control_and_water_conservation_district_groundwater_sustainability_agency_antelope',
                                      'tehama_county_flood_control_and_water_conservation_district_groundwater_sustainability_agency_bowman',
                                      'tehama_county_flood_control_and_water_conservation_district_groundwater_sustainability_agency_los_molinos',
                                      'tehama_county_flood_control_and_water_conservation_district_groundwater_sustainability_agency_red_bluff',
                                      'yucaipa_groundwater_sustainability_agency'))

clean_gsa_names <- function(gsa_names) {
   # "..._groundwater_sustainability_agency" -> "..._gsa"
   gsa_names_2 <- gsa_names %>%
      mutate(GSA_Name = str_replace(GSA_Name, "groundwater_sustainability_agency", "gsa"))

   # "..._groundwater_sustainability_agency" -> "..." (kept only if a groundwater body is still named)
   gsa_names_3 <- gsa_names %>%
      mutate(GSA_Name = str_replace(GSA_Name, "_groundwater_sustainability_agency", "")) %>%
      filter(grepl("groundwater", GSA_Name))

   bind_rows(gsa_names, gsa_names_2, gsa_names_3, gsa_manual) %>%
      # basin name produced by the suffix rule, not an agency
      filter(GSA_Name != "montecito_groundwater_basin")
}

gsa_names <- clean_gsa_names(gsa_names)


# ============================================================================
# net_stats: GSA tags and place-level measures
# ============================================================================

net_stats <- function(network_graph, gsp_id) {

   # get gsa names from gsa_gsp and gsa_names
   gsa_ids <- unlist(strsplit(gsa_gsp$GSA_IDs[as.numeric(gsa_gsp$GSP_ID) == gsp_id], ","))
   gsa_names_gsp <- gsa_names %>%
      filter(GSA_ID %in% gsa_ids) %>%
      pull(GSA_Name)

   network_graph <- set_vertex_attr(network_graph,
                                    'GSA',
                                    value = ifelse(V(network_graph)$name %in% gsa_names_gsp, 1, 0))

   network_graph <- set_vertex_attr(network_graph,
                                    'is_place',
                                    value = ifelse(is.na(V(network_graph)$GEOID20), 0, 1))

   # degree: node strength, in + out, repeated ties summed through weight
   network_graph <- set_vertex_attr(network_graph,
                                    'degree',
                                    value = strength(network_graph, mode = 'all'))

   # distance to gsa(s): unweighted, undirected
   gsa_ins <- which(V(network_graph)$GSA == 1)
   leader_dist <- distances(network_graph,
                            to = gsa_ins,
                            mode = 'all',
                            weights = NA) %>%
      apply(1, min)

   network_graph <- set_vertex_attr(network_graph,
                                    'reach',
                                    value = ifelse(is.finite(leader_dist), 1, 0))

   network_graph <- set_vertex_attr(network_graph,
                                    'leader_dist',
                                    value = ifelse(is.finite(leader_dist), leader_dist, NA))

   network_graph <- set_vertex_attr(network_graph,
                                    'leader_dist_nona',
                                    value = ifelse(is.finite(leader_dist),
                                                   leader_dist,
                                                   diameter(network_graph, directed = FALSE, weights = NA)))

   return(network_graph)
}


# ============================================================================
# Plan size
# ============================================================================

pages_fp <- paste0(Sys.getenv("BOX_PATH"),
                   "/Structural_Topic_Model_Paper/gsp_docs_lean")
plan_pages <- tibble(readRDS(pages_fp)) %>%
   mutate(GSP_ID = as.numeric(gsp_id)) %>%
   filter(is_comment == FALSE & is_reference == FALSE) %>%
   count(GSP_ID, name = "pages")


# ============================================================================
# Main loop: measures per network
# ============================================================================

place_nodes <- list()
plan_edges <- list()
manual_tags <- list()

for (g in seq_along(gsp_ids)) {
   graph_stats <- net_stats(network_graph = readRDS(paste0(network_fp, extract_list[g])),
                            gsp_id = gsp_ids[g])

   place_nodes[[g]] <- igraph::as_data_frame(graph_stats, what = "vertices") %>%
      filter(is_place == 1) %>%
      transmute(GSP_ID = gsp_ids[g],
                GEOID20 = as.numeric(GEOID20),
                degree,
                reach,
                leader_dist,
                leader_dist_nona)

   plan_edges[[g]] <- tibble(GSP_ID = gsp_ids[g],
                             edges = ecount(graph_stats),
                             n_gsa = sum(V(graph_stats)$GSA))

   # manual GSA entries that tag a node in this plan
   gsa_ids <- unlist(strsplit(gsa_gsp$GSA_IDs[as.numeric(gsa_gsp$GSP_ID) == gsp_ids[g]], ","))
   manual_tags[[g]] <- gsa_manual %>%
      filter(GSA_ID %in% gsa_ids, GSA_Name %in% V(graph_stats)$name) %>%
      mutate(GSP_ID = gsp_ids[g], .before = 1)

   print(paste0("Finished ", gsp_ids[g]))
}


# ============================================================================
# Community-plan frame
# ============================================================================

community_plan <- all_places %>%
   left_join(bind_rows(place_nodes), by = c("GSP_ID", "GEOID20")) %>%
   left_join(bind_rows(plan_edges), by = "GSP_ID") %>%
   left_join(plan_pages, by = "GSP_ID") %>%
   # network measures stay NA for places not in the network (H2 and H3 use network nodes only)
   mutate(included = ifelse(is.na(degree), 0, 1),
          any_edge = ifelse(degree > 0, 1, 0),
          leader_cat = case_when(reach == 0 ~ "unreachable",
                                 leader_dist == 1 ~ "1",
                                 leader_dist == 2 ~ "2",
                                 leader_dist >= 3 ~ "3+"))

write_csv(community_plan, paste0(data_dir, "community_plan.csv"))

# appendix table: manually tagged GSA nodes, by plan
write_csv(bind_rows(manual_tags), paste0(data_dir, "gsa_manual_list.csv"))
