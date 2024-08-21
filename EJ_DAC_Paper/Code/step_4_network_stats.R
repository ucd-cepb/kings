library(dotenv)
library(ggraph)
library(igraph)
library(tidyverse)
library(migraph)

load_dot_env()

network_fp <- paste0(Sys.getenv("BOX_PATH"),
                     "/EJ_Paper/processed_networks")
extract_list <- list.files(network_fp)

gsp_ids <- as.numeric(gsub("^0+", "", gsub("\\.RDS", "", extract_list)))
gsa_gsp <- tibble(read.csv('EJ_DAC_Paper/Data/gsa_gsp.csv'))
gsa_names <- read.csv('EJ_DAC_Paper/Data/gsa_names.csv')

clean_gsa_names <- function(gsa_names) {
   # replace groundwater_sustainability_agency with gsa
   gsa_names_2 <- gsa_names
   gsa_names_2$GSA_Name <- str_replace(gsa_names$GSA_Name, 
                                       "groundwater_sustainability_agency", 
                                       "gsa")
   
   # add in gsa names with 'groundwater' in them already, removing 'groundwater_sustainability_agency'
   gsa_names_3 <- gsa_names %>% 
      mutate(GSA_Name = str_replace(gsa_names$GSA_Name, 
                                    "_groundwater_sustainability_agency", 
                                    "")) %>% 
      filter(grepl("groundwater", GSA_Name))
   
   gsa_names_4 <- data.frame(GSA_ID=c('147',
                                      '461',
                                      '457',
                                      '253',
                                      '456',
                                      '106',
                                      '415',
                                      '418',
                                      '384',
                                      '432',
                                      '163',
                                      '433',
                                      '434',
                                      '49',
                                      '24',
                                      '403', 
                                      '47',
                                      '422',
                                      '124',
                                      '420', 
                                      '419'
                                      ),
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
                                          'tehama_county_flood_control_and_water_conservation_district_groundwater_sustainability_agency_red_bluff'
                                          )
                             )
   
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
   gsa_names2 <- c(gsa_names2, 'groundwater_sustainability_agency', 'gsa') #add in deafult
   gsa_ins <- c(which(V(network_graph)$name %in% gsa_names2))
   
   #distance to gsa(s)
   dists <- data.frame(matrix(ncol = length(gsa_ins), 
                              nrow = vcount(network_graph)))
   colnames(dists) <- paste0('X', gsa_ins)
   
   for (i in gsa_ins){
      dist <- distances(network_graph, 
                        to=i,
                        mode = 'in')
      dist[is.infinite(dist)] <- NA # replace infinite distance with NA
      dist[is.nan(dist)] <- NA # replace NA with NA
      dists[[paste0('X', i)]] <- dist
   }
   
   colnames(dists) <- V(network_graph)$name[gsa_ins]
   leader_dist_min <- apply(dists, 1, min, na.rm = TRUE)
   
   network_graph <- set_vertex_attr(network_graph,
                                    'leader_dist_min',
                                    value = ifelse(is.infinite(leader_dist_min), NA, leader_dist_min))
   
   network_graph <- set_vertex_attr(network_graph,
                                    'GSA',
                                    value = ifelse(V(network_graph)$name %in% gsa_names2, 1, 0))
   
   network_graph <- set_vertex_attr(network_graph,
                                    'is_place',
                                    value = ifelse(is.na(V(network_graph)$GEOID20), 0, 1))
   
   network_graph <- set_vertex_attr(network_graph, 
                                    'pr',
                                    value = igraph::page_rank(network_graph,
                                                              weights = NA)$vector)  
   
   network_graph <- set_vertex_attr(network_graph,
                                    'pr_w',
                                    value = igraph::page_rank(network_graph,
                                                              weights = V(network_graph)$weight)$vector)
   
   network_graph <- set_vertex_attr(network_graph,
                                    'alpha',
                                    value = node_alpha(network_graph))
   
   network_graph <- set_vertex_attr(network_graph,
                                    'in',
                                    value = node_indegree(network_graph, 
                                                          normalized=FALSE))
   network_graph <- set_vertex_attr(network_graph,
                                    'out',
                                    value = node_outdegree(network_graph,
                                                           normalized=FALSE))
   network_graph <- set_vertex_attr(network_graph,
                                    'deg',
                                    value = node_deg(network_graph, 
                                                     direction = 'all'))
   network_graph <- set_vertex_attr(network_graph,
                                    'eig',
                                    value = node_eigenvector(network_graph))
   
   V(network_graph)$admin_both <- sapply(V(network_graph), function(v) {
      sum(E(network_graph)[incident(network_graph, v, mode = "all")]$admin)
   })
   V(network_graph)$basin_plan_both <- sapply(V(network_graph), function(v) {
      sum(E(network_graph)[incident(network_graph, v, mode = "all")]$basin_plan)
   })
   V(network_graph)$sust_criteria_both <- sapply(V(network_graph), function(v) {
      sum(E(network_graph)[incident(network_graph, v, mode = "all")]$sust_criteria)
   })
   V(network_graph)$monitoring_networks_both <- sapply(V(network_graph), function(v) {
      sum(E(network_graph)[incident(network_graph, v, mode = "all")]$monitoring_networks)
   })
   V(network_graph)$projects_mgmt_actions_both <- sapply(V(network_graph), function(v) {
      sum(E(network_graph)[incident(network_graph, v, mode = "all")]$projects_mgmt_actions)
   })
   
   V(network_graph)$admin_in <- sapply(V(network_graph), function(v) {
      sum(E(network_graph)[incident(network_graph, v, mode = "in")]$admin)
   })
   V(network_graph)$basin_plan_in <- sapply(V(network_graph), function(v) {
      sum(E(network_graph)[incident(network_graph, v, mode = "in")]$basin_plan)
   })
   V(network_graph)$sust_criteria_in <- sapply(V(network_graph), function(v) {
      sum(E(network_graph)[incident(network_graph, v, mode = "in")]$sust_criteria)
   })
   V(network_graph)$monitoring_networks_in <- sapply(V(network_graph), function(v) {
      sum(E(network_graph)[incident(network_graph, v, mode = "in")]$monitoring_networks)
   })
   V(network_graph)$projects_mgmt_actions_in <- sapply(V(network_graph), function(v) {
      sum(E(network_graph)[incident(network_graph, v, mode = "in")]$projects_mgmt_actions)
   })
   
   V(network_graph)$admin_out <- sapply(V(network_graph), function(v) {
      sum(E(network_graph)[incident(network_graph, v, mode = "out")]$admin)
   })
   V(network_graph)$basin_plan_out <- sapply(V(network_graph), function(v) {
      sum(E(network_graph)[incident(network_graph, v, mode = "out")]$basin_plan)
   })
   V(network_graph)$sust_criteria_out <- sapply(V(network_graph), function(v) {
      sum(E(network_graph)[incident(network_graph, v, mode = "out")]$sust_criteria)
   })
   V(network_graph)$monitoring_networks_out <- sapply(V(network_graph), function(v) {
      sum(E(network_graph)[incident(network_graph, v, mode = "out")]$monitoring_networks)
   })
   V(network_graph)$projects_mgmt_actions_out <- sapply(V(network_graph), function(v) {
      sum(E(network_graph)[incident(network_graph, v, mode = "out")]$projects_mgmt_actions)
   })
   
   
   return(network_graph)
}

subnet_stats <- function(network_graph, gsp_id) {
   
   V(network_graph)$admin_both <- sapply(V(network_graph), function(v) {
      sum(E(network_graph)[incident(network_graph, v, mode = "all")]$admin)
   })
   V(network_graph)$basin_plan_both <- sapply(V(network_graph), function(v) {
      sum(E(network_graph)[incident(network_graph, v, mode = "all")]$basin_plan)
   })
   V(network_graph)$sust_criteria_both <- sapply(V(network_graph), function(v) {
      sum(E(network_graph)[incident(network_graph, v, mode = "all")]$sust_criteria)
   })
   V(network_graph)$monitoring_networks_both <- sapply(V(network_graph), function(v) {
      sum(E(network_graph)[incident(network_graph, v, mode = "all")]$monitoring_networks)
   })
   V(network_graph)$projects_mgmt_actions_both <- sapply(V(network_graph), function(v) {
      sum(E(network_graph)[incident(network_graph, v, mode = "all")]$projects_mgmt_actions)
   })
   
   network_graph <- induced_subgraph(network_graph, 
                              which(V(network_graph)$admin_both > 0|
                                       V(network_graph)$sust_criteria_both > 0| 
                                       V(network_graph)$monitoring_networks_both > 0|
                                       V(network_graph)$projects_mgmt_actions_both > 0))
   
   network_graph <- net_stats(network_graph, gsp_id)
   
   return(network_graph)
}

# process stats for each network
for (g in 111:117) {
   gsp_id <- gsp_ids[g]
   
   graph_stats <- net_stats(network_graph = readRDS(paste0(network_fp, 
                                                           "/", 
                                                           extract_list[g])
                                                    ), 
                            gsp_id = gsp_ids[g]
                            )

   # Save graph
   saveRDS(object = graph_stats, 
           file = paste0(Sys.getenv("BOX_PATH"), 
                         "/EJ_Paper/cleaned_extracts_DACified/", 
                         extract_list[g]))
   # Timing for subnet_stats
   
   subgraph_stats <- subnet_stats(network_graph = readRDS(paste0(network_fp, 
                                                                  "/", 
                                                                  extract_list[g])
                                                          ), 
                                  gsp_id = gsp_ids[g]
      )
   
   # Save graph
   saveRDS(object = subgraph_stats, 
           file = paste0(Sys.getenv("BOX_PATH"), 
                         "/EJ_Paper/cleaned_extracts_DACified_substantive/", 
                         extract_list[g]))
   
   print(paste0("Finished ", gsp_id))
   
}

# test 

neti <- 111

net <- net_stats(network_graph = readRDS(paste0(network_fp, "/",extract_list[neti])),
                   gsp_id = gsp_ids[neti])

df <- tibble(igraph::as_data_frame(net, what='vertices')); summary(df)

snet <- subnet_stats(network_graph = readRDS(paste0(network_fp, "/",extract_list[neti])),
                     gsp_id = gsp_ids[neti])

sdf <- tibble(igraph::as_data_frame(snet, what='vertices')); summary(sdf)

