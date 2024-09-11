library(dotenv)
library(ggraph)
library(igraph)
library(tidyverse)
library(migraph)
library(data.table)

load_dot_env()

network_fp <- paste0(Sys.getenv("BOX_PATH"),
                     "/Supernetwork_Paper/cleaned_unfiltered_extracts")
extract_list <- list.files(network_fp)

# path to page-level data
pages_fp <- paste0(Sys.getenv("BOX_PATH"), 
                   "/Structural_Topic_Model_Paper/gsp_docs_lean")
page_features <- tibble(readRDS(pages_fp))

all_places <- read.csv('EJ_DAC_Paper/Data/all_places.csv')

gsp_ids <- gsub("^0+", "", gsub("\\.RDS", "", extract_list))
gsa_gsp <- tibble(read.csv('EJ_DAC_Paper/Data/gsa_gsp.csv'))
gsa_names <- read.csv('EJ_DAC_Paper/Data/gsa_names.csv')

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
                                   '47'
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
                                        'madera_co_groundwater_sustainability_agency'
                                       )
                          )

gsa_names <- rbind(gsa_names, gsa_names_2, gsa_names_3, gsa_names_4)

# function to grab section columns from page_features to bind to edges
parent_loc_to_section <- function(pointer_str){
   # split str after '.pdf' and before '_'
   pointer_str <- strsplit(pointer_str, "_")[[1]][5]
   gsp_id_in <- as.numeric(strsplit(pointer_str, ".pdf")[[1]][1])
   page_num_in <- as.numeric(strsplit(pointer_str, ".pdf")[[1]][2])
   
   page_sections <- page_features %>% 
      mutate(gsp_id = as.numeric(gsp_id)) %>%
      filter(gsp_id == gsp_id_in) %>% 
      filter(page_num == page_num_in) %>% 
      select(gsp_id, page_num, is_comment, is_reference, 
             admin, basin_plan, sust_criteria, 
             monitoring_networks, projects_mgmt_actions) %>% 
      mutate(admin = as.numeric(admin),
             basin_plan = as.numeric(basin_plan),
             sust_criteria = as.numeric(sust_criteria),
             monitoring_networks = as.numeric(monitoring_networks),
             projects_mgmt_actions = as.numeric(projects_mgmt_actions))
   return(page_sections)
}

# process node/edgelist for use
net_process <- function(file, gsp_id){
   # read in rds file
   temp <- readRDS(file)
   # grab nodelist
   nl <- tibble(temp$nodelist)
   # tag places and DACs
   all_places <- all_places %>% filter(GSP_ID == gsp_id )
   nl <- nl %>% left_join(all_places, by=join_by(entity_name == NAME20))

   el <- tibble(temp$edgelist)
   edge_sources <- data.frame()
   for (edge in el$doc_sent_parent) {
      edge_source <- parent_loc_to_section(edge)
      edge_sources <- rbind(edge_sources, edge_source)
   }
   el <- cbind(el, edge_sources)

   # filter out NA edges and irrelevant cols
   el <- el %>% 
      select(-doc_sent_verb) %>% 
      filter(!is.na(source) & !is.na(target))
   
   networklist <- list("nodelist" = tibble(nl), "edgelist" = tibble(el))

   return(networklist)
}

# process node/edgelist to igraph object
net_graph <- function(networklist, gsp_id, aggregate = FALSE){
   
   network_graph <- igraph::graph_from_data_frame(networklist$edgelist,
                                                  vertices = networklist$nodelist)
   
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
                                    value = leader_dist_min)
   
   network_graph <- set_vertex_attr(network_graph,
                                         'GSA',
                                         value = ifelse(V(network_graph)$name %in% gsa_names2, 1, 0))
   
   network_graph <- set_vertex_attr(network_graph,
                                    'is_place',
                                    value = ifelse(is.na(V(network_graph)$GEOID20), 0, 1))
   
   network_graph_simp <- igraph::simplify(network_graph, 
                                          remove.multiple = TRUE,
                                          remove.loops = FALSE,
                                          edge.attr.comb = list(neg = 'mean',
                                                                has_hedge = 'mean',
                                                                is_future = 'mean',
                                                                gsp_id = 'mean',
                                                                page_num = 'concat',
                                                                admin = 'sum',
                                                                basin_plan = 'sum',
                                                                sust_criteria = 'sum',
                                                                monitoring_networks = 'sum',
                                                                projects_mgmt_actions = 'sum',
                                                                'ignore'))
   
   V(network_graph_simp)$admin_sum <- sapply(V(network_graph_simp), function(v) {
      sum(E(network_graph_simp)[incident(network_graph_simp, v, mode = "all")]$admin)
   })
   V(network_graph_simp)$basin_plan_sum <- sapply(V(network_graph_simp), function(v) {
      sum(E(network_graph_simp)[incident(network_graph_simp, v, mode = "all")]$basin_plan)
   })
   V(network_graph_simp)$sust_criteria_sum <- sapply(V(network_graph_simp), function(v) {
      sum(E(network_graph_simp)[incident(network_graph_simp, v, mode = "all")]$sust_criteria)
   })
   V(network_graph_simp)$monitoring_networks_sum <- sapply(V(network_graph_simp), function(v) {
      sum(E(network_graph_simp)[incident(network_graph_simp, v, mode = "all")]$monitoring_networks)
   })
   V(network_graph_simp)$projects_mgmt_actions_sum <- sapply(V(network_graph_simp), function(v) {
      sum(E(network_graph_simp)[incident(network_graph_simp, v, mode = "all")]$projects_mgmt_actions)
   })
   
   
   network_graph_simp <- set_vertex_attr(network_graph_simp, 
                                    'degree',
                                    value = node_degree(network_graph_simp))
   network_graph_simp <- set_vertex_attr(network_graph_simp, 
                                    'closeness',
                                    value = node_closeness(network_graph_simp))
   network_graph_simp <- set_vertex_attr(network_graph_simp, 
                                    'betweeness',
                                    value = node_betweenness(network_graph_simp))
   network_graph_simp <- set_vertex_attr(network_graph_simp, 
                                    'eigenvector',
                                    value = node_eigenvector(network_graph_simp))  
   network_graph_simp <- set_vertex_attr(network_graph_simp,
                                    'indegree',
                                    value = node_indegree(network_graph_simp))
   network_graph_simp <- set_vertex_attr(network_graph_simp,
                                    'outdegree',
                                    value = node_outdegree(network_graph_simp))
   
   return(network_graph_simp)
}

# apply functions to all networks
for (g in seq_along(gsp_ids)) {
   gsp_id <- paste0("gsp_",gsp_ids[g])
   
   print(paste0("Processing ", gsp_id))
   gsp_list <- net_process(file = paste0(network_fp, 
                                         "/",
                                         extract_list[g]),
                           gsp_id = gsp_ids[g]
                           )
   
   print(paste0("Graphing ", gsp_id))
   gsp_graph <- net_graph(gsp_list, 
                          gsp_id = gsp_ids[g])
   
   print(paste0("Saving Graph ", gsp_id))
   saveRDS(object = gsp_graph,
           file = paste0(Sys.getenv("BOX_PATH"),
                         "/EJ_Paper/cleaned_extracts_DACified",
                         "/",
                         extract_list[g]))

   print(paste0("Finished ", gsp_id))
}

# test functions for one network

glt <- net_process(file = paste0(network_fp, "/",extract_list[92]),
                             gsp_id = gsp_ids[92])

ggt <- net_graph(glt,
                 gsp_id = gsp_ids[92])
