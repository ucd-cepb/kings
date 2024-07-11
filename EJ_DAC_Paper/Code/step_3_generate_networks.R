library(dotenv)
library(ggraph)
library(igraph)
library(tidyverse)
library(migraph)
library(data.table)

load_dot_env()

# path to created network data
# network_fp <- paste0(Sys.getenv("BOX_PATH"), "/EJ_Paper/cleaned_extracts_textgov_paper_version")

network_fp <- paste0(Sys.getenv("BOX_PATH"),
                     "/Supernetwork_Paper/cleaned_unfiltered_extracts")

extract_list <- list.files(network_fp)

# path to page-level data
pages_fp <- paste0(Sys.getenv("BOX_PATH"), 
                   "/Structural_Topic_Model_Paper/gsp_docs_lean")
page_features <- tibble(readRDS(pages_fp))

gsp_ids <- gsub("^0+", "", gsub("\\.RDS", "", extract_list))
all_places <- read.csv('EJ_DAC_Paper/Data/all_places.csv')
gsa_gsp <- read.csv('EJ_DAC_Paper/Data/gsa_gsp.csv')
gsa_names <- read.csv('EJ_DAC_Paper/Data/gsa_names.csv')
gsa_names_2 <- gsa_names
gsa_names_2$GSA_Name <- str_replace(gsa_names$GSA_Name, 
                                    "groundwater_sustainability_agency", 
                                    "gsa")
gsa_names <- rbind(gsa_names, gsa_names_2)

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
net_graph <- function(networklist, gsp_id){
   
   network_graph <- igraph::graph_from_data_frame(networklist$edgelist,
                                                  vertices = networklist$nodelist)
   
   # get gsa names from gsa_gsp and gsa_names
   gsas <- gsa_gsp %>% filter(GSP_ID == gsp_id)
   gsa_ids <- as.integer(unlist(strsplit(gsas$GSA_IDs, ",")))
   gsa_names <- merge(data.frame(GSA_ID = gsa_ids), 
                      gsa_names, 
                      by = "GSA_ID")$GSA_Name
   gsa_names <- c(gsa_names, 'groundwater_sustainability_agency', 'gsa') #add in deafult
   gsa_ins <- c(which(V(network_graph)$name %in% gsa_names))
   
   # if gsa matching fails identify most common node with GSA in name
   if (length(gsa_ins) == 0){
      #df with all possible GSA candidates
      ins <- which(str_detect(networklist$nodelist$entity_name, 
                              'groundwater_sustainability_agency|gsa'))
      gsa_ins <- ins[which.max(networklist$nodelist$num_appearances[ins])]
      gsa_names <- c(gsa_names, networklist$nodelist$entity_name[gsa_ins])
   }
   
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
      
   # calculate leader distance per node
   
   # leader_weights <- V(network_graph)[gsa_ins]$num_appearances
   # leader_weights <- leader_weights / sum(leader_weights)
   # weighted_dists <- (sweep(dists, 2, leader_weights, "*")) 
   # leader_dist_weight <- rowSums(weighted_dists, na.rm = TRUE) 
   # leader_dist_unweight <- rowMeans(dists, na.rm = TRUE)
   leader_dist_min <- apply(dists, 1, min, na.rm = TRUE)
   
   ## ADD IN schematic 
   
   network_graph <- set_vertex_attr(network_graph,
                                    'leader_dist_w',
                                    value = leader_dist_weight)
   network_graph <- set_vertex_attr(network_graph,
                                    'leader_dist_uw',
                                    value = leader_dist_unweight)
   network_graph <- set_vertex_attr(network_graph,
                                    'leader_dist_min',
                                    value = leader_dist_min)
   network_graph <- set_vertex_attr(network_graph, 
                                    'degree',
                                    value = node_degree(network_graph))
   network_graph <- set_vertex_attr(network_graph, 
                                    'closeness',
                                    value = node_closeness(network_graph))
   network_graph <- set_vertex_attr(network_graph, 
                                    'betweeness',
                                    value = node_betweenness(network_graph))
   network_graph <- set_vertex_attr(network_graph, 
                                    'eigenvector',
                                    value = node_eigenvector(network_graph))  
   network_graph <- set_vertex_attr(network_graph,
                                    'indegree',
                                    value = node_indegree(network_graph))
   network_graph <- set_vertex_attr(network_graph,
                                    'outdegree',
                                    value = node_outdegree(network_graph))
   # set binary variable for GSAs if node name is in gsa_names
   network_graph <- set_vertex_attr(network_graph,
                                    'GSA',
                                    value = ifelse(V(network_graph)$name %in% gsa_names, 1, 0))
   
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
   
   print(paste0("Saving ", gsp_id))
   saveRDS(object = gsp_graph,
           file = paste0(Sys.getenv("BOX_PATH"),
                         "/EJ_Paper/cleaned_extracts_DACified",
                         "/",
                         extract_list[g]))
   
   print(paste0("Finished ", gsp_id))
}

# test functions for one network

glt <- net_process(file = paste0(network_fp, "/",extract_list[67]),
                             gsp_id = gsp_ids[67])

ggt <- net_graph(glt,
                 gsp_id = gsp_ids[67])


isolates_test <- which(degree(ggt) == 0)
graph_2_test <- delete.vertices(ggt, isolates_test)

ggraph(graph_2_test, 
       layout = 'igraph',
       algorithm = 'nicely') +
   geom_edge_link(color = "black", 
                  alpha = .5) + 
   geom_node_point(aes(size = num_appearances, colour = exists)) + 
   theme_graph()
