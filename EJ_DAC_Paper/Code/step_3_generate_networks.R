library(dotenv)
library(ggraph)
library(igraph)
library(tidyverse)
library(migraph)
library(data.table)

load_dot_env()

network_fp <- paste0(Sys.getenv("BOX_PATH"), 
                     "/EJ_Paper/cleaned_extracts_textgov_paper_version")
extract_list = list.files(network_fp)
gsp_ids <- gsub("^0+", "", gsub("\\.RDS", "", extract_list))
all_places <- read.csv('EJ_DAC_Paper/Data/all_places.csv')
gsa_gsp <- read.csv('EJ_DAC_Paper/Data/gsa_gsp.csv')
gsa_names <- read.csv('EJ_DAC_Paper/Data/gsa_names.csv')

net_process <- function(file, gsp_id){
   # read in rds file
   temp <- readRDS(file)
   # grab nodelist
   nl <- temp$nodelist
   # label places and DACs
   all_places <- all_places %>% filter(GSP_ID == gsp_id )
   nl <- nl %>% left_join(all_places, by=join_by(entity_name == NAME20))
   el <- temp$edgelist
   # filter out NA edges and irrelevant cols
   el <- el %>% 
      filter(!is.na(source) & !is.na(target))  %>% 
      select(-doc_sent_verb)
   networklist <- list("nodelist" = nl, "edgelist" = el)
   
   return(networklist)
}

net_graph <- function(networklist, gsp_id){
   
   network_graph <- igraph::graph_from_data_frame(networklist$edgelist,
                                                  vertices = networklist$nodelist)
   
   # get gsa names from gsa_gsp and gsa_names
   gsas <- gsa_gsp %>% filter(GSP_ID == gsp_id)
   gsa_ids <- as.integer(unlist(strsplit(gsas$GSA_IDs, ",")))
   gsa_names <- merge(data.frame(GSA_ID = gsa_ids), gsa_names, by = "GSA_ID")$GSA_Name
   gsa_names <- c(gsa_names, 'groundwater_sustainability_agency') #add in deafult
   
   #distance to gsa(s)
   gsa_ins <- which(V(network_graph)$name %in% gsa_names)
   mean_dist <- list()
   for (i in gsa_ins){
      dist <- distances(network_graph, 
                        to=i,
                        mode = 'in')
      dist[is.infinite(dist)] <- NA # replace infinite distance with NA
      dist[is.nan(dist)] <- NA # replace NA with NA
      mean_dist[[paste0('X', i)]] <- dist
   }
   
   leader_dist_raw <- data.frame(mean_dist)
   leader_weights <- V(network_graph)[gsa_ins]$num_appearances
   weighted_dists <- (sweep(leader_dist_raw, 2, leader_weights, "*")) 
   leader_dist <- rowSums(weighted_dists, na.rm = TRUE) / sum(leader_weights)
   
   network_graph <- set_vertex_attr(network_graph,
                                    'leader_dist',
                                    value = leader_dist)
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
   return(network_graph)
}

for (g in seq_along(gsp_ids)) {
   gsp_list <- net_process(file = paste0(network_fp, 
                                         "/",
                                         extract_list[g]),
                           gsp_id = gsp_ids[g]
                           )
   gsp_id <- paste0("gsp_",gsp_ids[g])
   print(gsp_id)
   gsp_graph <- net_graph(gsp_list, 
                          gsp_id = gsp_ids[g])
   saveRDS(object = gsp_graph,
           file = paste0(Sys.getenv("BOX_PATH"),
                         "/EJ_Paper/cleaned_extracts_DACified",
                         "/",
                         extract_list[g]))
}

gsp_list_test <- net_process(file = paste0(network_fp, "/",extract_list[6]),
                             gsp_id = gsp_ids[6])

gsp_graph_test <- net_graph(gsp_list_test,
                            gsp_id = gsp_ids[6])

isolates_test <- which(degree(gsp_graph_test) == 0)
graph_2_test <- delete.vertices(gsp_graph_test, isolates_test)

ggraph(graph_2_test, 
       layout = 'igraph',
       algorithm = 'nicely') +
   geom_edge_link(color = "black", 
                  alpha = .5) + 
   geom_node_point(aes(size = num_appearances, colour = exists)) + 
   theme_graph()