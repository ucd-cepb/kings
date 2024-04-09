library(dotenv)
library(ggraph)
library(igraph)
library(tidyverse)
library(migraph)
library(data.table)

load_dot_env()

network_fp <- paste0(Sys.getenv("BOX_PATH"), "/EJ_Paper/cleaned_extracts_textgov_paper_version")
extract_list = list.files(network_fp)
gsp_ids <- gsub("^0+", "", gsub("\\.RDS", "", extract_list))
locs <- read.csv('EJ_DAC_Paper/Data/locations.csv')

### convenience function for reading in and processing nodes
### extracts have ORG, PERSON, and GPE nodetypes
net_process <- function(file, drop = 'PERSON'){
   # read in rds file
   temp <- readRDS(file)
   # grap nodelist
   nl <- temp$nodelist
   # label places and DACs
   nl$place <- ifelse(nl$entity_name %in% locs$place_name, 1, 0)
   nl <- nl %>% left_join(locs, by=join_by(entity_name == place_name))
   el <- temp$edgelist
   # filter out NA edges and irrelevant cols
   el <- el %>% 
      filter(!is.na(source) & !is.na(target))  %>% 
      select(-doc_sent_verb)
   networklist <- list("nodelist" = nl, "edgelist" = el)
   
   return(networklist)
}

net_graph <- function(networklist){
   network_graph <- graph_from_data_frame(networklist$edgelist,
                                          vertices = networklist$nodelist)
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
   return(network_graph)
}

gsp_list <- net_process(paste0(network_fp, "/",extract_list[15]))
gsp_graph <- net_graph(gsp_list)

isolates <- which(degree(gsp_graph) == 0)
graph_2 <- delete.vertices(gsp_graph, isolates)

ggraph(gsp_graph, layout = 'kk') +
   geom_edge_link(color = "black", alpha = .5) + 
   geom_node_point(aes(size = num_appearances, colour = DAC)) + 
   theme_graph()

