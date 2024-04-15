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
all_places <- read.csv('EJ_DAC_Paper/Data/all_places.csv')

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

net_graph <- function(networklist){
   
   network_graph <- igraph::graph_from_data_frame(networklist$edgelist,
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


for (g in seq_along(gsp_ids)) {
   gsp_list <- net_process(file = paste0(network_fp, 
                                         "/",
                                         extract_list[g]),
                           gsp_id = gsp_ids[g]
                           )
   gsp_id <- paste0("gsp_",gsp_ids[g])
   gsp_graph <- net_graph(gsp_list)
   saveRDS(object = gsp_graph, 
           file = paste0(Sys.getenv("BOX_PATH"),
                         "/EJ_Paper/cleaned_extracts_DACified", 
                         "/", 
                         extract_list[g])
           )
}

# gsp_list_test <- net_process(file = paste0(network_fp, "/",extract_list[3]),
#                              gsp_id = gsp_ids[3])
# 
# gsp_graph_test <- net_graph(gsp_list_test)
