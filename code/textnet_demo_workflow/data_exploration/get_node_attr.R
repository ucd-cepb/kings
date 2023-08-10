library(network)
library(tidyverse)
library(sna)
library(intergraph)
library(network)

edges_and_nodes <- list.files(path = "cleaned_extracts", full.names = T)
gspids <- substr(edges_and_nodes, 18,21)
overwrite = T

super <- readRDS("data_output/supernetwork_weighted")
super <- subgraph(super, V(super)[
   vertex_attr(super,"entity_type") %in% c("ORG","PERSON")])
super <- get.data.frame(super, what="vertices")
super <- super[,c("name","num_GSPs_in")]


gsps <- c(1:38,40:67,69:length(gspids))
for(m in gsps){
   if(overwrite==T){
      makefile <- T
   }else{
      makefile <- !file.exists(paste0("data_output/node_attr_",gspids[m]))
   }
   print(m)
  if(makefile==T){
    agency_ig <- readRDS(paste0("data_output/full_directed_graph_",gspids[m]))
    agency_ig <- subgraph(agency_ig, V(agency_ig)[
       vertex_attr(agency_ig,"entity_type") %in% c("ORG","PERSON")])
    
    agency_df <- get.data.frame(agency_ig, what = "both")
    agency_df$vertices <- left_join(agency_df$vertices, super)
    agency_net <- network(x=agency_df$edges[,1:2], directed = T,
                          hyper = F, loops = T, multiple = T, 
                          bipartiate = F, vertices = agency_df$vertices,
                          matrix.type = "edgelist")
    closens <- sna::closeness(agency_net, gmode = "graph", cmode="undirected")
    between <- sna::betweenness(agency_net,gmode = "graph",cmode="undirected")
    deg <- sna::degree(agency_net, gmode = "graph", cmode = "undirected")
    #eigenvector centrality not calculated because only meaningful for connected components
    #eigenvector <- sna::evcent(agency_net, gmode = "graph",maxiter = 1e6)
    centr_df <- tibble(name = network::get.vertex.attribute(agency_net, "vertex.names"), 
                       closens, between, deg, #eigenvector, 
                       type = network::get.vertex.attribute(agency_net, "type"))
    saveRDS(centr_df, paste0("data_output/node_attr_",gspids[m]))
  }
}  
  





