library(network)
library(tidyverse)
library(sna)
library(intergraph)
library(DirectedClustering)
library(GGally)

#removes loops for node attr calculations

edges_and_nodes <- list.files(path = "cleaned_extracts", full.names = T)
gspids <- substr(edges_and_nodes, 18,21)
overwrite = T

super <- readRDS("data_output/supernetwork_weighted")
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
    agency_ig <- readRDS(paste0("data_output/to_weighted_graph_",gspids[m]))
    agency_ig <- subgraph(agency_ig, V(agency_ig)[
       vertex_attr(agency_ig,"entity_type") %in% c("ORG","PERSON")])
    agency_ig <- igraph::simplify(agency_ig, remove.multiple=T, remove.loops=T)
    agency_df <- get.data.frame(agency_ig, what = "both")
    agency_df$vertices <- left_join(agency_df$vertices, super)
    agency_net <- network(x=agency_df$edges[,1:2], directed = T,
                          hyper = F, loops = T, multiple = F, 
                          bipartiate = F, vertices = agency_df$vertices,
                          matrix.type = "edgelist")
    agency_mat <- as.matrix(agency_net)
    paths2 <- diag(agency_mat %*% agency_mat)
    recip <- 2*paths2 / degree(agency_net)
    totalCC <- ClustF(agency_mat, type = "directed", isolates="zero")$totalCC
    closens <- sna::closeness(agency_net, gmode = "graph", cmode="undirected")
    between <- sna::betweenness(agency_net,gmode = "graph",cmode="undirected")
    deg <- sna::degree(agency_net, gmode = "graph", cmode = "undirected")
    #eigenvector centrality not calculated because only meaningful for connected components
    #eigenvector <- sna::evcent(agency_net, gmode = "graph",maxiter = 1e6)
    centr_df <- tibble(name = network::get.vertex.attribute(agency_net, "vertex.names"), 
                       closens, 
                       between, 
                       deg,
                       recip,
                       totalCC,
                       entity_type = get.vertex.attribute(agency_net,"entity_type"),
                       num_GSPs_in = network::get.vertex.attribute(agency_net, "num_GSPs_in"))
    saveRDS(centr_df, paste0("data_output/node_attr_",gspids[m]))
  }
}  
  
centr_df <- vector(mode = "list", length= length(gspids))
for(m in 1:length(gspids)){
   if(!gspids[m] %in% c("0053","0089")){
      centr_df[[m]] <- readRDS(paste0("data_output/node_attr_",gspids[m]))
      centr_df[[m]]$gspid <- gspids[m]
   }
}

all_nodes <- rbindlist(centr_df)

ggpairs(all_nodes)

recip_model <- glm(recip ~ as.factor(entity_type) + 
                      as.factor(gspid) + deg + num_GSPs_in)
totalCC_model <- glm(totalCC ~ as.factor(entity_type) + 
                        as.factor(gspid) + deg + num_GSPs_in)

t.test(as.numeric(all_nodes$deg[all_nodes$entity_type=="PERSON"]), 
       as.numeric(all_nodes$deg[all_nodes$entity_type=="ORG"]))

t.test(as.numeric(all_nodes$num_GSPs_in[all_nodes$entity_type=="PERSON"]), 
       as.numeric(all_nodes$num_GSPs_in[all_nodes$entity_type=="ORG"]))

t.test(as.numeric(all_nodes$recip[all_nodes$entity_type=="PERSON"]), 
       as.numeric(all_nodes$recip[all_nodes$entity_type=="ORG"]))

t.test(as.numeric(all_nodes$totalCC[all_nodes$entity_type=="PERSON"]), 
       as.numeric(all_nodes$totalCC[all_nodes$entity_type=="ORG"]))



