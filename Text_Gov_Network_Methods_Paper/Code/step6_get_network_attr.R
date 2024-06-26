library(network)
library(sna)
library(igraph)
library(intergraph)
library(tidyverse)

filekey <- read.csv("filekey.csv")

edges_and_nodes <- list.files(path = filekey[filekey$var_name=="disambiged_extracts_govnetpaper",]$filepath, full.names = T)
gspids <-stringr::str_extract(edges_and_nodes,'[0-9]{1,}')

#CHOOSE ONE
type = "topic"
type = "governance_dir_full"
type = "governance_dir_weighted"
type = "people"
type = "orgs"
type = "gov_dir_weight_no_gpes"
#Table 2 uses gov_dir_weight_no_gpes


super <- readRDS(filekey[filekey$var_name=="supernetwork_weighted_govnetpaper",]$filepath)
super <- get.data.frame(super, what="vertices")
super <- super[,c("name","num_GSPs_in")]


if(type=="governance_dir_full"){
   network_properties <-  data.frame(matrix(NA, nrow = length(gspids), ncol=32))
   names(network_properties) <- c("gsp_id", "num_nodes", "num_edges",
                                  "connectedness", "centralization", "transitivity",
                                  "num_communities", "modularity", 
                                  "percent_homophily", "reciprocity", 
                                  "percent_org_to_org","percent_org_to_person","percent_org_to_gpe",
                                  "percent_person_to_org","percent_person_to_person","percent_person_to_gpe",
                                  "percent_gpe_to_org","percent_gpe_to_person","percent_gpe_to_gpe",
                                  "percent_org","percent_persons","percent_gpe", 
                                  "median_out_ties","mean_out_ties",
                                  "median_in_ties","mean_in_ties",
                                  "percent_vbn", "percent_vbg","percent_vbp",
                                  "percent_vbd","percent_vb","percent_vbz")
}
if(type=="governance_dir_weighted"){
   network_properties <-  data.frame(matrix(NA, nrow = length(gspids), ncol=28))
   names(network_properties) <- c("gsp_id", "num_nodes", "num_edges", "density", 
                                  "connectedness", "centralization", "transitivity",
                                  "num_communities", "modularity", 
                                  "percent_homophily", "reciprocity", 
                                  "percent_org_to_org","percent_org_to_person","percent_org_to_gpe",
                                  "percent_person_to_org","percent_person_to_person","percent_person_to_gpe",
                                  "percent_gpe_to_org","percent_gpe_to_person","percent_gpe_to_gpe",
                                  "percent_org","percent_persons","percent_gpe", 
                                  "median_num_out_neighbors","mean_num_out_neighbors",
                                  "median_num_in_neighbors","mean_num_in_neighbors",
                                  "mean_edge_weight")
}
if(type=="gov_dir_weight_no_gpes"){
   network_properties <-  data.frame(matrix(NA, nrow = length(gspids), ncol=22))
   names(network_properties) <- c("gsp_id", "num_nodes", "num_edges", "density", 
                                  "connectedness", "centralization", "transitivity",
                                  "num_communities", "modularity", 
                                  "percent_homophily", "reciprocity", 
                                  "percent_org_to_org","percent_org_to_person",
                                  "percent_person_to_org","percent_person_to_person",
                                  "percent_org","percent_persons",
                                  "median_num_out_neighbors","mean_num_out_neighbors",
                                  "median_num_in_neighbors","mean_num_in_neighbors",
                                  "mean_edge_weight")
}
if(type=="people"){
   network_properties <-  data.frame(matrix(NA, nrow = length(gspids), ncol=16))
   names(network_properties) <- c("gsp_id", "num_nodes", "num_edges", "density", 
                                  "connectedness", "centralization", "transitivity",
                                  "num_communities", "modularity", 
                                  "percent_homophily", "reciprocity", 
                                  "median_num_out_neighbors","mean_num_out_neighbors",
                                  "median_num_in_neighbors","mean_num_in_neighbors",
                                  "mean_edge_weight")
}
if(type=="orgs"){
   network_properties <-  data.frame(matrix(NA, nrow = length(gspids), ncol=16))
   names(network_properties) <- c("gsp_id", "num_nodes", "num_edges", "density", 
                                  "connectedness", "centralization", "transitivity",
                                  "num_communities", "modularity", 
                                  "percent_homophily", "reciprocity",
                                  "median_num_out_neighbors","mean_num_out_neighbors",
                                  "median_num_in_neighbors","mean_num_in_neighbors",
                                  "mean_edge_weight")
}
if(type=="topic"){
   gspids <- gspids[!gspids %in% c("0089","0053")]
   network_properties <-  data.frame(matrix(NA, nrow = length(gspids), ncol=9))
   names(network_properties) <- c("gsp_id", "num_nodes", "num_edges", "density", 
                                  "connectedness", "centralization", "transitivity",
                                  "num_communities", "modularity")
}


for(m in 1:length(gspids)){
   if(type=="governance_dir_full"){
      igr <- readRDS(paste0(filekey[filekey$var_name=="full_directed_graphs_govnetpaper",]$filepath,gspids[m]))
      agency_df <- get.data.frame(igr, what = "both")
      net <- network(x=agency_df$edges[,1:2], directed = T,
                     hyper = F, loops = T, multiple = T, 
                     bipartiate = F, vertices = agency_df$vertices,
                     matrix.type = "edgelist")
      edgelist <- get.data.frame(igr, what = "edges")
      nodelist <- get.data.frame(igr, what = "vertices")
      percent_homophily <- sum(nodelist$entity_type[match(edgelist$from, nodelist$name)]==nodelist$entity_type[match(edgelist$to, nodelist$name)]) / nrow(edgelist)
      reciprocated <- reciprocity(igr, ignore.loops = T, mode = "default")
      
      set.seed(327856)
      lc <- cluster_louvain(as.undirected(igr, mode = "collapse"))
      
      network_properties[m,] <- c(gspids[m], 
                                  network::network.size(net), 
                                  network::network.edgecount(net),
                                  #no way to define density for multiplex network
                                  sna::connectedness(net),
                                  sna::centralization(net,sna::degree),
                                  sna::gtrans(net, mode = "graph", use.adjacency=F),
                                  length(unique(lc$membership)), 
                                  modularity(igr,lc$membership,edgelist$weights),
                                  percent_homophily,
                                  reciprocated,
                                  sum(nodelist$entity_type[match(edgelist$from, nodelist$name)]=="ORG" & nodelist$entity_type[match(edgelist$to, nodelist$name)]=="ORG") / nrow(edgelist),
                                  sum(nodelist$entity_type[match(edgelist$from, nodelist$name)]=="ORG" & nodelist$entity_type[match(edgelist$to, nodelist$name)]=="PERSON") / nrow(edgelist),
                                  sum(nodelist$entity_type[match(edgelist$from, nodelist$name)]=="ORG" & nodelist$entity_type[match(edgelist$to, nodelist$name)]=="GPE") / nrow(edgelist),
                                  sum(nodelist$entity_type[match(edgelist$from, nodelist$name)]=="PERSON" & nodelist$entity_type[match(edgelist$to, nodelist$name)]=="ORG") / nrow(edgelist),
                                  sum(nodelist$entity_type[match(edgelist$from, nodelist$name)]=="PERSON" & nodelist$entity_type[match(edgelist$to, nodelist$name)]=="PERSON") / nrow(edgelist),
                                  sum(nodelist$entity_type[match(edgelist$from, nodelist$name)]=="PERSON" & nodelist$entity_type[match(edgelist$to, nodelist$name)]=="GPE") / nrow(edgelist),
                                  sum(nodelist$entity_type[match(edgelist$from, nodelist$name)]=="GPE" & nodelist$entity_type[match(edgelist$to, nodelist$name)]=="ORG") / nrow(edgelist),
                                  sum(nodelist$entity_type[match(edgelist$from, nodelist$name)]=="GPE" & nodelist$entity_type[match(edgelist$to, nodelist$name)]=="PERSON") / nrow(edgelist),
                                  sum(nodelist$entity_type[match(edgelist$from, nodelist$name)]=="GPE" & nodelist$entity_type[match(edgelist$to, nodelist$name)]=="GPE") / nrow(edgelist),
                                  sum(nodelist$entity_type=="ORG")/nrow(nodelist),
                                  sum(nodelist$entity_type=="PERSON")/nrow(nodelist),
                                  sum(nodelist$entity_type=="GPE")/nrow(nodelist),
                                  #degree includes loops by default
                                  median(igraph::degree(igr,mode="out")), mean(igraph::degree(igr,mode="out")),
                                  median(igraph::degree(igr,mode="in")), mean(igraph::degree(igr,mode="in")),
                                  sum(edgelist$head_verb_tense=="VBN")/nrow(edgelist),
                                  sum(edgelist$head_verb_tense=="VBG")/nrow(edgelist),
                                  sum(edgelist$head_verb_tense=="VBP")/nrow(edgelist),
                                  sum(edgelist$head_verb_tense=="VBD")/nrow(edgelist),
                                  sum(edgelist$head_verb_tense=="VB")/nrow(edgelist),
                                  sum(edgelist$head_verb_tense=="VBZ")/nrow(edgelist)
                                  
      )
      
   }
   if(type=="governance_dir_weighted"){
      
      igr <- readRDS(paste0(filekey[filekey$var_name=="weighted_nets_govnetpaper",]$filepath,gspids[m]))
      agency_df <- get.data.frame(igr, what = "both")
      net <- network(x=agency_df$edges[,1:2], directed = T,
                            hyper = F, loops = T, multiple = F, 
                            bipartiate = F, vertices = agency_df$vertices,
                            matrix.type = "edgelist")
      edgelist <- get.data.frame(igr, what = "edges")
      nodelist <- get.data.frame(igr, what = "vertices")
      percent_homophily <- sum(nodelist$entity_type[match(edgelist$from, nodelist$name)]==nodelist$entity_type[match(edgelist$to, nodelist$name)]) / nrow(edgelist)
      reciprocated <- reciprocity(igr, ignore.loops = T, mode = "default")
      
      set.seed(327856)
      lc <- cluster_louvain(as.undirected(igr, mode = "collapse"))
      mean_edge_weight <- mean(get.edge.attribute(igr, "weight"))
      network_properties[m,] <- c(gspids[m], 
                                  network::network.size(net), 
                                  network::network.edgecount(net),
                                  network::network.density(net),
                                  sna::connectedness(net),
                                  sna::centralization(net,sna::degree),
                                  sna::gtrans(net, mode = "graph", use.adjacency=F),
                                  length(unique(lc$membership)), 
                                  modularity(igr,lc$membership,edgelist$weights),
                                  percent_homophily,
                                  reciprocated,
                                  sum(nodelist$entity_type[match(edgelist$from, nodelist$name)]=="ORG" & nodelist$entity_type[match(edgelist$to, nodelist$name)]=="ORG") / nrow(edgelist),
                                  sum(nodelist$entity_type[match(edgelist$from, nodelist$name)]=="ORG" & nodelist$entity_type[match(edgelist$to, nodelist$name)]=="PERSON") / nrow(edgelist),
                                  sum(nodelist$entity_type[match(edgelist$from, nodelist$name)]=="ORG" & nodelist$entity_type[match(edgelist$to, nodelist$name)]=="GPE") / nrow(edgelist),
                                  sum(nodelist$entity_type[match(edgelist$from, nodelist$name)]=="PERSON" & nodelist$entity_type[match(edgelist$to, nodelist$name)]=="ORG") / nrow(edgelist),
                                  sum(nodelist$entity_type[match(edgelist$from, nodelist$name)]=="PERSON" & nodelist$entity_type[match(edgelist$to, nodelist$name)]=="PERSON") / nrow(edgelist),
                                  sum(nodelist$entity_type[match(edgelist$from, nodelist$name)]=="PERSON" & nodelist$entity_type[match(edgelist$to, nodelist$name)]=="GPE") / nrow(edgelist),
                                  sum(nodelist$entity_type[match(edgelist$from, nodelist$name)]=="GPE" & nodelist$entity_type[match(edgelist$to, nodelist$name)]=="ORG") / nrow(edgelist),
                                  sum(nodelist$entity_type[match(edgelist$from, nodelist$name)]=="GPE" & nodelist$entity_type[match(edgelist$to, nodelist$name)]=="PERSON") / nrow(edgelist),
                                  sum(nodelist$entity_type[match(edgelist$from, nodelist$name)]=="GPE" & nodelist$entity_type[match(edgelist$to, nodelist$name)]=="GPE") / nrow(edgelist),
                                  sum(nodelist$entity_type=="ORG")/nrow(nodelist),
                                  sum(nodelist$entity_type=="PERSON")/nrow(nodelist),
                                  sum(nodelist$entity_type=="GPE")/nrow(nodelist),
                                  #degree includes loops by default
                                  median(igraph::degree(igr,mode="out")), mean(igraph::degree(igr,mode="out")),
                                  median(igraph::degree(igr,mode="in")), mean(igraph::degree(igr,mode="in")),
                                  mean_edge_weight
                                  
      )
   }
   if(type=="gov_dir_weight_no_gpes"){
      
      igr <- readRDS(paste0(filekey[filekey$var_name=="weighted_nets_govnetpaper",]$filepath,gspids[m]))
      igr <- subgraph(igr, V(igr)[vertex_attr(igr,"entity_type") %in% c("ORG","PERSON")])
      agency_df <- get.data.frame(igr, what = "both")
      net <- network(x=agency_df$edges[,1:2], directed = T,
                     hyper = F, loops = T, multiple = F, 
                     bipartiate = F, vertices = agency_df$vertices,
                     matrix.type = "edgelist")
      edgelist <- get.data.frame(igr, what = "edges")
      nodelist <- get.data.frame(igr, what = "vertices")
      percent_homophily <- sum(nodelist$entity_type[match(edgelist$from, nodelist$name)]==nodelist$entity_type[match(edgelist$to, nodelist$name)]) / nrow(edgelist)
      reciprocated <- reciprocity(igr, ignore.loops = T, mode = "default")
      
      set.seed(327856)
      lc <- cluster_louvain(as.undirected(igr, mode = "collapse"))
      mean_edge_weight <- mean(get.edge.attribute(igr, "weight"))
      network_properties[m,] <- c(gspids[m], 
                                  network::network.size(net), 
                                  network::network.edgecount(net),
                                  network::network.density(net),
                                  sna::connectedness(net),
                                  sna::centralization(net,sna::degree),
                                  sna::gtrans(net, mode = "graph", use.adjacency=F),
                                  length(unique(lc$membership)), 
                                  modularity(igr,lc$membership,edgelist$weights),
                                  percent_homophily,
                                  reciprocated,
                                  sum(nodelist$entity_type[match(edgelist$from, nodelist$name)]=="ORG" & nodelist$entity_type[match(edgelist$to, nodelist$name)]=="ORG") / nrow(edgelist),
                                  sum(nodelist$entity_type[match(edgelist$from, nodelist$name)]=="ORG" & nodelist$entity_type[match(edgelist$to, nodelist$name)]=="PERSON") / nrow(edgelist),
                                  sum(nodelist$entity_type[match(edgelist$from, nodelist$name)]=="PERSON" & nodelist$entity_type[match(edgelist$to, nodelist$name)]=="ORG") / nrow(edgelist),
                                  sum(nodelist$entity_type[match(edgelist$from, nodelist$name)]=="PERSON" & nodelist$entity_type[match(edgelist$to, nodelist$name)]=="PERSON") / nrow(edgelist),
                                  sum(nodelist$entity_type=="ORG")/nrow(nodelist),
                                  sum(nodelist$entity_type=="PERSON")/nrow(nodelist),
                                  #degree includes loops by default
                                  median(igraph::degree(igr,mode="out")), mean(igraph::degree(igr,mode="out")),
                                  median(igraph::degree(igr,mode="in")), mean(igraph::degree(igr,mode="in")),
                                  mean_edge_weight
                                  
      )
   }
   if(type %in% c("orgs","people")){
      igr <- readRDS(paste0(filekey[filekey$var_name=="weighted_nets_govnetpaper",]$filepath,gspids[m]))
      if(type=="orgs"){
         igr <- subgraph(igr, V(igr)[vertex_attr(igr,"entity_type") == "ORG"])
      }else{
         igr <- subgraph(igr, V(igr)[vertex_attr(igr,"entity_type") == "PERSON"])
      }
      agency_df <- get.data.frame(igr, what = "both")
      agency_df$vertices <- left_join(agency_df$vertices, super)
      if(nrow(agency_df$edges)>0){
         net <- network(x=agency_df$edges[,1:2], directed = T,
                        hyper = F, loops = T, multiple = F, 
                        bipartiate = F, vertices = agency_df$vertices,
                        matrix.type = "edgelist")
         edgelist <- get.data.frame(igr, what = "edges")
         nodelist <- get.data.frame(igr, what = "vertices")
         percent_homophily <- sum(nodelist$entity_type[match(edgelist$from, nodelist$name)]==nodelist$entity_type[match(edgelist$to, nodelist$name)]) / nrow(edgelist)
         reciprocated <- reciprocity(igr, ignore.loops = T, mode = "default")
         
         set.seed(327856)
         lc <- cluster_louvain(as.undirected(igr, mode = "collapse"))
         mean_edge_weight <- mean(igraph::get.edge.attribute(igr, "weight"))
         network_properties[m,] <- c(gspids[m], 
                                     network::network.size(net), 
                                     network::network.edgecount(net),
                                     network::network.density(net),
                                     sna::connectedness(net),
                                     sna::centralization(net,sna::degree),
                                     sna::gtrans(net, mode = "graph", use.adjacency=F),
                                     length(unique(lc$membership)), 
                                     modularity(igr,lc$membership,edgelist$weights),
                                     percent_homophily,
                                     reciprocated,
                                     #degree includes loops by default
                                     median(igraph::degree(igr,mode="out")), mean(igraph::degree(igr,mode="out")),
                                     median(igraph::degree(igr,mode="in")), mean(igraph::degree(igr,mode="in")),
                                     mean_edge_weight)
                                     
      }else{
         network_properties[m,] <- c(gspids[m],
                                     nrow(agency_df$vertices),
                                     nrow(agency_df$edges), 
                                     0,
                                     0,
                                     NA,
                                     NA,
                                     nrow(agency_df$vertices),
                                     rep(NA, 8))
      }
   }
}

if(type=="topic"){
   
   for(m in 1:length(gspids)){
      
      net <- readRDS(paste0(filekey[filekey$var_name=="topic_corr_files",]$filepath,gspids[m]))$posadj
      igr <- igraph::graph.adjacency(net, mode = "undirected",weighted=NULL,diag=F)
      net <- asNetwork(igr)
      edgelist <- get.data.frame(igr, what = "edges")
      #topics do not have "types", so percent_homophily is not applicable
      
      set.seed(327856)
      lc <- cluster_louvain(igr)
      
      network_properties[m,] <- c(gspids[m], 
                                  network::network.size(net), 
                                  network::network.edgecount(net),
                                  network::network.density(net),
                                  sna::connectedness(net),
                                  sna::centralization(net,sna::degree),
                                  sna::gtrans(net, mode = "graph", use.adjacency=F),
                                  length(unique(lc$membership)), 
                                  modularity(igr,lc$membership,edgelist$weights))
      
   }
}


if(type=="topic"){
   
   saveRDS(network_properties, paste0(filekey[filekey$var_name=="network_properties_folder_govnetpaper",]$filepath,"/topic_network_properties"))
   
}

if(type=="governance_dir_full"){
   
   saveRDS(network_properties, paste0(filekey[filekey$var_name=="network_properties_folder_govnetpaper",]$filepath,"/dir_full_network_properties"))
   
}
if(type=="governance_dir_weighted"){
   
   saveRDS(network_properties, paste0(filekey[filekey$var_name=="network_properties_folder_govnetpaper",]$filepath,"/dir_weighted_network_properties"))
   
}
if(type=="gov_dir_weight_no_gpes"){
   
   saveRDS(network_properties, paste0(filekey[filekey$var_name=="network_properties_folder_govnetpaper",]$filepath,"/gov_dir_weight_no_gpe_network_properties"))
   
}
if(type=="orgs"){
   saveRDS(network_properties, paste0(filekey[filekey$var_name=="network_properties_folder_govnetpaper",]$filepath,"/gov_dir_weight_orgs_properties"))
}
if(type=="people"){
   saveRDS(network_properties, paste0(filekey[filekey$var_name=="network_properties_folder_govnetpaper",]$filepath,"/gov_dir_weight_people_properties"))
}
#not including the yuba duplicate and the improper pdf formatting
network_properties <- network_properties[c(1:38,40:67,69:119),]

#network_properties <- cbind("gsp_id" = network_properties$gsp_id, 
#                            sapply(network_properties[,2:ncol(network_properties)], function(x) as.numeric(x)))

if(type=="governance_dir_full"){
   network_properties_summary_table <- network_properties[,c("num_nodes", "num_edges",
                                                             "connectedness", "centralization", "transitivity",
                                                             "num_communities", "modularity", 
                                                             "percent_homophily", "reciprocity", 
                                                             "percent_org_to_org",
                                                             "percent_org","mean_out_ties","mean_in_ties",
                                                             "percent_vbn", "percent_vbg","percent_vbp",
                                                             "percent_vbd","percent_vb","percent_vbz")]
   View(summary(network_properties_summary_table,digits=2))
   
}else if(type=="governance_dir_weighted"){
   network_properties_summary_table <- network_properties[,c("num_nodes", "num_edges",
                                                             "connectedness", "centralization", "transitivity",
                                                             "num_communities", "modularity", 
                                                             "percent_homophily", "reciprocity", 
                                                             "percent_org_to_org",
                                                             "percent_org","mean_num_out_neighbors","mean_num_in_neighbors",
                                                             "mean_edge_weight")]
   
   View(summary(network_properties_summary_table,digits=2))
   
}




if(type == "governance_dir_full"){
   network_properties_for_pairs <- network_properties[,c("connectedness", "centralization", "transitivity",
                                                         "num_communities", "modularity", 
                                                         "percent_homophily", "reciprocity", 
                                                         "percent_org_to_org","percent_org_to_person","percent_org_to_gpe",
                                                         "percent_person_to_org","percent_person_to_person","percent_person_to_gpe",
                                                         "percent_gpe_to_org","percent_gpe_to_person",
                                                         "mean_out_ties",
                                                         "percent_vbn", "percent_vbg","percent_vbp",
                                                         "percent_vbd","percent_vb","percent_vbz")]
   View(summary(network_properties_summary_table,digits=2))
   
}

library(GGally) 
library(ggplot2)



verbtensepairs <- ggpairs(as.data.frame(network_properties_for_pairs[,(16:22)]))+theme_bw()
#summarypairs1 did not include column 7
summarypairs2 <- ggpairs(as.data.frame(network_properties_for_pairs[,(c(1:5,7,16))]))+theme_bw()

ggsave(paste0("verb_tense_and_degree_",type,".png"), plot = verbtensepairs, device = "png",
       path = filekey[filekey$var_name=="govnetpaper_figures",]$filepath, width = 4020, height = 3015, dpi = 300,
       units = "px", bg = "white")

ggsave(paste0("summarypairs2",type,".png"), plot = summarypairs2, device = "png",
       path = filekey[filekey$var_name=="govnetpaper_figures",]$filepath, width = 4020, height = 3015, dpi = 300,
       units = "px", bg = "white")

gsp_meta <- readRDS(filekey[filekey$var_name=="gsp_docs_meta_stmpaper",]$filepath)
gsp_mini <- unique(gsp_meta[,c(14,16,19:26,7)])
gsp_mini <- gsp_mini[!gsp_mini$gsp_id %in% c("0089","0053"),]
#for meta
network_properties <- as_tibble(network_properties)
net_with_gsa_attr <- merge(gsp_mini, network_properties)

if(type=="gov_dir_weight_no_gpes"){
   #net_with_gsa_attr <- net_with_gsa_attr[,c(1:10,16:18,20:22,29,32)]
}else{
   #net_with_gsa_attr <- net_with_gsa_attr[,c(1:10,16:18,20:22,36,39)]
   
}


net_with_gsa_attr$ynapproved <- ifelse(net_with_gsa_attr$approval %in% c("Inadequate","Incomplete"),"Y",
                                      ifelse(net_with_gsa_attr$approval == "Approved","N","Under_Review"))
net_with_gsa_attr$ynapproved <- factor(net_with_gsa_attr$ynapproved, ordered = T, levels = c("N","Under_Review","Y"))
net_with_gsa_attr$priority <- factor(net_with_gsa_attr$priority,ordered=T, levels = c("High","Medium","Low","Very Low"))

net_with_gsa_attr$mult_gsas <- as.numeric(net_with_gsa_attr$mult_gsas)
net_with_gsa_attr$connectedness <- as.numeric(net_with_gsa_attr$connectedness)
net_with_gsa_attr$centralization <- as.numeric(net_with_gsa_attr$centralization)
net_with_gsa_attr$transitivity <- as.numeric(net_with_gsa_attr$transitivity)
net_with_gsa_attr$modularity <- as.numeric(net_with_gsa_attr$modularity)
net_with_gsa_attr$percent_homophily <- as.numeric(net_with_gsa_attr$percent_homophily)
net_with_gsa_attr$reciprocity <- as.numeric(net_with_gsa_attr$reciprocity)
net_with_gsa_attr$mean_num_out_neighbors <- as.numeric(net_with_gsa_attr$mean_num_out_neighbors)
net_with_gsa_attr$mean_edge_weight <- as.numeric(net_with_gsa_attr$mean_edge_weight)
net_with_gsa_attr$ynapproved <- as.numeric(net_with_gsa_attr$ynapproved)
net_with_gsa_attr$percent_dac_by_pop <- as.numeric(net_with_gsa_attr$percent_dac_by_pop)
net_with_gsa_attr$num_nodes <- as.numeric(net_with_gsa_attr$num_nodes)
net_with_gsa_attr$num_edges <- as.numeric(net_with_gsa_attr$num_edges)

library(htmlTable)
library(vtable)
sumtable(data = net_with_gsa_attr[,-c('gsp_id','id')],add.median = T,out = 'htmlreturn',file = 'figures/table2_sumstats.html',)
sum_res <- summary(net_with_gsa_attr[,-c('gsp_id','id')])

data.table(sum_res)



#exploring whether gsa attributes are correlated with any network properties
#does not appear so.
aprmodel <- lm(ynapproved ~ percent_dac_by_pop + percent_homophily + 
                  ag_gw_asfractof_tot_gw + 
                  mean_edge_weight, data = net_with_gsa_attr)
AIC(aprmodel)
summary(aprmodel)
#plans with high % dac are more likely to be approved
#plans with high percent homophily are more likely to be approved (if gpes included)
#plans with high ag gw are more likely to be approved

net <- net_with_gsa_attr
ggpairs(net)

cor.test(net$percent_dac_by_pop, net$connectedness)
cor.test(net$percent_dac_by_pop, net$modularity)
cor.test(net$percent_dac_by_pop, net$mean_num_out_neighbors)

performance::check_model(lm(percent_dac_by_pop~mean_num_out_neighbors, data=net))

#extremes of connection and centralization
min_connect <- network_properties_df %>% 
   filter(connectedness == min(connectedness))
max_connect <- network_properties_df %>% 
   filter(connectedness == max(connectedness))
min_centr <- network_properties_df %>% 
   filter(centralization == min(centralization))
max_centr <- network_properties_df %>% 
   filter(centralization == max(centralization))
min_comm <- network_properties_df %>% 
   filter(num_communities== min(num_communities))
max_comm <- network_properties_df %>% 
   filter(num_communities== max(num_communities))