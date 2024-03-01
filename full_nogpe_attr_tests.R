library(network)
library(sna)
library(igraph)
library(intergraph)
library(tidyverse)

filekey <- read.csv("filekey.csv")

edges_and_nodes <- list.files(path = filekey[filekey$var_name=="disambiged_extracts_govnetpaper",]$filepath, full.names = T)
gspids <-stringr::str_extract(edges_and_nodes,'[0-9]{1,}')

type = "governance_dir_weighted"

if(type == "governance_dir_full"){
   network_properties <-  data.frame(matrix(NA, nrow = length(gspids), ncol=26))
   names(network_properties) <- c("gsp_id", "num_nodes", "num_edges",
                                  "connectedness", "centralization", "transitivity",
                                  "num_communities", "modularity", 
                                  "percent_homophily", "reciprocity", 
                                  "percent_org_to_org","percent_org_to_person",
                                  "percent_person_to_org","percent_person_to_person",
                                  "percent_org","percent_persons", 
                                  "median_out_ties","mean_out_ties",
                                  "median_in_ties","mean_in_ties",
                                  "percent_vbn", "percent_vbg","percent_vbp",
                                  "percent_vbd","percent_vb","percent_vbz")
   
   
   for(m in 1:length(gspids)){
      
      igr <- readRDS(paste0(filekey[filekey$var_name=="full_directed_graphs_govnetpaper",]$filepath,gspids[m]))
      #remove GPEs
      igr <- subgraph(igr, V(igr)[vertex_attr(igr,"entity_type") %in% c("ORG","PERSON")])
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
                                  sum(nodelist$entity_type[match(edgelist$from, nodelist$name)]=="PERSON" & nodelist$entity_type[match(edgelist$to, nodelist$name)]=="ORG") / nrow(edgelist),
                                  sum(nodelist$entity_type[match(edgelist$from, nodelist$name)]=="PERSON" & nodelist$entity_type[match(edgelist$to, nodelist$name)]=="PERSON") / nrow(edgelist),
                                  sum(nodelist$entity_type=="ORG")/nrow(nodelist),
                                  sum(nodelist$entity_type=="PERSON")/nrow(nodelist),
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
   
   
   saveRDS(network_properties, paste0(filekey[filekey$var_name=="network_properties_folder_verbnetpaper",]$filepath,"/dir_full_nogpe_network_properties"))
   
   
}


if(type == "governance_dir_weighted"){
   network_properties <-  data.frame(matrix(NA, nrow = length(gspids), ncol=24))
   names(network_properties) <- c("gsp_id", "num_nodes", "num_edges", "density", 
                                  "connectedness", "centralization", "transitivity",
                                  "num_communities", "modularity", 
                                  "percent_homophily", "reciprocity", 
                                  "percent_org_to_org","percent_org_to_person",
                                  "percent_person_to_org","percent_person_to_person",
                                  "percent_org","percent_persons",
                                  "median_num_out_neighbors","mean_num_out_neighbors",
                                  "median_num_in_neighbors","mean_num_in_neighbors",
                                  "mean_edge_weight", "mean_out_strength","mean_total_neighbors")
   for(m in 1:length(gspids)){
      
      igr <- readRDS(paste0(filekey[filekey$var_name=="weighted_nets_govnetpaper",]$filepath,gspids[m]))
      #remove GPEs
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
                                  mean_edge_weight,
                                  mean(igraph::strength(igr,mode="out", loops=T)),
                                  mean(igraph::degree(igr,mode="all"))
                                  
                                  
      )
      
      
   }
   
   
   saveRDS(network_properties, paste0(filekey[filekey$var_name=="network_properties_folder_verbnetpaper",]$filepath,"/dir_weighted_nogpe_network_properties"))
   
   
}

network_properties <- network_properties[!network_properties$gsp_id%in%c("0053","0089"),]
   
webvarfilename <- filekey[filekey$var_name=="gsp_web_vars_planevolutionpaper",]$filepath
webvarfilenamesplits <- unlist(strsplit(webvarfilename,split="/"))
webvarpath <- paste(webvarfilenamesplits[1:(length(webvarfilenamesplits)-1)],collapse = "/")
webvarpattern <- webvarfilenamesplits[length(webvarfilenamesplits)]

#get the most recent file in the xpath that matches xpattern
recentwebvarfile <- readRDS(list.files(path = webvarpath, pattern = webvarpattern, full.names = T)[
   length(list.files(path = webvarpath, pattern = webvarpattern, full.names = T))])
gsp_webvars <- recentwebvarfile[recentwebvarfile$version=="1"]
gsp_webvars <- gsp_webvars[!gsp_webvars$gsp_num_id %in% c("0089","0053"),]
colnames(gsp_webvars)[colnames(gsp_webvars)=="gsp_num_id"] <- "gsp_id"
gsp_webvars <- gsp_webvars[,c("gsp_id","version_approval")]

gsp_meta <- readRDS(filekey[filekey$var_name=="gsp_docs_lean",]$filepath)
gsp_mini <- unique(gsp_meta[,c("gsp_id","approval","mult_gsas",
                               "fract_of_area_in_habitat_log_scaled",
                               "urbangw_af_log_scaled",
                               "gwsum",
                               "percent_dac_by_pop_scaled",
                               "Republican_Vote_Share_scaled",
                               "Agr_Share_Of_GDP_scaled",
                               "Perc_Bach_Degree_Over25_scaled",
                               "local_govs_per_10k_people_log_scaled",
                               "maxdryspell_scaled")])
gsp_mini <- gsp_mini[!gsp_mini$gsp_id %in% c("0089","0053"),]
gsp_mini$approval <- NULL

gsp_all_vars <- full_join(gsp_webvars, gsp_mini)

colnames(gsp_all_vars)[colnames(gsp_all_vars)=="version_approval"] <- "approval"


library(GGally) 
library(ggplot2)


network_properties <- as_tibble(network_properties)
net_with_gsa_attr <- merge(gsp_all_vars, network_properties)


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
net_with_gsa_attr$mean_out_strength <- as.numeric(net_with_gsa_attr$mean_out_strength)
net_with_gsa_attr$ynapproved <- as.numeric(net_with_gsa_attr$ynapproved)
net_with_gsa_attr$percent_dac_by_pop <- as.numeric(net_with_gsa_attr$percent_dac_by_pop)
net_with_gsa_attr$num_nodes <- as.numeric(net_with_gsa_attr$num_nodes)
net_with_gsa_attr$num_edges <- as.numeric(net_with_gsa_attr$num_edges)
net_with_gsa_attr$mean_total_neighbors <- as.numeric(net_with_gsa_attr$mean_total_neighbors)



#exploring whether gsa attributes are correlated with any network properties
#does not appear so.
aprmodel <- lm(ynapproved ~ percent_dac_by_pop + percent_homophily + 
                  Agr_Share_Of_GDP_scaled + 
                  mean_total_neighbors, data = net_with_gsa_attr)
AIC(aprmodel)
summary(aprmodel)
#plans with high % dac are more likely to be approved
#plans with high percent homophily are more likely to be approved 
#plans with high ag gw are more likely to be approved

net <- net_with_gsa_attr

cor.test(net$percent_dac_by_pop, net$connectedness)
cor.test(net$percent_dac_by_pop, net$modularity)
cor.test(net$percent_dac_by_pop, net$mean_num_out_neighbors)
cor.test(net$percent_dac_by_pop, net$mean_total_neighbors)
cor.test(net$percent_dac_by_pop, net$mean_out_strength)

performance::check_model(lm(percent_dac_by_pop_scaled~mean_out_ties, data=net))

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