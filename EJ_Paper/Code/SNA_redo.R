library(ggnetwork)
library(igraph)
library(devtools)
library(ggraph)
library(sna)
library(stringr)
library(dplyr)
library(data.table)
library(pbapply)
library(stringi)
library(tidyverse)


###PULL FILE###
file_loc <- 'C:\\Users\\hgsha\\Box Sync\\Kings_Large_Files\\data\\EJ_Paper\\cleaned_extracts_textgov_paper_version\\'
extract_list = list.files(file_loc)
n = readRDS('C:\\Users\\hgsha\\Box Sync\\Kings_Large_Files\\data\\EJ_Paper\\cleaned_extracts_textgov_paper_version\\0007.RDS')
head(n$nodelist)
head(n$edgelist)

#EDGE AND NODE LIST
edges_007<- n$edgelist
edge_complete <- edges_007 %>% filter(edgeiscomplete=="TRUE") %>%
   select(source, target)
head(edge_complete)

nodes_007 <- n$nodelist
nodes_007 <- nodes_007[nodes_007$entity_type != 'PERSON']

#MERGE LISTS#
org_nodes <- read.csv("EJ_Paper/Data/Test/merged_dt_4_24.csv")
nodelist_merge <- inner_join(nodes_007, org_nodes, by = c("entity_name", "entity_type"))

#GSUB RULES
######FIXING COMMON DUPLICATION PROBLEMS
nodelist_merge$new_name <- gsub("_wd$", "_water_district", nodelist_merge$entity_name) #works
nodelist_merge$new_name <- gsub("_s$", "", nodelist_merge$new_name) #works
nodelist_merge$new_name <- gsub("^a_", "", nodelist_merge$new_name) #works
nodelist_merge$new_name <- gsub("(.+?_)\\1+", "\\1", nodelist_merge$new_name) #works
nodelist_merge$new_name <- gsub('([a-z]+_[a-z]+_)\\1+','\\1',nodelist_merge$new_name,perl = T)

###FIX NODELIST MERGE FILE
head(nodelist_merge)
#Drop unnecessary columns - clean the file
drop_count <- str_count(nodelist_merge$comb_org, "Drop")

###NETWORK OBJECT###
edge_m <- as.matrix(edge_complete)
net <- network(edge_complete, matrix.type="edgelist", loops = T, 
                    directed = T, duplicates = TRUE, multiple = T)

###NEW PLOT###
net %v% 'comb_org' <- nodelist_merge$comb_org[match(network.vertex.names(net),nodelist_merge$entity_name)]

categories_to_filter <- c('Drop', 'Follow')
net <- get.inducedSubgraph(net,v = which(!{net %v% 'comb_org' %in% categories_to_filter}))
categories_to_label <- c("NGO", "CBO")
graph<- plot(net,
     displaylabels=F,
     vertex.col='comb_org',
     vertex.cex=0.8)


#Plot - this plot works - need to figure out legend
graph<- plot(edgelist, displaylabels=F, vertex.col='comb_org', vertex.cex=1)
vertex_colors <- edgelist %v% 'comb_org'
legend("topright", legend = levels(factor('comb_org'), fill = vertex_colors))

plot(graph,
     layout = igraph::layout_nicely(graph),
     vertex.label =NA,
     vertex.size = 5,
     edge.arrow.size = 0.5)
gplot(edgelist)




##centrality stats
degree_sna <- degree(net, gmode = "digraph") #NUMBER OF EDGES
degree_sna_in <- degree(net, gmode = "digraph", cmode ="indegree") #indegree
degree_sna_out <- degree(net, gmode = "digraph", cmode ="outdegree") #outdegree
graph_dens <- gden(net, g=NULL, mode="digraph", ignore.eval = F) #Density of a graph


##dataframe##dataframenet
edgelist_df <- as.data.frame(edgelist, unit = c("edges", "vertices"),
              na.rm = F,
              attrs_to_ignore = NA,
              name_vertices = T,
              sort_attrs = F,
              store_eid = F)



help("get.inducedSubgraph")

net <- get.inducedSubgraph(nrelations,v = which(!{nrelations %v% 'comb_org' %in% categories_to_filter}))
net_nodrop <- get.inducedSubgraph(nrelations,v = which({nrelations %v% 'comb_org'}))

network.vertex.names(edgelist)

categories_to_label <- c("NGO", "CBO")

plot(net,
     displaylabels=F,
     vertex.col='comb_org',
     vertex.cex=0.8)

#legend("topleft", 
      # legend = levels(factor(nodelist_merge$comb_org)), 
      # fill = category_colors,
      # cex = 0.4)

###SNA###
degree_cent <- degree(net) #NUMBER OF EDGES
degree_sna <- degree(nrelations, gmode = "digraph")
degree_sna_in <- degree(nrelations, gmode = "digraph", cmode ="indegree")
degree_sna_out <- degree(nrelations, gmode = "digraph", cmode ="outdegree")
