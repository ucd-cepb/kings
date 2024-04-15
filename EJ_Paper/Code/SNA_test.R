#note: not ready to run!
#This script should only be run after the EJ orgs database is ready
library(igraph)
library(devtools)
install.packages("ggraph")
install_github("https://github.com/thomasp85/ggraph")
library(ggraph)
library(sna)
library(stringr)
library(dplyr)
library(data.table)
library(pbapply)
library(stringi)
library(data.table)
library(stringr)
library(tidyverse)
library(dplyr)
library(plyr)
library(ggplot2)
###Set up
file_loc <- 'C:\\Users\\hgsha\\Box Sync\\Kings_Large_Files\\data\\EJ_Paper\\cleaned_extracts_textgov_paper_version\\'
extract_list = list.files(file_loc)

###SNA
n = readRDS('C:\\Users\\hgsha\\Box Sync\\Kings_Large_Files\\data\\EJ_Paper\\cleaned_extracts_textgov_paper_version\\0007.RDS')
#n = readRDS('data\\Text_Gov_Network_Methods_Paper\\cleaned_extracts\\0007.RDS')
head(n$nodelist)
head(n$edgelist)

edges_007<- n$edgelist
nodes_007 <- n$nodelist
nodes_007 <- nodes_007[nodes_007$entity_type != 'PERSON']

#Join nodelist with my list
org_nodes <- read.csv("EJ_Paper/Data/Test/merged_dt.csv")
#Does not include GPE, only orgs
nodelist_merge <- inner_join(nodes_007, org_nodes, by = c("entity_name", "entity_type"))

network::as.network(edges_007) ###this throws up an error that there are NA values
###Need to ask how to run things when there is missing data


###Test with complete edges - walking through iGraph vignette
###https://ona-book.org/gitbook/restructuring-data.html
edge_test <- edges_007 %>% filter(edgeiscomplete=="TRUE") %>%
   select(source, target)

edge_m <- as.matrix(edge_test)
isSymmetric(edge_m)
colnames(edge_m) <- rownames(edge_m)
nodeinfo <- read.csv("EJ_Paper/Data/Test/merged_dt.csv")
head(nodeinfo)

nrelations <- network(edge_test, loops = T, 
                      directed = T, duplicates = TRUE, multiple = T)
summary(nrelations)
list.vertex.attributes(nrelations)
nrelations%v%"vertex.names"

edgelist <- network(edge_test, matrix.type="edgelist", loops = T, 
        directed = T, duplicates = TRUE, multiple = T)
edgelist


###Graphing
library(viridis)
#graph_from_adjacency_matrix(edge_m, mode="directed")

gplot(edgelist)

category_colors <- rainbow(length(unique(nodeinfo$comb_org)))
category_to_color <- category_colors[as.numeric(factor(nodeinfo$comb_org))]
plot(nrelations, displaylabels=F, vertex.col=category_to_color, vertex.cex=1)
legend("topright", legend = levels(factor(nodeinfo$comb_org)), fill = category_colors)

#filtered categories
categories_to_filter <- c("Ambig", "Follow")
filtered_nodeinfo <- nodeinfo[!(nodeinfo$comb_org %in% categories_to_filter), ]
plot(nrelations, 
     displaylabels=F, 
     vertex.col=category_to_color[!(nodeinfo$comb_org %in% categories_to_filter)],
     vertex.cex=1)
legend("topleft", 
       legend = levels(factor(nodeinfo$comb_org)), 
       fill = category_colors,
       cex = 0.4)

###Drop "Drop" Category

categories_to_filter <- c("Ambig", "Follow", "Drop")
filtered_nodeinfo <- nodeinfo[!(nodeinfo$comb_org %in% categories_to_filter), ]
categories_to_label <- c("NGO", "CBO")

plot(nrelations, 
     displaylabels=F, 
     vertex.col=category_to_color[!(nodeinfo$comb_org %in% categories_to_filter)],
     vertex.cex=0.8,
     )

legend("topleft", 
       legend = levels(factor(nodeinfo$comb_org)), 
       fill = category_colors,
       cex = 0.4)

####################

#Try to run sna statistics - chapter 6.2 in book

g14 <- igraph::graph_from_data_frame(nrelations, direct = F)
igraph::degree(g14)
igraph::ego(g14, order = 2, nodes = "aliso_water_district")
igraph::ego_size(g14, order=2, nodes = "aliso_water_district")

igraph::closeness(g14, vids = c("subbasin","aliso_water_district"))
igraph::betweenness(g14, v = c("subbasin","aliso_water_district"))

eigens <- igraph::eigen_centrality(g14, scale = F)
eigens$vector

#############################Things that did not work
edge_matrix <- network::as.network(edge_test, loops = T, 
                    directed = T, duplicates = TRUE, multiple = T)
edge_matrix[1:10, 1:10]
edge_matrix <- as.matrix(edge_matrix)
isSymmetric(edge_matrix)

graph_from_edgelist(edge_matrix, directed=T)


full_directed_graph <- igraph::graph_from_data_frame(edge_test, vertices = nodelist_merge, directed = T)
