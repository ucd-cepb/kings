
library(igraph)
library(ggraph)
library(sna)
library(stringr)
library(dplyr)

source('govscicleaning.R')

edges_and_nodes <- list.files(path = "network_extracts", full.names = T)
gspids <- substr(edges_and_nodes, 18,21)

for(m in 1:length(edges_and_nodes)){
   
   edgenodelist <- readRDS(edges_and_nodes[m])
   
   #does not remove "California" prefixes
   
   #removing leading underscores, and US variations
   edgelist <- as.data.frame(edgenodelist$edgelist)
   edgelist$source <- str_remove(edgelist$source, "^_")
   edgelist$source <- str_remove(edgelist$source, '^(US_|U\\.S\\._|United_States_|UnitedStates_)')
   edgelist$target <- str_remove(edgelist$target, "^_")
   edgelist$target <- str_remove(edgelist$target, '^(US_|U\\.S\\._|United_States_|UnitedStates_)')
   nodelist <- as.data.frame(edgenodelist$nodelist)
   nodelist$entity_cat <- str_remove(nodelist$entity_cat, "^_")
   nodelist$entity_cat <- str_remove(nodelist$entity_cat, '^(US_|U\\.S\\._|United_States_|UnitedStates)')
   #removing non-persons/gpes/orgs
   edgelist <- edgelist %>% filter(source %in% nodelist$entity_cat & target %in% nodelist$entity_cat)
   
   abbr <- function(strng){
      
      if (!identical(grep(paste0("\b",strng,"\b"),govscitbl$Abbr,useBytes = F), integer(0))){
         return(govscitbl$Agency[grep(paste0("\b",strng,"\b"),govscitbl$Abbr, useBytes = F)] )
      }
      else
         return(strng)
   }
   
   #TODO fix orgtyp bug
   #orgtyp <- function(strng){
   # 
   #if (!identical(grep(paste0("\b",strng,"\b"),govscitbl$Agency, useBytes = F), integer(0))){
   # return(govscitbl$State[grep(paste0("\b",strng,"\b"),govscitbl$Agency, useBytes = F)])
   #}else if(!identical(grep(paste0("\b",strng,"\b"),govscitbl$Abbr, useBytes = F), integer(0))){
   # return(govscitbl$State[grep(paste0("\b",strng,"\b"),govscitbl$Abbr, useBytes = F)])
   #}else
   # return(NA)
   #}
   
   edgelist$source <- sapply(edgelist$source, abbr)
   edgelist$target <- sapply(edgelist$target, abbr)
   nodelist$entity_cat <- sapply(nodelist$entity_cat, abbr)
   
   colnames(nodelist)[3] <- "num_appearances"
   nodelist <- nodelist %>% arrange(desc(num_appearances))
   
   #get rid of duplicates
   nodelist <- nodelist %>%
      group_by(entity_cat) %>%
      arrange(desc(num_appearances)) %>%
      filter(row_number()==1)
   
   
   #nodelist$orglevel <- sapply(nodelist$entity_cat, orgtyp)
   #nodelist$type_subtype <- paste0(nodelist$entity_type, "_", nodelist$orglevel)
   #org type:
   #govscitbl$State[grep(paste0("\b",strng,"\b"),govscitbl$Abbr, useBytes = F)])
   
   #putting source and target first
   edgelist <- edgelist[,c(2:ncol(edgelist),1)]
   
   #use graph_from_data_frame because you can put node list as an argument
   full_directed_graph <- igraph::graph_from_data_frame(edgelist, vertices = nodelist, directed = T)
   
   full_directed_graph <- igraph::set_vertex_attr(full_directed_graph, "degr", value = igraph::degree(full_directed_graph))
   
   saveRDS(full_directed_graph, paste0("data_output/full_directed_graph_",gspids[m]))
   
   weighted_graph <- full_directed_graph
   
   weighted_graph <- igraph::delete_edge_attr(weighted_graph, "head_verb_id")
   weighted_graph <- igraph::delete_edge_attr(weighted_graph, "head_verb_tense")
   weighted_graph <- igraph::delete_edge_attr(weighted_graph, "head_verb_name")
   weighted_graph <- igraph::delete_edge_attr(weighted_graph, "head_verb_lemma")
   weighted_graph <- igraph::delete_edge_attr(weighted_graph, "parent_verb_id")
   weighted_graph <- igraph::delete_edge_attr(weighted_graph, "neg")
   weighted_graph <- igraph::delete_edge_attr(weighted_graph, "doc_sent_verb")
   weighted_graph <- igraph::delete_edge_attr(weighted_graph, "doc_sent_parent")
   igraph::E(weighted_graph)$weight <- 1
   weighted_graph <- igraph::simplify(weighted_graph, edge.attr.comb=list(weight="sum"), remove.loops = F)
   
   #uses original edges to calculate degree
   degs <- sort(igraph::degree(full_directed_graph),decreasing = T)
   topdegs <- names(degs[1:7])
   weighted_graph <- igraph::set_vertex_attr(weighted_graph, "labels", 
                                             value = ifelse(igraph::get.vertex.attribute(weighted_graph,"name") %in% topdegs, 
                                                            igraph::get.vertex.attribute(weighted_graph,"name"), NA))
   
   saveRDS(weighted_graph, paste0("data_output/to_weighted_graph_",gspids[m]))
}
