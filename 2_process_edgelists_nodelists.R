
library(igraph)
library(ggraph)
library(sna)
library(stringr)
library(dplyr)

source('govscicleaning.R')

edges_and_nodes <- list.files(path = "network_extracts", full.names = T)
gspids <- substr(edges_and_nodes, 18,21)
govscitbl$Agency <- clean_entities(govscitbl$Agency, remove_nums = T)
govscitbl$Abbr <- clean_entities(govscitbl$Abbr, remove_nums = T)
govscitbl <- unique(govscitbl)

gsp_text_with_meta <- readRDS("data_output/gsp_docs_w_meta")
agency_tbl <- readRDS(list.files(path = "data_output", pattern = "web_repaired", full.names = T)[
   length(list.files(path = "data_output", pattern = "web_repaired", full.names = T))])
agency_tbl <- agency_tbl[!is.na(gsp_id),]

#change hyphens and spaces to underscores, to match spacy parse formatting
agency_tbl$name_gsas <- lapply(agency_tbl$name_gsas, function(w)
   stringr::str_replace_all(w,"-|\\s","_"))

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
   
   agency_disambig <- function(strng,m){
      #remove parentheses
      agency_names <- agency_tbl[gsp_id==gspids[m]]$name_gsas[[1]]
      agency_names <- unlist(lapply(agency_names, function(b) str_split(b,"\\(")[[1]][1]))
      #clean entities so they are same format as spacy tokens
      agency_names <- clean_entities(agency_names, remove_nums=T)
      plural <- agency_tbl[gsp_id==gspids[m]]$mult_gsas
      if((plural==T && !is.na(strng) && (tolower(strng) %in% c("the_gsas","the_agencies"))) | 
         (!plural==T && is.na(strng) && (tolower(strng) %in% c("the_gsa","the_agency")))){
         return(agency_names)
      }else if (is.na(strng)){
         return(NA_character_)
      }else{
         return(strng)
      }
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
   

   #temp rows because changing character column into list column
   edgelist$sourcetemp <- lapply(edgelist$source, function(strng) agency_disambig(strng,m))
   edgelist$targettemp <- lapply(edgelist$target, function(strng) agency_disambig(strng,m))
   edgelist$length_source <- sapply(edgelist$sourcetemp, length)
   edgelist$length_target <- sapply(edgelist$targettemp, length)
   rows <- edgelist[rep(seq(1, nrow(edgelist)), edgelist$length_source)]
   rows$source <- unlist(edgelist$sourcetemp)
   rowstarget <- rows[rep(seq(1, nrow(rows)), rows$length_target)]
   rowstarget$target <- unlist(rows$targettemp)
      
   rowstarget$sourcetemp <- rowstarget$targettemp <- rowstarget$length_source <- rowstarget$length_target<- NULL
   edgelist <- rowstarget
   
   nodelist$entity_cattemp <- lapply(nodelist$entity_cat, function(strng) agency_disambig(strng,m))
   nodelist$length_entitycat <- sapply(nodelist$entity_cattemp, length)
   rows <- nodelist[rep(seq(1, nrow(nodelist)), nodelist$length_entitycat)]
   rows$entity_cat <- unlist(nodelist$entity_cattemp)
   rows$entity_cattemp <- rows$length_entitycat <- NULL
   nodelist <- rows
 
   remove_the <- function(v){
      remove <- c("^_*The_","^_*the_","^_*THE_","^_*The$","^_*the$","^_*THE$")
      index <- which(grepl(paste(remove,collapse = '|'),v,perl = T))
      v[index] <- str_remove_all(v[index],paste(remove,collapse = '|'))
      return(v)
   }
   
   edgelist$source <- remove_the(edgelist$source)
   edgelist$target <- remove_the(edgelist$target)
   nodelist$entity_cat <- remove_the(nodelist$entity_cat)  
   
   abbr <- function(strng){
      
      if (!identical(grep(paste0("\b",strng,"\b"),govscitbl$Abbr,useBytes = F), integer(0))){
         return(govscitbl$Agency[grep(paste0("\b",strng,"\b"),govscitbl$Abbr, useBytes = F)] )
      }
      else
         return(strng)
   }
   
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
