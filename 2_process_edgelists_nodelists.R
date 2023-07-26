
library(igraph)
library(ggraph)
library(sna)
library(stringr)
library(dplyr)

###Section 1: Govscitbl####
source('govscicleaning.R')

govscitbl$State <- clean_entities(govscitbl$State, remove_nums = T)
govscitbl$Agency <- clean_entities(govscitbl$Agency, remove_nums = T)
govscitbl$Abbr <- clean_entities(govscitbl$Abbr, remove_nums = T)
govscitbl <- unique(govscitbl)


###Section 2: GSAs####
edges_and_nodes <- list.files(path = "network_extracts", full.names = T)
gspids <- substr(edges_and_nodes, 18,21)

agency_tbl <- readRDS(list.files(path = "data_output", pattern = "web_repaired", full.names = T)[
   length(list.files(path = "data_output", pattern = "web_repaired", full.names = T))])
agency_tbl <- agency_tbl[!is.na(gsp_id),]
#change hyphens and spaces to underscores, to match spacy parse formatting
agency_tbl$name_gsas <- lapply(agency_tbl$name_gsas, function(w)
   stringr::str_replace_all(w,"-|\\s","_"))

agency_nicknames <- setDT(list("name"=rep(vector(mode="list",length(edges_and_nodes)*2)),
                               "nickname"=rep(NA_character_,length(edges_and_nodes)*2)))
for(m in 1:length(edges_and_nodes)){
   #remove parentheses
   agency_names <- agency_tbl[gsp_id==gspids[m]]$name_gsas[[1]]
   agency_names <- unlist(lapply(agency_names, function(b) str_split(b,"\\(")[[1]][1]))
   #clean entities so they are same format as spacy tokens
   agency_names <- clean_entities(agency_names, remove_nums=T)
   plural <- agency_tbl[gsp_id==gspids[m]]$mult_gsas
   
   if(plural==T){
      agency_abbr <- c("Groundwater_Sustainability_Agencies","Agencies")
   }else{
      agency_abbr <- c("Groundwater_Sustainability_Agency","Agency")
   }
   agency_nicknames[m*2-1, (colnames(agency_nicknames)):= list(agency_names, agency_abbr[1])]
   agency_nicknames[m*2, (colnames(agency_nicknames)):= list(agency_names, agency_abbr[2])]
}

###Section 3: Acronyms####

#add GSAs -> Groundwater_Sustainability_Agencies
###Section 4: Disambiguation####



pdftxt <- readRDS("data_output/cleaned_pdfs")


for(m in 1:length(edges_and_nodes)){
   edgenodelist <- readRDS(edges_and_nodes[m])
   acrons <- find_acronyms(pdftxt[[m]])
   customdt <- rbind(acrons,govscitbl,agency_nicknames[(m*2-1):(m*2)])
   match_partial_entity <- rep(c(T,F,F), c(nrow(acrons),nrow(govscitbl),nrow(agency_nicknames[(m*2-1):(m*2)])))
   prefixes <- list(c("^_*The_","^_*the_","^_*THE_","^_*The$","^_*the$","^_*THE$"), c("US_|U_S_|United_States_|UnitedStates"))
   edgenodelist_dis <- disambiguate(from=customdt[,2], to=customdt[,1], match_partial_entity, edgenodelist, prefixes)
   
   #does not remove "California" prefixes
 
   
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
   
   
   ###Section 5: Network Object Generation####
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
