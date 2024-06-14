
library(igraph)
library(ggraph)
library(sna)
library(stringr)
library(dplyr)
library(data.table)
library(pbapply)
library(stringi)
library(textNet)
filekey <- read.csv("filekey.csv")

###Section 1: Govscitbl####
source(filekey[filekey$var_name=="govscicleaning_script",]$filepath)

govscitbl$State <- textNet::clean_entities(govscitbl$State, remove_nums = T)
govscitbl$Agency <- textNet::clean_entities(govscitbl$Agency, remove_nums = T)
govscitbl$Abbr <- textNet::clean_entities(govscitbl$Abbr, remove_nums = T)
govscitbl <- unique(govscitbl)
saveRDS(govscitbl, filekey[filekey$var_name=="govsci_tbl_clean",]$filepath)
write.csv(govscitbl, paste0(filekey[filekey$var_name=="govsci_tbl_clean",]$filepath,".csv"))
###Section 2: GSAs####
edges_and_nodes <- list.files(path = filekey[filekey$var_name=="nondisambiged_extracts_govnetpaper",]$filepath, full.names = T)
gspids <- stringr::str_extract(edges_and_nodes,'[0-9]{1,}')

gsp_tblfilename <- filekey[filekey$var_name=="gsp_web_vars_repaired_stmpaper",]$filepath
gsp_tblfilenamesplits <- unlist(strsplit(gsp_tblfilename,split="/"))
gsp_tblpath <- paste(gsp_tblfilenamesplits[1:(length(gsp_tblfilenamesplits)-1)],collapse = "/")
gsp_tblpattern <- gsp_tblfilenamesplits[length(gsp_tblfilenamesplits)]

agency_tbl <- readRDS(list.files(path = gsp_tblpath, pattern = gsp_tblpattern, full.names = T)[
   length(list.files(path = gsp_tblpath, pattern = gsp_tblpattern, full.names = T))])
agency_tbl <- agency_tbl[!is.na(gsp_id),]
#change hyphens and spaces to underscores, to match spacy parse formatting
agency_tbl$name_gsas <- lapply(agency_tbl$name_gsas, function(w)
   stringr::str_replace_all(w,"-|\\s","_"))
#initialize empty dt
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

   if(m %in% c(38,39)){
      #this one is manually adjusted because the website lists them separately but the
      #GSP is actually combined
      agency_nicknames[(m*2-1):(m*2)]$name <- list(c(
         "City_of_Marysville_GSA","Cordua_Irrigation_District_GSA",
         "Yuba_Water_Agency_GSA"),c(
            "City_of_Marysville_GSA","Cordua_Irrigation_District_GSA",
            "Yuba_Water_Agency_GSA"))
      agency_nicknames[(m*2-1)]$nickname <- "Groundwater_Sustainability_Agencies"
      agency_nicknames[(m*2)]$nickname <- "Agencies"
   }
}

###Section 3: Acronyms####
acrons <- vector(mode="list",length=length(edges_and_nodes))
pdftxt <- readRDS(filekey[filekey$var_name=="cleaned_pdfs_for_govnetpaper",]$filepath)

for(m in 1:length(edges_and_nodes)){
   acrons[[m]] <-find_acronyms(pdftxt[[m]])
   acrons[[m]]$name <- clean_entities(acrons[[m]]$name)
   acrons[[m]]$acronym <- clean_entities(acrons[[m]]$acronym)
}
for(m in 1:length(edges_and_nodes)){
   acrons[[m]] <- rbind(list("Groundwater_Sustainability_Agencies","GSAs"),
         list("Groundwater_Sustainability_Agency","GSA"),
         list("Groundwater_Sustainability_Plan","GSP"),
         list("Groundwater_Sustainability_Plans","GSPs"),
         acrons[[m]])
   acrons[[m]] <- unique(acrons[[m]],by="acronym")
   #making sure that the GSA-related acronyms show up properly
}
###Section 4: Disambiguation####

govscitbl_mini <- unique(govscitbl[!is.na(Abbr) & nchar(Abbr)>0,c("Agency","Abbr")])
names(govscitbl_mini)=c("to","from")

customdt <- vector(mode="list",length=length(edges_and_nodes))
for(m in 1:length(edges_and_nodes)){
   names(acrons[[m]])=c("to","from")
   names(agency_nicknames)=c("to","from")
   customdt[[m]] <- rbind(acrons[[m]],govscitbl_mini)
   customdt[[m]] <- unique(customdt[[m]])
   match_partial_entity <- rep(c(T,F), c(nrow(customdt[[m]]),nrow(agency_nicknames[(m*2-1):(m*2)])))
   customdt[[m]] <- rbind(customdt[[m]],agency_nicknames[(m*2-1):(m*2)])
   customdt[[m]]$match_partial_entity <- match_partial_entity
   rm(match_partial_entity)
   fromgroups <- table(customdt[[m]]$from)
   #now we know the max number of identical is 2, from the agency_nicknames and something else
   fromgroups <- fromgroups[fromgroups>1]
   
   if(length(fromgroups)>1){
      #Subsection A resolves duplicates in from column#### 
      for(k in 1:length(fromgroups)){
         tos <- customdt[[m]][from==names(fromgroups)[k]]$to
         if(length(unique(str_remove(tos,"^United_States_|^US_|California_")))==1){
            keepto <- tos[str_detect(tos, "^United_States_|^US_|California_")]
            makefrom <- tos[!str_detect(tos, "^United_States_|^US_|California_")]
            if(length(makefrom)==0){
               #if one is "United_States" and the other is "US"
               keepto <- keepto[1]
               makefrom <- keepto[2]
            }
         }else if(length(unique(nchar(tos)))==2){
            keepto <- tos[nchar(tos)!=max(nchar(tos))]
            makefrom <- tos[nchar(tos)==max(nchar(tos))]
         }else{
            keepto <- tos[1]
            makefrom <- tos[2]
         }
         match_partial <- ifelse(F %in% customdt[[m]][from %in% names(fromgroups)[k]]$match_partial_entity, F, T)
         customdt[[m]] <- customdt[[m]][!(from %in% names(fromgroups)[k])] 
         
         
         customdt[[m]] <- rbind(customdt[[m]], list(keepto, names(fromgroups)[k], match_partial))
         if(!makefrom %in% customdt[[m]]$from){
            customdt[[m]] <- rbind(customdt[[m]], list(keepto, makefrom, match_partial))
         }
      }
      
   }
}
for(m in 1:length(edges_and_nodes)){
   print(m)
   #should not drop "us" from custom list, or from nodelist/edgelist. however, if it doesn't match "us" on
   #drop "us" from the nodelist/edgelist and try again to match with the custom list
   try_drop <- "^US_|^U_S_|^United_States_|^UnitedStates_"
   edgenodelist <- readRDS(edges_and_nodes[m])
   edgenodelist <- disambiguate(from=customdt[[m]]$from, to=customdt[[m]]$to, 
                                    match_partial_entity=customdt[[m]]$match_partial_entity, textnet_extract = edgenodelist, try_drop = try_drop)
   saveRDS(edgenodelist,paste0(filekey[filekey$var_name=="disambiged_extracts_govnetpaper",]$filepath,"/",gspids[m],".RDS"))
}

for(m in 1:length(edges_and_nodes)){
   edgenodelist <- readRDS(paste0(filekey[filekey$var_name=="disambiged_extracts_govnetpaper",]$filepath,"/",gspids[m],".RDS"))
###Orgtype (not currently implemented)####   
   #TODO fix orgtyp bug
   #orgtyp <- function(strng){
   # 
   #if (!identical(grep(paste0("\b",strng,"\b"),govscitbl_mini$Agency, useBytes = F), integer(0))){
   # return(govscitbl_mini$State[grep(paste0("\b",strng,"\b"),govscitbl_mini$Agency, useBytes = F)])
   #}else if(!identical(grep(paste0("\b",strng,"\b"),govscitbl_mini$Abbr, useBytes = F), integer(0))){
   # return(govscitbl_mini$State[grep(paste0("\b",strng,"\b"),govscitbl_mini$Abbr, useBytes = F)])
   #}else
   # return(NA)
   #}
   #nodelist$orglevel <- sapply(nodelist$entity_name, orgtyp)
   #nodelist$type_subtype <- paste0(nodelist$entity_type, "_", nodelist$orglevel)
   #org type:
   #govscitbl_mini$State[grep(paste0("\b",strng,"\b"),govscitbl_mini$Abbr, useBytes = F)])
###Section 4 Continued####   
   edgelist <- edgenodelist$edgelist
   nodelist <- edgenodelist$nodelist
   #only using complete edges
   edgelist <- edgelist %>% filter(!is.na(source) & !is.na(target))
   #only using edges and nodes where each node has more than one a-z letter
   edgelist$esletters <- str_remove_all(string = edgelist$source, pattern = "[^a-z_]")
   edgelist$etletters <- str_remove_all(string = edgelist$target, pattern = "[^a-z_]")
   nodelist$nletters <- str_remove_all(string = nodelist$entity_name, pattern = "[^a-z_]")
   
   nodelist <- nodelist %>% filter(nchar(nletters)>1)
   edgelist <- edgelist %>% filter(nchar(esletters)>1 & nchar(etletters)>1)
   edgelist$esletters <- NULL
   edgelist$etletters <- NULL
   nodelist$nletters <- NULL
   #putting source and target first
   edgelist <- edgelist[,.SD,.SDcols=c(2:ncol(edgelist),1)]
   
   
###Section 5: Network Object Generation####
   #use graph_from_data_frame because you can put node list as an argument
   full_directed_graph <- igraph::graph_from_data_frame(edgelist, vertices = nodelist, directed = T)
   
   full_directed_graph <- igraph::set_vertex_attr(full_directed_graph, "degr", value = igraph::degree(full_directed_graph))
   
   saveRDS(full_directed_graph, paste0(filekey[filekey$var_name=="full_directed_graphs_govnetpaper",]$filepath,gspids[m]))
   
   weighted_graph <- full_directed_graph
   
   weighted_graph <- igraph::delete_edge_attr(weighted_graph, "head_verb_id")
   weighted_graph <- igraph::delete_edge_attr(weighted_graph, "head_verb_tense")
   weighted_graph <- igraph::delete_edge_attr(weighted_graph, "head_verb_name")
   weighted_graph <- igraph::delete_edge_attr(weighted_graph, "head_verb_lemma")
   weighted_graph <- igraph::delete_edge_attr(weighted_graph, "parent_verb_id")
   weighted_graph <- igraph::delete_edge_attr(weighted_graph, "neg")
   weighted_graph <- igraph::delete_edge_attr(weighted_graph, "doc_sent_verb")
   weighted_graph <- igraph::delete_edge_attr(weighted_graph, "doc_sent_parent")
   weighted_graph <- igraph::delete_edge_attr(weighted_graph, "helper_lemma")
   weighted_graph <- igraph::delete_edge_attr(weighted_graph, "helper_token")
   weighted_graph <- igraph::delete_edge_attr(weighted_graph, "xcomp_verb")
   weighted_graph <- igraph::delete_edge_attr(weighted_graph, "xcomp_helper_lemma")
   weighted_graph <- igraph::delete_edge_attr(weighted_graph, "xcomp_helper_token")
   weighted_graph <- igraph::delete_edge_attr(weighted_graph, "edgeiscomplete")
   weighted_graph <- igraph::delete_edge_attr(weighted_graph, "has_hedge")
   weighted_graph <- igraph::delete_edge_attr(weighted_graph, "is_future")
   
   
   igraph::E(weighted_graph)$weight <- 1
   weighted_graph <- igraph::simplify(weighted_graph, edge.attr.comb=list(weight="sum"), remove.loops = F)
   
   #uses original edges to calculate degree
   degs <- sort(igraph::degree(full_directed_graph),decreasing = T)
   topdegs <- names(degs[1:7])
   weighted_graph <- igraph::set_vertex_attr(weighted_graph, "labels", 
                                             value = ifelse(igraph::get.vertex.attribute(weighted_graph,"name") %in% topdegs, 
                                                            igraph::get.vertex.attribute(weighted_graph,"name"), NA))
   
   saveRDS(weighted_graph, paste0(filekey[filekey$var_name=="weighted_nets_govnetpaper",]$filepath,gspids[m]))
}
