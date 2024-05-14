#Org type network structure

library(data.table)
library(stringr)
library(tidyverse)
library(dplyr)
library(plyr)
library(igraph)
library(devtools)
library(statnet)

#Set directory
file_loc <- 'C:\\Users\\hgsha\\Box Sync\\Kings_Large_Files\\data\\EJ_Paper\\cleaned_extracts_textgov_paper_version\\'
extract_list = list.files(file_loc)
setwd('C:\\Users\\hgsha\\Box Sync\\Kings_Large_Files\\data\\EJ_Paper\\cleaned_extracts_textgov_paper_version\\')
getwd()

#files
org_nodes <- read.csv("C:\\Users\\hgsha\\Desktop\\kings\\EJ_Paper\\Data\\Test\\merged_dt_4_24.csv")


#Create table
results <- data.frame((matrix(NA, nrow=126, ncol = 16)))
names(results) <- c("ID","Ambig", "CBO","Coalition","Consult",
                    "Drop","Follow", "GSA","Loc_Gov",
                    "NGO","NL_Gov","Private", "Project", 
                    "Research", "Spec_Dist","WC")
id_numbers <- seq(7,132)
results$ID <- sprintf("%03d", id_numbers)



results <- data.frame((matrix(NA, nrow=1, ncol = 18)))
names(results) <- c("Ambig", "CBO","Coalition","Consult",
                    "Drop","Follow", "GSA","Loc_Gov", "Nat_Am",
                    "NGO","NL_Gov", "Other", "Private", "Project", "RCD",
                    "Research", "Spec_Dist","WC")


for(file in extract_list[1:5]){
   print(file)
   temp = readRDS(file)
   #read in edgelist
   edges = temp$edgelist
   nodes = temp$nodelist
   #only use complete edges
   edge_complete <- temp$edgelist %>% filter(edgeiscomplete=="TRUE") %>%
      select(source, target)
   #remove GPE and People
   #grab_orgs <- function(file,drop = c('PERSON', 'GPE')){
      # read in rds file
    #  temp = readRDS(file)
      # isolate nodelist
     # nodes = temp$nodelist
      # filter out node types we don't want
      #nodes = nodes[!nodes$entity_type%in% drop,]
      #return(nodes)
   #}
   #nodes = for(file in extract_list[1]){
    #  print(file)
     # temp = grab_orgs(paste0(file_loc,file))
      #empty_node_dt <- rbind(empty_node_dt,temp,fill = T,use.names = T)
      #empty_node_dt = empty_node_dt[,list(num_appearances = sum(num_appearances)),
                                #    by=.(entity_name,entity_type)]
   #}
   #merge files
   nodelist_merge <- inner_join(temp$nodelist, org_nodes, by = c("entity_name", "entity_type"))
   net <- network(edge_complete, matrix.type="edgelist", loops = T, 
                  directed = T, duplicates = TRUE, multiple = T)
   duplicated_vertices <- nodelist_merge$entity_name[duplicated(nodelist_merge$entity_name)]
   if(length(duplicated_vertices)>0 ) {
      nodelist_merge <- nodelist_merge[!duplicated(nodelist_merge$entity_name),]
   }
   #remove missing vertex namesmissing_vertices <- setdiff(unique(edge_complete$source), unique(nodelist_merge$entity_name))
   missing_vertices <- setdiff(unique(edge_complete$source), unique(nodelist_merge$entity_name))
   print(missing_vertices)
   edge_complete_s <- edge_complete[!edge_complete$source %in% missing_vertices, ]
   
   missing_vertices_t <-setdiff(unique(edge_complete_s$target), unique(nodelist_merge$entity_name))
   print(missing_vertices_t)
   edge_complete_all <- edge_complete_s[!edge_complete_s$target %in% missing_vertices_t,]
   
   #try network again
   igr <-  igraph::graph_from_data_frame(d = edge_complete_all, 
                                         vertices = nodelist_merge, 
                                         directed = T)

   #view node attribute
   head(V(igr)$comb_org)
   head(V(igr)$entity_type)
   head(V(igr)$name)
   
   #try calculation
   count_org <- table(V(igr)$comb_org)
   print(count_org)
   results <- rbind(results, count_org)
   }
print(results)


#remove missing nodes
missing_vertices <- setdiff(unique(edge_complete_1$source), unique(nodelist_merge$entity_name))
print(missing_vertices)
edge_complete_s <- edge_complete_1[!edge_complete_1$source %in% missing_vertices, ]

missing_vertices_t <-setdiff(unique(edge_complete_s$target), unique(nodelist_merge$entity_name))
print(missing_vertices_t)
edge_complete_all <- edge_complete_s[!edge_complete_s$target %in% missing_vertices_t,]


missing_vertices_t <- setdiff(unique(edge_complete$target), unique(nodelist_merge$entity_name))
missing_vertices <- c(missing_vertices_t, missing_vertices)
edge_complete_1 <- edge_complete[!edge_complete$source %in% missing_vertices & !edge_complete$target
                                 %in% missing_vertices,]




??graph_from_data_frame

##DRAFT - IGNORE
for(file in extract_list[1:2]){
   print(file)
   temp = readRDS(file)
   edges = temp$edgelist
   edge_complete <-  temp$edgelist %>% filter(edgeiscomplete=="TRUE") %>%
      select(source, target)
   nodes=temp$nodelist
   net <- network(edge_complete, matrix.type="edgelist", loops = T, 
                  directed = T, duplicates = TRUE, multiple = T)
   igr <-  igraph::graph_from_data_frame(d = edge_complete, 
                                         vertices = nodes, 
                                         directed = T)
      results <- rbind(results, result_row)
}

org_nodes <- read.csv("EJ_Paper/Data/Test/merged_dt_4_24.csv")
nodelist_merge <- inner_join(empty_node_dt, org_nodes, by = c("entity_name", "entity_type"))
print(results)