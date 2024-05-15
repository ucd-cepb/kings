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
empty_df <- data.frame()


results <- data.frame((matrix(, nrow=1, ncol = 18)))
names(results) <- c("Ambig", "CBO","Coalition","Consult",
                    "Drop","Follow", "GSA","Loc_Gov", "Nat_Am",
                    "NGO","NL_Gov", "Other", "Private", "Project", "RCD",
                    "Research", "Spec_Dist","WC")


for(file in extract_list[1]){
   print(file)
   temp = readRDS(file)
   #read in edgelist
   edges = temp$edgelist
   nodes = temp$nodelist
   #only use complete edges
   edge_complete <- temp$edgelist %>% filter(edgeiscomplete=="TRUE") %>%
      select(source, target)
   #merge nodelist
   nodelist_merge <- inner_join(temp$nodelist, org_nodes, by = c("entity_name", "entity_type"))
   #create networks
   net <- network(edge_complete, matrix.type="edgelist", loops = T, 
                  directed = T, duplicates = TRUE, multiple = T)
   #remove duplicates for igraph
   duplicated_vertices <- nodelist_merge$entity_name[duplicated(nodelist_merge$entity_name)]
   if(length(duplicated_vertices)>0 ) {
      nodelist_merge <- nodelist_merge[!duplicated(nodelist_merge$entity_name),]
   }
   #Remove misaligned vertex/edge items
   missing_vertices <- setdiff(unique(edge_complete$source), unique(nodelist_merge$entity_name))
   edge_complete_s <- edge_complete[!edge_complete$source %in% missing_vertices, ]
   
   missing_vertices_t <-setdiff(unique(edge_complete_s$target), unique(nodelist_merge$entity_name))
   edge_complete_all <- edge_complete_s[!edge_complete_s$target %in% missing_vertices_t,]
   
   #try network again
   igr <-  igraph::graph_from_data_frame(d = edge_complete_all, 
                                         vertices = nodelist_merge, 
                                         directed = T)

   #view node attribute
   head(V(igr)$comb_org)
   head(V(igr)$entity_type)
   head(V(igr)$name)
   
   #make objects out of node attributes
   vertex_names <- V(igr)$name
   comb_org <- V(igr)$comb_org
   
   #make df out of selected items
   node_attr <- data.frame(Name = vertex_names, comb_org = comb_org)
   edgelist <- as.data.frame(as_edgelist(igr))
   
   org_counts <- table(node_attr$comb_org)
   print(org_counts)
   org_counts_df <- as.data.frame(org_counts)
   class(org_counts_df)
   row.names(org_counts_df) <- org_counts_df$Var1
   org_counts_df <- subset(org_counts_df, select = -Var1)
   t_org <- t(org_counts_df)
   t_org_df <- as.data.frame(t_org)
   
#merge data
   columns <- colnames(results)
   for (col in columns) {
      if(col %in% colnames(t_org_df)) {
         results[[col]] <- t_org_df[[col]]
      } else {
         results[[col]] <- rep(0, nrow(results))
      }
   }
   print(results)
   
}


   
  # count_org <- table(V(igr)$comb_org)
   #print(count_org)
   #rbind works when just binding this and an empty dataframe

      #try calculation
   #empty_df<- data.frame()
   #for (file in extract_list[1]) {
    #  net.list = readRDS(file)
     # tb = table(V(igr)$comb_org)
      #empty_df <- plyr::rbind.fill(empty_df, data.frame(rbind(tb)))
   }
   }
print(results)
?rbind
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
for(file in extract_list[1:3]){
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