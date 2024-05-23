#Tyler/my script

library(data.table)
library(stringr)
library(tidyverse)
library(dplyr)
library(plyr)
library(igraph)
library(devtools)
library(statnet)
library(ggplot2)
#Set directory
file_loc <- 'data/EJ_Paper/cleaned_extracts_textgov_paper_version/'
extract_list = list.files(file_loc)
#files
org_nodes <- read.csv("EJ_Paper/Data/Test/merged_dt_4_24.csv")
#Create table
#Create table
empty_df <- data.frame()

#Start of the loop
for(file in extract_list){
   print(file)
   #file = extract_list[1]
   temp = readRDS(paste0(file_loc,file))
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
   org_counts_df <- data.frame(rbind(org_counts))
   motif_counts_df <- data.frame(rbind(summary(net ~ edges + triangles + twopath)))
   graph_stats_df <- data.frame(network_density = network.density(net),degree_centralization = sna::centralization(net,sna::degree))
   stats_df = cbind(org_counts_df,motif_counts_df,graph_stats_df)
   stats_df$FILE = file
   empty_df <- plyr::rbind.fill(empty_df,stats_df)
}
empty_df

#Summary degree centralization; ##NEED TO RUN HISTOGRAM
Central <- ggplot(empty_df, aes(x = degree_centralization)) +
   geom_histogram(binwidth = 0.05, fill = "lightblue", color = "black") + scale_x_continuous(limits = c(0,1)) +
   labs(x = "Centralization: Degree", y = "Frequency", title = "Histogram of Centralization: Degree") +
   theme_minimal()

##Percentages - filtered out ambig, follow up, and drop
org_type <- c("CBO","Coalition","Consult",
               "GSA","Loc_Gov", "Nat_Am",
               "NGO","NL_Gov", "Other", "Private", "Project", "RCD",
               "Research", "Spec_Dist","WC")
org_type_df <- empty_df[, org_type]

org_type_df <- org_type_df %>% mutate_all(
   ~ifelse(is.na(.),0, .)) #works

percentage_df <- as.data.frame(lapply(
   org_type_df, function(x) x / rowSums(org_type_df)*100))

#appropriate packages
library(ggplot2)

head(percentage_df)
library(tidyr)

melt_version = tidyr::pivot_longer(percentage_df,cols = which(colnames(percentage_df) %in% org_type))
ggplot(melt_version,aes(x = value,fill = name)) + geom_histogram()+
   facet_wrap(~name) + theme_bw() + guides(fill = F)


#Create histogram
CBO_perc <- ggplot(percentage_df, aes(x = CBO)) +
   geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
   labs(x = "Percent", y = "Frequency", title = "Histogram of CBO Percentages") +
   theme_minimal()
Coal_perc<- ggplot(percentage_df, aes(x = Coalition)) +
   geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
   labs(x = "Percent", y = "Frequency", title = "Histogram of Coalition Percentages") +
   theme_minimal()
Consult_perc<-ggplot(percentage_df, aes(x = Consult)) +
   geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
   labs(x = "Percent", y = "Frequency", title = "Histogram of Consult Percentages") +
   theme_minimal()
LocalGov_perc<-ggplot(percentage_df, aes(x = Loc_Gov)) +
   geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
   labs(x = "Percent", y = "Frequency", title = "Histogram of Local Gov Percentages") +
   theme_minimal()
GSA_perc<-ggplot(percentage_df, aes(x = GSA)) +
   geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
   labs(x = "Percent", y = "Frequency", title = "Histogram of GSA Percentages") +
   theme_minimal()
Nat_am_perc <- ggplot(percentage_df, aes(x = Nat_Am)) +
   geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
   labs(x = "Percent", y = "Frequency", title = "Histogram of Native American Tribes Percentages") +
   theme_minimal()
NGO_perc<-ggplot(percentage_df, aes(x = NGO)) +
   geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
   labs(x = "Percent", y = "Frequency", title = "Histogram of NGO Percentages") +
   theme_minimal()
NL_gov_perc <- ggplot(percentage_df, aes(x = NL_Gov)) +
   geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
   labs(x = "Percent", y = "Frequency", title = "Histogram of Non-Local Gov Percentages") +
   theme_minimal()
Private_perc<-ggplot(percentage_df, aes(x = Private)) +
   geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
   labs(x = "Percent", y = "Frequency", title = "Histogram of Private Percentages") +
   theme_minimal()
Project_perc <- ggplot(percentage_df, aes(x = Project)) +
   geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
   labs(x = "Percent", y = "Frequency", title = "Histogram of Project Percentages") +
   theme_minimal()
RCD_perc <- ggplot(percentage_df, aes(x = RCD)) +
   geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
   labs(x = "Percent", y = "Frequency", title = "Histogram of RCD Percentages") +
   theme_minimal()
Research_perc <- ggplot(percentage_df, aes(x = Research)) +
   geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
   labs(x = "Percent", y = "Frequency", title = "Histogram of Research Percentages") +
   theme_minimal()
SD_perc <- ggplot(percentage_df, aes(x = Spec_Dist)) +
   geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
   labs(x = "Percent", y = "Frequency", title = "Histogram of Special District Percentages") +
   theme_minimal()
WC_perc <- ggplot(percentage_df, aes(x = WC)) +
   geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
   labs(x = "Percent", y = "Frequency", title = "Histogram of Water Company Percentages") +
   theme_minimal()

#combine percent histogram
install.packages("patchwork")
library(patchwork)

combined_plot <- CBO_perc + Coal_perc + Consult_perc + LocalGov_perc +
   GSA_perc + Nat_am_perc + NGO_perc + NL_gov_perc + Private_perc + Project_perc +
   RCD_perc + Research_perc + SD_perc + WC_perc

#org type WITH DROP
##Percentages - filtered out ambig, follow up, and drop
org_type_drop <- c("CBO","Coalition","Consult", "Drop",
              "GSA","Loc_Gov", "Nat_Am",
              "NGO","NL_Gov", "Other", "Private", "Project", "RCD",
              "Research", "Spec_Dist","WC")
org_type_df_drop <- empty_df[, org_type_drop]

org_type_df_drop <- org_type_df_drop %>% mutate_all(
   ~ifelse(is.na(.),0, .)) #works

percentage_df_d <- as.data.frame(lapply(
   org_type_df_drop, function(x) x / rowSums(org_type_df_drop)*100))


#HISTOGRAM WITH DROP
CBO_perc <- ggplot(percentage_df_d, aes(x = CBO)) +
   geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
   labs(x = "Percent", y = "Frequency", title = "Histogram of CBO Percentages") +
   theme_minimal()
Coal_perc<- ggplot(percentage_df_d, aes(x = Coalition)) +
   geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
   labs(x = "Percent", y = "Frequency", title = "Histogram of Coalition Percentages") +
   theme_minimal()
Consult_perc<-ggplot(percentage_df_d, aes(x = Consult)) +
   geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
   labs(x = "Percent", y = "Frequency", title = "Histogram of Consult Percentages") +
   theme_minimal()
LocalGov_perc<-ggplot(percentage_df_d, aes(x = Loc_Gov)) +
   geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
   labs(x = "Percent", y = "Frequency", title = "Histogram of Local Gov Percentages") +
   theme_minimal()
GSA_perc<-ggplot(percentage_df_d, aes(x = GSA)) +
   geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
   labs(x = "Percent", y = "Frequency", title = "Histogram of GSA Percentages") +
   theme_minimal()
Nat_am_perc <- ggplot(percentage_df_d, aes(x = Nat_Am)) +
   geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
   labs(x = "Percent", y = "Frequency", title = "Histogram of Native American Tribes Percentages") +
   theme_minimal()
NGO_perc<-ggplot(percentage_df_d, aes(x = NGO)) +
   geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
   labs(x = "Percent", y = "Frequency", title = "Histogram of NGO Percentages") +
   theme_minimal()
NL_gov_perc <- ggplot(percentage_df_d, aes(x = NL_Gov)) +
   geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
   labs(x = "Percent", y = "Frequency", title = "Histogram of Non-Local Gov Percentages") +
   theme_minimal()
Private_perc<-ggplot(percentage_df_d, aes(x = Private)) +
   geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
   labs(x = "Percent", y = "Frequency", title = "Histogram of Private Percentages") +
   theme_minimal()
Project_perc <- ggplot(percentage_df_d, aes(x = Project)) +
   geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
   labs(x = "Percent", y = "Frequency", title = "Histogram of Project Percentages") +
   theme_minimal()
RCD_perc <- ggplot(percentage_df_d, aes(x = RCD)) +
   geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
   labs(x = "Percent", y = "Frequency", title = "Histogram of RCD Percentages") +
   theme_minimal()
Research_perc <- ggplot(percentage_df_d, aes(x = Research)) +
   geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
   labs(x = "Percent", y = "Frequency", title = "Histogram of Research Percentages") +
   theme_minimal()
SD_perc <- ggplot(percentage_df_d, aes(x = Spec_Dist)) +
   geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
   labs(x = "Percent", y = "Frequency", title = "Histogram of Special District Percentages") +
   theme_minimal()
WC_perc <- ggplot(percentage_df_d, aes(x = WC)) +
   geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
   labs(x = "Percent", y = "Frequency", title = "Histogram of Water Company Percentages") +
   theme_minimal()
Drop_perc <- ggplot(percentage_df_d, aes(x = Drop)) +
   geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
   labs(x = "Percent", y = "Frequency", title = "Histogram of Drop Percentages") +
   theme_minimal()

#combine percent histogram
library(patchwork)

combined_plot <- CBO_perc + Coal_perc + Consult_perc + LocalGov_perc +
   GSA_perc + Nat_am_perc + NGO_perc + NL_gov_perc + Private_perc + Project_perc +
   RCD_perc + Research_perc + SD_perc + WC_perc + Drop_perc



   

##count histograms
org_type <- c("CBO","Coalition","Consult",
"GSA","Loc_Gov", "Nat_Am",
"NGO","NL_Gov", "Other", "Private", "Project", "RCD",
"Research", "Spec_Dist","WC")
org_type_df <- empty_df[, org_type]

org_type_df <- org_type_df %>% mutate_all(
   ~ifelse(is.na(.),0, .))

###Create histogram
CBO_count <- ggplot(org_type_df, aes(x = CBO)) +
   geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
   labs(x = "Count", y = "Frequency", title = "Histogram of CBO Count") +
   theme_minimal()
Coal_count<- ggplot(org_type_df, aes(x = Coalition)) +
   geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
   labs(x = "Count", y = "Frequency", title = "Histogram of Coalition Count") +
   theme_minimal()
Consult_count<-ggplot(org_type_df, aes(x = Consult)) +
   geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
   labs(x = "Count", y = "Frequency", title = "Histogram of Consult Count") +
   theme_minimal()
LocalGov_count<-ggplot(org_type_df, aes(x = Loc_Gov)) +
   geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
   labs(x = "Count", y = "Frequency", title = "Histogram of Local Gov Count") +
   theme_minimal()
GSA_count<-ggplot(org_type_df, aes(x = GSA)) +
   geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
   labs(x = "Count", y = "Frequency", title = "Histogram of GSA Count") +
   theme_minimal()
Nat_am_count <- ggplot(org_type_df, aes(x = Nat_Am)) +
   geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
   labs(x = "Count", y = "Frequency", title = "Histogram of Native American Tribes Count") +
   theme_minimal()
NGO_count<-ggplot(org_type_df, aes(x = NGO)) +
   geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
   labs(x = "Count", y = "Frequency", title = "Histogram of NGO Count") +
   theme_minimal()
NL_gov_count <- ggplot(org_type_df, aes(x = NL_Gov)) +
   geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
   labs(x = "Count", y = "Frequency", title = "Histogram of Non-Local Gov Count") +
   theme_minimal()
Private_count<-ggplot(org_type_df, aes(x = Private)) +
   geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
   labs(x = "Count", y = "Frequency", title = "Histogram of Private Count") +
   theme_minimal()
Project_count <- ggplot(org_type_df, aes(x = Project)) +
   geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
   labs(x = "Count", y = "Frequency", title = "Histogram of Project Count") +
   theme_minimal()
RCD_count <- ggplot(org_type_df, aes(x = RCD)) +
   geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
   labs(x = "Count", y = "Frequency", title = "Histogram of RCD Count") +
   theme_minimal()
Research_count <- ggplot(org_type_df, aes(x = Research)) +
   geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
   labs(x = "Count", y = "Frequency", title = "Histogram of Research Count") +
   theme_minimal()
SD_count <- ggplot(org_type_df, aes(x = Spec_Dist)) +
   geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
   labs(x = "Count", y = "Frequency", title = "Histogram of Special District Count") +
   theme_minimal()
WC_count <- ggplot(org_type_df, aes(x = WC)) +
   geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
   labs(x = "Count", y = "Frequency", title = "Histogram of Water Company Count") +
   theme_minimal()

#combine percent histogram
library(patchwork)

combined_plot_count <- CBO_count + Coal_count + Consult_count + LocalGov_count +
   GSA_count + Nat_am_count + NGO_count + NL_gov_count + Private_count + Project_count +
   RCD_count + Research_count + SD_count + WC_count

#raw count, average degree, percentage of org types; average centrality of org types

#average centrality of org types


#Need to figure out average centrality of types
org_type <- c("CBO","Coalition","Consult",
              "GSA","Loc_Gov", "Nat_Am",
              "NGO","NL_Gov", "Private", "Project", "RCD",
              "Research", "Spec_Dist","WC")
org_type_df <- empty_df[, org_type]

org_type_df <- org_type_df %>% mutate_all(
   ~ifelse(is.na(.),0, .)) #works

#Test loop - figure out degrees
empty_df <- data.frame()


result_by_graph <- lapply(extract_list,function(file){
   print(file)
   temp = readRDS(paste0(file_loc,file))
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
   #head(V(igr)$comb_org)
   #head(V(igr)$entity_type)
   #head(V(igr)$name)
   #make objects out of node attributes
   vertex_names <- V(igr)$name
   comb_org <- V(igr)$comb_org
   
   #calculate degree
   V(igr)$degree <- igraph::degree(igr,mode = 'total')
   # degree <- V(igr)$degree
   # 
   # #make df out of selected items
   # degree_empty <- data.frame()
   # degree_df<- data.frame(org_type = comb_org, degree = degree)
   # df <- degree_df %>%
   #    dplyr::group_by(org_type) %>%
   #    dplyr::summarise(mean_deg = mean(degree, na.rm=T)) %>% data.frame()
   # 
   # 
   #transposed_df <- t(df$mean_deg)
   transposed_df <- t(tapply( V(igr)$degree, V(igr)$comb_org,mean))

   colnames(trannetcolnames(transposed_df) <- df$org_type
   transposed_df <- data.frame(transposed_df) #puts it in the right format
   colnames(transposed_df) <- paste0(colnames(transposed_df),'.meandegree')
   ##NEED TO FINISH THIS

   #summ_df <- plyr::rbind.fill(degree_empty, summ_df)
   ##Still not copying multiple rows
   
    #Below works
   node_attr <- data.frame(Name = vertex_names, comb_org = comb_org)
   edgelist <- as.data.frame(as_edgelist(igr))
   org_counts <- table(node_attr$comb_org)
   org_counts_df <- data.frame(rbind(org_counts))
   colnames(org_counts_df) <- paste0(colnames(org_type_df),'.count')
   motif_counts_df <- data.frame(rbind(summary(net ~ edges + triangles + twopath)))
   graph_stats_df <- data.frame(network_density = network.density(net),
                                degree_centralization = sna::centralization(net,sna::degree))
   stats_df = cbind(org_counts_df,motif_counts_df,graph_stats_df, transposed_df) ##why isn't summ_df getting into empty df
   stats_df$FILE = file
   stats_df ##why is it copying rows?
}
)
#### this function in data.table takes a list object where each item in the list is a data.frame (or data.table)
   #### and it binds them all together
graph_dt = data.table::rbindlist(result_by_graph,use.names = T,fill = T)






library(tidyr)

empty_df
#Test summarize
degree_df<- data.frame(org_type = comb_org, degree = degree)
summ_df <- data.frame(degree_df %>% 
   dplyr::group_by(org_type) %>%
   dplyr::summarise(mean_deg = mean(degree, na.rm = T)) %>%
   pivot_wider(names_from = org_type, values_from = mean_deg))

str(summ_df)
print(summ_df)

wide_summ_df <- summ_df %>%
   pivot_wider(names_from = org_type, values_from = mean_deg)


##Call summary stats on node_data - degree data for each nodes
summary_metrics <- node_data %>%
   dplyr::group_by(organization_type) %>%
   dplyr::summarise(
      avg_degree = mean(degree),
      count = n()
   )

#Histogram based on degrees
summary_metrics <- node_data %>%
   dplyr::group_by(organization_type) %>%
   summarise(avg_degree = mean(degree))

str(node_data)
node_data$organization_type <- as.factor(node_data$organization_type)

Deg_count <- ggplot(node_data, aes(x = degree)) +
   geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
   labs(x = "Count", y = "Frequency", title = "Histogram of Degree") +
   theme_minimal()

#Pulled from bottom code
#node_data <- data.frame(name = character(),
#                       organization_type = character(),
#                      degree = numeric(),
#                     node_file = character(),
#                    stringsAsFactors = FALSE)
#node_data<- data.frame()
#for (file in extract_list[1:2]) {
#  print(file)
#file = extract_list[1]
# temp = readRDS(paste0(file_loc,file))
#   node_names <- V(igr)$name
#  org <- V(igr)$comb_org
# degrees <- degree(igr)
# Create a temporary dataframe for the current file
#temp_data <- data.frame(
#  name = node_names,
# organization_type = org,
#   degree = degrees,
#  node_file = file,
# stringsAsFactors = FALSE)
#Append the temporary dataframe to the main dataframe
# node_data <- plyr::rbind.fill(node_data, temp_data)
#   }
