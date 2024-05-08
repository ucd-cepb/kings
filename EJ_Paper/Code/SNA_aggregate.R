library(data.table)
library(stringr)
library(tidyverse)
library(dplyr)
library(plyr)
library(purrr)

library(devtools)
library(ggraph)
library(sna)
library(pbapply)
library(stringi)
library(statnet)


#Where are the files
file_loc <- 'C:\\Users\\hgsha\\Box Sync\\Kings_Large_Files\\data\\EJ_Paper\\cleaned_extracts_textgov_paper_version\\'
extract_list = list.files(file_loc)

##Grab nodelist

grab_orgs <- function(file,drop = 'PERSON'){
   # read in rds file
   temp = readRDS(file)
   # isolate nodelist
   nodes = temp$nodelist
   # filter out node types we don't want
   nodes = nodes[!nodes$entity_type%in% drop,]
   return(nodes)
}

empty_node_dt <- data.table()

n= for(file in extract_list[1:5]){
   print(file)
   temp = grab_orgs(paste0(file_loc,file))
   empty_node_dt <- rbind(empty_node_dt,temp,fill = T,use.names = T)
   empty_node_dt = empty_node_dt[,list(num_appearances = sum(num_appearances)),
                                 by=.(entity_name,entity_type)]
}

###Grab edges
grab_edge <- function(file, drop = FALSE){
   # read in rds file
   temp = readRDS(file)
   # isolate nodelist
   edges = temp$edgelist
   edges = edges[!edgeiscomplete %in% drop,] 
   return(edges)
}

empty_edge_dt <- data.table()


e=for(file in extract_list[1:5]){
   print(file)
   temp = grab_edge(paste0(file_loc,file))
   empty_edge_dt <- rbind(empty_edge_dt, temp, fill = T, use.names = T)
}


#Check
edge_complete <- empty_edge_dt %>% filter(edgeiscomplete=="TRUE") %>%
   select(source, target)
head(edge_complete)

head(empty_node_dt)

#Merge nodes with dictionary
org_nodes <- read.csv("EJ_Paper/Data/Test/merged_dt_4_24.csv")
nodelist_merge <- inner_join(empty_node_dt, org_nodes, by = c("entity_name", "entity_type"))

head(nodelist_merge)

#Add column to the nodelist for reduced categorization of nodes
nodelist_merge$red_org <- sapply(nodelist_merge$comb_org, function(comb_org) {
   if (comb_org %in% c("Ambig", "Coalition", "Consult", 
                       "GSA", "Loc_Gov",
                       "NL_Gov", "Nat_Am", "Project", "Private",
                       "Research", "RCD",
                       "Spec_Dist", "WC")) {
      return("Orgs")
   } else if (comb_org %in% c("Drop", "Follow")) {
      return("Drop")
   } else {
      return(comb_org)
   }
})

head(nodelist_merge)

#Missing items
df_miss <- nodelist_merge[is.na(nodelist_merge$comb_org),]

###NEEED TO FIGURE OUT HOW TO INTEGRATE GSUB ITEMS AND HAVE IT RUN AGAINST THOSE ONES
nodelist_merge$new_name <- gsub("_wd$", "_water_district", nodelist_merge$entity_name) #works
nodelist_merge$new_name <- gsub("_s$", "", nodelist_merge$new_name) #works
nodelist_merge$new_name <- gsub("^a_", "", nodelist_merge$new_name) #works
nodelist_merge$new_name <- gsub("(.+?_)\\1+", "\\1", nodelist_merge$new_name) #works
nodelist_merge$new_name <- gsub('([a-z]+_[a-z]+_)\\1+','\\1',nodelist_merge$new_name,perl = T)

#Network stats for not aggregated items
net <- network(edge_complete, matrix.type="edgelist", loops = T, 
               directed = T, duplicates = TRUE, multiple = T)

net %v% 'comb_org' <- nodelist_merge$comb_org[match(network.vertex.names(net),nodelist_merge$entity_name)]
net %v% 'red_org' <- nodelist_merge$red_org[match(network.vertex.names(net),nodelist_merge$entity_name)]
df_red <- data.frame(node=network.vertex.names(net), 
                     org.type.red = net %v% 'red_org',
                     node.betweenness.value = betweenness(net),
                     node.in.cent = degree(net, gmode = "digraph", cmode ="indegree"),
                     node.out.cent = degree(net, gmode = "digraph", cmode = "outdegree"),
                     node.degree = degree(net, gmode = "digraph"))

mod1 <- aov(node.in.cent ~ org.type.red, data = df_red)
summary(mod1)
TukeyHSD(mod1)

mod2 <- aov(node.out.cent ~ org.type.red, data = df_red) ##significant
summary(mod2)
TukeyHSD(mod2)

mod3 <- aov(node.degree ~ org.type.red, data = df_red)
summary(mod3)
TukeyHSD(mod3)

mod4 <- aov(node.betweenness.value ~ org.type.red, data = df_red)
summary(mod4)
TukeyHSD(mod4)

mean_between_cent_no_filter_red <- df_red %>%
   group_by(org.type.red) %>%
   dplyr::summarize(mean_between = mean(node.betweenness.value, na.rm = T),
                    mean_incent = mean(node.in.cent),
                    mean_outcent = mean(node.out.cent),
                    mean_degree = mean(node.degree))

#Network stats for non-aggregated items
#Network and statistics, no filter

net %v% 'comb_org' <- nodelist_merge$comb_org[match(network.vertex.names(net),nodelist_merge$entity_name)]
df <- data.frame(node=network.vertex.names(net), 
                 org.type = net %v% 'comb_org',
                 node.betweenness.value = betweenness(net),
                 node.in.cent = degree(net, gmode = "digraph", cmode ="indegree"),
                 node.out.cent = degree(net, gmode = "digraph", cmode = "outdegree"),
                 node.degree = degree(net, gmode = "digraph"))

mean_between_cent_no_filter <- df %>%
   group_by(org.type) %>%
   dplyr::summarize(mean_between = mean(node.betweenness.value, na.rm = T),
                    mean_incent = mean(node.in.cent),
                    mean_outcent = mean(node.out.cent),
                    mean_degree = mean(node.degree))
write.csv(mean_between_cent_no_filter, "EJ_Paper/Data/cent_stats.csv")


##Test anova on centrality statistics - no filters, all categories
mod1 <- aov(node.in.cent ~ org.type, data = df)
summary(mod1)
TukeyHSD(mod1)

mod2 <- aov(node.out.cent ~ org.type, data = df) ##significant
summary(mod2)
TukeyHSD(mod2)

mod3 <- aov(node.degree ~ org.type, data = df)
summary(mod3)
TukeyHSD(mod3)

mod4 <- aov(node.betweenness.value ~ org.type, data = df)
summary(mod4)
TukeyHSD(mod4)
