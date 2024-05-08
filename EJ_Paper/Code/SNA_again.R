library(devtools)
library(ggraph)
library(sna)
library(stringr)
library(dplyr)
library(data.table)
library(pbapply)
library(stringi)
library(stringr)
library(tidyverse)
library(plyr)
library(statnet)

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

#MERGE LISTS# Add Column
org_nodes <- read.csv("EJ_Paper/Data/Test/merged_dt_4_24.csv")
nodelist_merge <- inner_join(nodes_007, org_nodes, by = c("entity_name", "entity_type"))

##Need to figure out how to add column - IT WORKS!
nodelist_merge$red_org <- sapply(nodelist_merge$comb_org, function(comb_org) {
   if (comb_org %in% c("Ambig", "Coalition", "Consult", 
                       "GSA", "NL_Gov", "Nat_Am" "Project", 
                       "Research", "Spec_Dist", "WC")) {
      return("Orgs")
   } else if (comb_org %in% c("Drop", "Follow")) {
      return("Drop")
   } else {
         return(comb_org)
      }
   })

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

network::set.edge.attribute()
network::list.edge.attributes()
network::get.edge.attribute()

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

#Network and statistics, 4 categories


#Centrality stats with reduced other org category
net %v% 'comb_org' <- nodelist_merge$comb_org[match(network.vertex.names(net),nodelist_merge$entity_name)]
net %v% 'red_org' <- nodelist_merge$red_org[match(network.vertex.names(net),nodelist_merge$entity_name)]
df_red <- data.frame(node=network.vertex.names(net), 
                 org.type.red = net %v% 'red_org',
                 node.betweenness.value = betweenness(net),
                 node.in.cent = degree(net, gmode = "digraph", cmode ="indegree"),
                 node.out.cent = degree(net, gmode = "digraph", cmode = "outdegree"),
                 node.degree = degree(net, gmode = "digraph"))

mean_between_cent_no_filter_red <- df_red %>%
   group_by(org.type.red) %>%
   dplyr::summarize(mean_between = mean(node.betweenness.value, na.rm = T),
                    mean_incent = mean(node.in.cent),
                    mean_outcent = mean(node.out.cent),
                    mean_degree = mean(node.degree))
write.csv(mean_between_cent_no_filter_red, "EJ_Paper/Data/cent_stats_red_org.csv")

##Test anova on centrality statistics - no filters but less categories
install.packages("car")
library(car)

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


##Network and statistics, "Drop" is filtered out
categories_to_filter <- c('Drop', 'Follow')
net %v% 'comb_org' <- nodelist_merge$comb_org[match(network.vertex.names(net),nodelist_merge$entity_name)]



view(means)
head(df)

#Split apply combine - read in graphs, apply function, combine iteratively
#keep adding and storing - lapply - store, combine later
#Org type, each of the stats I want, save some type of node ID, org type, network stats for node, and stack those things up
#end up with a giant df - each row is a node, each column is a stat, and then can run comparisons between all of the stuff
#try with 4 graphs, go from there
