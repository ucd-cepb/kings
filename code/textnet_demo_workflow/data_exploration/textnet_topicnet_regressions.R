library(network)
library(sna)
library(igraph)
library(intergraph)
library(vegan)

#choose your gov_net base
#gov_net_full <- readRDS("data_output/dir_full_network_properties")
#gov_net_orgs <- readRDS("data_output/dir_full_network_properties_orgs")
gov_net <- as.data.frame(lapply(gov_net, as.numeric))

topic_net <- readRDS("data_output/topic_network_properties")
topic_net <- as.data.frame(lapply(topic_net, as.numeric))
gov_net <- gov_net[gov_net$gsp_id %in% topic_net$gsp_id,]

topic_prevalences <- readRDS("data_output/topic_prevalence")

shannon_div <- diversity(topic_prevalences, index="shannon")

topics_of_interest <- as.numeric(gsub("\\D", "", readRDS("data_temp/topics_of_interest")))
#dw_only <- c(21, 24, 45, 49)
#topics_of_interest <- dw_only
H1_model <- lm(as.matrix(topic_prevalences[,topics_of_interest]) ~ 
                 gov_net$connectedness + 
                 gov_net$transitivity + 
                 gov_net$centralization +
                 gov_net$num_communities + 
                 gov_net$modularity + 
                 gov_net$percent_homophily
                 )
  
H2_model <-lm(shannon_div ~ 
                gov_net$connectedness + 
                gov_net$transitivity + 
                gov_net$centralization + 
                gov_net$num_communities + 
                gov_net$modularity + 
                gov_net$percent_homophily)
  
H3_model <- lm(as.matrix(topic_net[,c("connectedness","transitivity",
                                      "centralization","num_communities")]) ~ 
                 gov_net$connectedness + 
                 gov_net$transitivity + 
                 gov_net$centralization + 
                 gov_net$num_communities + 
                 gov_net$modularity + 
                 gov_net$percent_homophily
                 )

