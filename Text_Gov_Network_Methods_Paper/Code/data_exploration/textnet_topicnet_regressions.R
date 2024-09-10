library(network)
library(sna)
library(igraph)
library(intergraph)
library(vegan)

gov_net <- readRDS("data/output_large_files/gov_dir_weight_no_gpe_network_properties")
gov_net <- as.data.frame(lapply(gov_net, as.numeric))

topic_net <- readRDS("data/output_large_files/topic_network_properties")
topic_net <- as.data.frame(lapply(topic_net, as.numeric))
gov_net <- gov_net[gov_net$gsp_id %in% topic_net$gsp_id,]

topic_prevalences <- readRDS("data/Structural_Topic_Model_Paper/topic_prevalence")

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

summary(H1_model)
summary(H2_model)
summary(H3_model)
