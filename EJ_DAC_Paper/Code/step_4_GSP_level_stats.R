library(dotenv)
library(ggraph)
library(igraph)
library(tidyverse)
library(migraph)
library(data.table)

load_dot_env()

network_fp <- paste0(Sys.getenv("BOX_PATH"), "/EJ_Paper/cleaned_extracts_DACified")
extract_list = list.files(network_fp)
gsp_ids <- gsub("^0+", "", gsub("\\.RDS", "", extract_list))

dac_e_cent <- data.frame()

# compare eigenvector centrality
for (g in seq_along(gsp_ids)) {
   net <- readRDS(paste0(network_fp, "/", extract_list[g]))
   gsp_id <- paste0("gsp_",gsp_ids[g])
   dac_eig <- mean(V(net)$eigenvector[V(net)$DAC == 0], na.rm = TRUE)
   non_dac_eig <- mean(V(net)$eigenvector[V(net)$DAC == 1], na.rm = TRUE)
   new_row <- data.frame(gsp_id = gsp_id,
                         dac_eig = dac_eig,
                         non_dac_eig = non_dac_eig,
                         hyp = dac_eig > non_dac_eig)
   dac_e_cent <- rbind(dac_e_cent, new_row)
}

summary(dac_e_cent)

dac_deg_df <- data.frame()

# compare degree 
for (g in seq_along(gsp_ids)) {
   net <- readRDS(paste0(network_fp, "/", extract_list[g]))
   gsp_id <- paste0("gsp_",gsp_ids[g])
   dac_deg <- mean(V(net)$degree[V(net)$DAC == 0], na.rm = TRUE)
   non_dac_deg <- mean(V(net)$degree[V(net)$DAC == 1], na.rm = TRUE)
   new_row <- data.frame(gsp_id = gsp_id,
                         dac_deg = dac_deg,
                         non_dac_deg = non_dac_deg,
                         hyp = dac_deg > non_dac_deg)
   dac_deg_df <- rbind(dac_deg_df, new_row)
}

summary(dac_deg_df)

ggplot(dac_deg_df) +
   geom_freqpoly(aes(non_dac_deg), binwidth=0.001, color = 'blue')+
   geom_freqpoly(aes(dac_deg), binwidth=0.001, color = 'red')

ggplot(dac_e_cent) +
   geom_freqpoly(aes(non_dac_eig), binwidth=0.001, color = 'blue')+
   geom_freqpoly(aes(dac_eig), binwidth=0.001, color = 'red')

places_data <- data.frame()

# compare eigenvector centrality
for (g in seq_along(gsp_ids)) {
   net <- readRDS(paste0(network_fp, "/", extract_list[g]))
   gsp_id <- paste0("gsp_",gsp_ids[g])
   places <- as_data_frame(induced_subgraph(net, which(V(net)$exists == 1)), what = "vertices")
   places_data <- rbind(places_data, places)
}

deg_mod <- lm(degree ~ MHI+POP+incorporated+per_latino, data = places_data); summary(deg_mod)
clo_mod <- lm(closeness ~ MHI+POP+incorporated+per_latino, data = places_data); summary(clo_mod)
eig_mod <- lm(eigenvector ~ MHI+POP+incorporated+per_latino, data = places_data); summary(eig_mod)
bet_mod <- lm(betweeness ~ MHI+POP+incorporated+per_latino, data = places_data); summary(bet_mod)

gsp_list_test <- net_process(file = paste0(network_fp, "/",extract_list[3]),
                             gsp_id = gsp_ids[3])

gsp_graph_test <- net_graph(gsp_list_test)

isolates_test <- which(degree(gsp_graph_test) == 0)
graph_2_test <- delete.vertices(gsp_graph_test, isolates_test)

ggraph(gsp_graph_test, layout = 'kk') +
   geom_edge_link(color = "black", alpha = .5) + 
   geom_node_point(aes(size = num_appearances, colour = DAC)) + 
   theme_graph()

## TODO
## get census data
## correlation table between community characteristics and place in network
## clean up stats
## simplify table of contents to 4/5 categories
## tage each edge to the section of the ToC it belongs to