library(dotenv)
library(ggraph)
library(igraph)
library(tidyverse)
library(migraph)
library(data.table)
library(ggcorrplot)
library(knitr)

load_dot_env()

network_fp <- paste0(Sys.getenv("BOX_PATH"), "/EJ_Paper/cleaned_extracts_DACified")
extract_list = list.files(network_fp)
gsp_ids <- gsub("^0+", "", gsub("\\.RDS", "", extract_list))

gsp_summary <- data.frame()

for (g in seq_along(gsp_ids)) {
   net <- readRDS(paste0(network_fp, "/", extract_list[g]))
   gsp_id <- paste0("gsp_",gsp_ids[g])
   new_row <- data.frame(gsp_id = gsp_id,
                         num_node = vcount(net),
                         num_edge = ecount(net),
                         eig = mean(V(net)$eigenvector, na.rm = TRUE),
                         dist = mean(V(net)$leader_dist, na.rm = TRUE),
                         dac_eig = mean(V(net)$eigenvector[V(net)$DAC == 0], na.rm = TRUE),
                         non_dac_eig = mean(V(net)$eigenvector[V(net)$DAC == 1], na.rm = TRUE),
                         dac_dist = mean(V(net)$leader_dist[V(net)$DAC == 0], na.rm = TRUE),
                         non_dac_dist = mean(V(net)$leader_dist[V(net)$DAC == 1], na.rm = TRUE)
   )
   gsp_summary <- rbind(gsp_summary, new_row)
}

sum_tab <- gsp_summary %>% 
   select(-gsp_id) %>% 
   summary()

