library(dotenv)
library(ggraph)
library(igraph)
library(tidyverse)
library(migraph)
library(data.table)
library(ggcorrplot)

load_dot_env()

network_fp <- paste0(Sys.getenv("BOX_PATH"), "/EJ_Paper/cleaned_extracts_DACified")
extract_list = list.files(network_fp)
gsp_ids <- gsub("^0+", "", gsub("\\.RDS", "", extract_list))

all_nodes <- data.frame()

for (g in seq_along(gsp_ids)) {
   net <- readRDS(paste0(network_fp, "/", extract_list[g]))
   gsp_id <- paste0("gsp_",gsp_ids[g])
   nodes <- igraph::as_data_frame(net, what = "vertices")
   nodes$gsp_id <- gsp_id
   all_nodes <- rbind(all_nodes, nodes)
}


all_nodes_compare <- all_nodes %>% 
   mutate(degree = as.numeric(degree),
          eigenvector = as.numeric(eigenvector),
          leader_dist = as.numeric(leader_dist)) %>% 
   select(gsp_id, degree, eigenvector, leader_dist, exists)

all_nodes_compare %>% filter(exists==1) %>% summary()


summary(all_nodes_compare)
