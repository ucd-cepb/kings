library(dotenv)
library(ggraph)
library(network)
library(tidyverse)
library(intergraph)
library(migraph)
library(data.table)

load_dot_env()

network_fp <- paste0(Sys.getenv("BOX_PATH"),
                     "/EJ_Paper/cleaned_extracts_textgov_paper_version")
extract_list = list.files(network_fp)
locs <- read.csv('EJ_DAC_Paper/Data/locations.csv')

### convenience function for reading in and processing nodes
### extracts have ORG, PERSON, and GPE nodetypes
process_extract <- function(file, drop = 'PERSON'){
   # read in rds file
   temp = readRDS(file)
   # grap nodelist
   nl <- temp$nodelist
   # label places and DACs
   nl$place <- ifelse(nl$entity_name %in% locs$place_name, 1, 0)
   nl <- nl %>% left_join(locs, by=join_by(entity_name == place_name))
   el <- temp$edgelist
   # filter out NA edges and irrelevant cols
   el <- el %>% 
      filter(!is.na(source) & !is.na(target))  %>% 
      select(-doc_sent_verb)
   networklist <- list("nodelist" = nl, "edgelist" = el)
   return(networklist)
}
net <- list()
for (i in seq_along(extract_list[2:3])){
   path = paste0(network_fp, "/",extract_list[i+1])
   net[[i]] <- process_extract(path)
   gsp_graph <- network(net[[i]]$edgelist,
                        vertices = net[[i]]$nodelist,
                        loops = TRUE,
                        multiple = TRUE)
}


gsp_degree <- data.frame(node_degree(gsp_graph))
