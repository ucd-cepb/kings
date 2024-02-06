library(dotenv)
library(ggraph)
library(network)
library(tidyverse)
library(intergraph)
library(migraph)

load_dot_env()
network_fp <- paste0(Sys.getenv("BOX_PATH"),"/EJ_Paper/cleaned_extracts_textgov_paper_version/0012.RDS")
gsp<- readRDS(network_fp)
locs <- read.csv('EJ_Paper/Data/locations.csv')

nl <- gsp[[1]]
nl$place <- ifelse(nl$entity_name %in% locs$place_name, 1, 0)
nl[nl$place == 1,]
el <- gsp[[2]]
el <- el %>% 
    filter(!is.na(source) & !is.na(target))  %>% 
    select(-doc_sent_verb)

gsp_graph <- network(el, vertices = nl, loops = TRUE, multiple = TRUE)
gsp_degree <- data.frame(node_degree(gsp_graph))
