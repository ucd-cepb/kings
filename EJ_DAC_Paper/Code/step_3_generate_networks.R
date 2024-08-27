library(dotenv)
library(ggraph)
library(igraph)
library(tidyverse)
library(migraph)
library(data.table)

load_dot_env()

network_fp <- paste0(Sys.getenv("BOX_PATH"),
                     "/Supernetwork_Paper/cleaned_unfiltered_extracts")
extract_list <- list.files(network_fp)
extract_list <- setdiff(extract_list, c("0053.RDS", '0089.RDS'))

all_places <- read.csv('EJ_DAC_Paper/Data/all_places.csv')

gsp_ids <- as.numeric(gsub("^0+", "", gsub("\\.RDS", "", extract_list)))

edge_fp <- paste0(Sys.getenv("BOX_PATH"), "/Verb_Analysis_Paper/edgelist_with_verb_meta")

wl <- readLines('EJ_DAC_Paper/Data/wl.txt')

# path to page-level data
pages_fp <- paste0(Sys.getenv("BOX_PATH"), 
                   "/Structural_Topic_Model_Paper/gsp_docs_lean")

page_features <- tibble(readRDS(pages_fp)) %>% 
   mutate(gsp_id = as.numeric(gsp_id)) %>%
   filter(is_comment == FALSE & is_reference == FALSE) %>% 
   mutate(original_page_num = as.numeric(page_num),
          page_num = ave(1:nrow(.), gsp_id, FUN = seq_along)) %>% #renumber pages after removing comments/reference
   select(c("gsp_id", "page_num", "admin", "basin_plan", 
            "sust_criteria", "monitoring_networks", 
            "projects_mgmt_actions", "is_comment", "is_reference", "original_page_num"))

ve <- readRDS(edge_fp) %>% 
   as_tibble() %>% 
   filter(!is.na(source) & !is.na(target)) %>% # remove edges without both nodes attached
   filter(head_verb_lemma %in% wl) %>% # compare with scowl wl
   select(-c(2:10)) %>% 
   mutate(GSP_ID = as.numeric(gsp_id),
          page_num = as.numeric(stringr::str_remove(stringr::str_remove(doc_sent_verb, 
                                                                        ".*pdf"), 
                                                    "_.*"))) %>% 
   select(-c(gsp_id, doc_sent_verb)) %>%
   select(source, target, GSP_ID, doc_sent_parent, everything()) %>% 
   select(-c(24:124)) 
   
ve_w_sections <- ve %>% 
   left_join(page_features, by = c("GSP_ID" = "gsp_id",
                                   "page_num" = "page_num"))

# process node/edgelist for use
net_process <- function(file, gsp_id){
   # read in rds file
   temp <- readRDS(file)
   # grab nodelist
   nl <- tibble(temp$nodelist)
   # tag places and DACs
   all_places <- all_places %>% 
      filter(GSP_ID == gsp_id )
   
   nl <- nl %>% 
      left_join(all_places, 
                by=join_by(entity_name == NAME20)) %>% 
      select(-c(entity_type, num_appearances))

   el <- ve_w_sections %>% 
      filter(GSP_ID == gsp_id) %>% 
      mutate(weight = 1)
   
   networklist <- list("nodelist" = tibble(nl), "edgelist" = tibble(el))

   return(networklist)
}

# process node/edgelist to igraph object
net_graph <- function(networklist, gsp_id) {
   
   network_graph <- igraph::graph_from_data_frame(networklist$edgelist,
                                                  vertices = networklist$nodelist)
   
   # network_graph_simp <- network_graph
   network_graph <- igraph::simplify(network_graph,
                                          remove.multiple = TRUE,
                                          remove.loops = FALSE,
                                          edge.attr.comb = list(weight = 'sum',
                                                                neg = 'mean',
                                                                has_hedge = 'mean',
                                                                is_future = 'mean',
                                                                gsp_id = 'mean',
                                                                page_num = 'concat',
                                                                original_page_num = 'concat',
                                                                admin = 'sum',
                                                                basin_plan = 'sum',
                                                                sust_criteria = 'sum',
                                                                monitoring_networks = 'sum',
                                                                projects_mgmt_actions = 'sum',
                                                                'ignore'))
   
   return(network_graph)
}

# Apply functions to all networks
for (g in seq_along(gsp_ids)) {
   gsp_id <- gsp_ids[g]
   
   # Timing for net_process
   time_process <- system.time({
      gsp_list <- net_process(file = paste0(network_fp, 
                                            "/", 
                                            extract_list[g]), 
                              gsp_id = gsp_ids[g])
   })
   
   print(paste0("Processed ", 
                gsp_id, 
                ": ", 
                round(time_process["elapsed"], 2), 
                " seconds"))
   
   # Timing for net_graph
   time_graph <- system.time({
      gsp_graph <- net_graph(gsp_list, gsp_id = gsp_ids[g])
   })
   
   print(paste0("Graphed ", 
                gsp_id, 
                ": ", 
                round(time_graph["elapsed"], 2), 
                " seconds"))
   
   # Save graph
   saveRDS(object = gsp_graph, 
           file = paste0(Sys.getenv("BOX_PATH"), 
                         "/EJ_Paper/processed_networks/", 
                         extract_list[g]))
}

# test functions for one network

glt <- net_process(file = paste0(network_fp, "/",extract_list[102]),
                             gsp_id = gsp_ids[102])

ggt <- net_graph(glt,
                 gsp_id = gsp_ids[102])
