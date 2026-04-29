# step_3_generate_networks.R
# Generates networks from supernetwork cleaned_unfiltered_extracts.
# Applies place/DAC tagging to each network's nodelist.

library(dotenv)
library(ggraph)
library(igraph)
library(tidyverse)
library(migraph)
library(data.table)
library(stringr)

load_dot_env()

out_dir <- paste0(Sys.getenv("BOX_PATH"), "/EJ_Paper/processed_networks/")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# --- Data sources ---

network_fp <- paste0(Sys.getenv("BOX_PATH"),
                     "/Supernetwork_Paper/cleaned_unfiltered_extracts")
extract_list <- list.files(network_fp)
extract_list <- setdiff(extract_list, c("0053.RDS", "0089.RDS"))
gsp_ids <- as.numeric(gsub("^0+", "", gsub("\\.RDS", "", extract_list)))

all_places <- read.csv("EJ_DAC_Paper/Data/all_places.csv")
wl <- readLines("EJ_DAC_Paper/Data/wl.txt")

# page-level section data
pages_fp <- paste0(Sys.getenv("BOX_PATH"),
                   "/Structural_Topic_Model_Paper/gsp_docs_lean")
page_features <- tibble(readRDS(pages_fp)) %>%
   mutate(gsp_id = as.numeric(gsp_id)) %>%
   filter(is_comment == FALSE & is_reference == FALSE) %>%
   mutate(original_page_num = as.numeric(page_num),
          page_num = ave(1:nrow(.), gsp_id, FUN = seq_along)) %>%
   dplyr::select(c("gsp_id", "page_num"))

# verb-based edgelist
edge_fp <- paste0(Sys.getenv("BOX_PATH"), "/Verb_Analysis_Paper/edgelist_with_verb_meta")
ve <- readRDS(edge_fp) %>%
   as_tibble() %>%
   filter(!is.na(source) & !is.na(target)) %>%
   filter(head_verb_lemma %in% wl) %>%
   dplyr::select(-c(2:10)) %>%
   mutate(GSP_ID = as.numeric(gsp_id),
          page_num = as.numeric(stringr::str_remove(stringr::str_remove(doc_sent_verb,
                                                                        ".*pdf"),
                                                    "_.*"))) %>%
   dplyr::select(-c(gsp_id, doc_sent_verb)) %>%
   dplyr::select(source, target, GSP_ID, doc_sent_parent, everything()) %>%
   dplyr::select(-c(24:124))

ve_w_sections <- ve %>%
   left_join(page_features, by = c("GSP_ID" = "gsp_id",
                                   "page_num" = "page_num"))


# ============================================================================
# net_process: place/DAC tagging
# ============================================================================

net_process <- function(file, gsp_id) {
   temp <- readRDS(file)
   nl <- tibble(temp$nodelist)
   places_gsp <- all_places %>% filter(GSP_ID == gsp_id)

   nl <- nl %>%
      left_join(places_gsp, by = join_by(entity_name == NAME20)) %>%
      dplyr::select(-c(entity_type, num_appearances))

   el <- ve_w_sections %>%
      filter(GSP_ID == gsp_id) %>%
      mutate(weight = 1)

   list("nodelist" = tibble(nl), "edgelist" = tibble(el))
}


# ============================================================================
# net_graph
# ============================================================================

net_graph <- function(networklist, gsp_id) {
   network_graph <- igraph::graph_from_data_frame(networklist$edgelist,
                                                  vertices = networklist$nodelist)

   network_graph <- igraph::simplify(network_graph,
                                     remove.multiple = TRUE,
                                     remove.loops = FALSE,
                                     edge.attr.comb = list(weight = "sum",
                                                           neg = "mean",
                                                           has_hedge = "mean",
                                                           is_future = "mean",
                                                           gsp_id = "mean",
                                                           page_num = "concat",
                                                           original_page_num = "concat",
                                                           admin = "sum",
                                                           basin_plan = "sum",
                                                           sust_criteria = "sum",
                                                           monitoring_networks = "sum",
                                                           projects_mgmt_actions = "sum",
                                                           "ignore"))

   return(network_graph)
}


# ============================================================================
# Main loop
# ============================================================================

for (g in seq_along(gsp_ids)) {
   gsp_id <- gsp_ids[g]

   time_process <- system.time({
      gsp_list <- net_process(
         file = paste0(network_fp, "/", extract_list[g]),
         gsp_id = gsp_id)
   })

   print(paste0("Processed ", gsp_id, ": ",
                round(time_process["elapsed"], 2), " seconds"))

   time_graph <- system.time({
      gsp_graph <- net_graph(gsp_list, gsp_id = gsp_id)
   })

   print(paste0("Graphed ", gsp_id, ": ",
                round(time_graph["elapsed"], 2), " seconds"))

   saveRDS(object = gsp_graph,
           file = paste0(out_dir, extract_list[g]))
}
