# step_3_generate_networks.R
# Generates networks from supernetwork cleaned_unfiltered_extracts.
# Applies place tagging to each network's nodelist.
# Place aliases: nodes that name a Census place itself are renamed to the place before graphing
#    R1  (the_)?(city|town|community|village)_of_(the_)?X
#    R2  X_(city|town|community|ca|california|cdp)
#    R3  X after dropping _ - . ' and folding accents, saint/st, mount/mt, fort/ft
#  a node that is itself another Census place name is never an alias,
#  and a node claimed by two places in the same plan stays separate
# Self-loops are removed (including loops created by merging X with city_of_X).
# Run from the project root (kings.Rproj).

library(dotenv)
library(igraph)
library(tidyverse)
library(stringr)

load_dot_env()

data_dir <- "EJ_DAC_Paper/Data/"
out_dir <- paste0(Sys.getenv("BOX_PATH"), "/EJ_Paper/processed_networks_final/")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# --- Data sources ---

network_fp <- paste0(Sys.getenv("BOX_PATH"),
                     "/Supernetwork_Paper/cleaned_unfiltered_extracts")
extract_list <- list.files(network_fp)
extract_list <- setdiff(extract_list, c("0053.RDS", "0089.RDS"))
gsp_ids <- as.numeric(gsub("^0+", "", gsub("\\.RDS", "", extract_list)))

all_places <- read.csv(paste0(data_dir, "all_places.csv"))
census_place_names <- read.csv(paste0(data_dir, "places.csv"))$place_name
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
# place_aliases: which nodes name an expected place itself
# ============================================================================

# R3 normalisation: fold accents and ligatures, unify saint/mount/fort, drop separators
normalise_name <- function(name) {
   name <- iconv(name, "UTF-8", "ASCII//TRANSLIT", sub = "")
   name <- gsub("\\bsaint\\b|^st_|_st_", "st", name)
   name <- gsub("^mount_|_mount_|^mt_|_mt_", "mt", name)
   name <- gsub("^fort_|^ft_", "ft", name)
   gsub("[_\\-\\.']", "", name)
}

alias_rule <- function(node, place) {
   place_re <- gsub("([.^$|()\\[\\]{}*+?\\\\-])", "\\\\\\1", place, perl = TRUE)
   case_when(
      grepl(paste0("^(the_)?(city|town|community|village)_of_(the_)?", place_re, "$"), node) ~ "R1",
      grepl(paste0("^", place_re, "_(city|town|community|ca|california|cdp)$"), node) ~ "R2",
      normalise_name(node) == normalise_name(place) ~ "R3",
      TRUE ~ NA_character_)
}

place_aliases <- function(nodes, places) {
   candidates <- setdiff(nodes, census_place_names)  # other Census places are never aliases

   # 4 plans contain no Census place
   if (!length(places)) return(tibble(place = character(), alias = character(), rule = character()))

   map_dfr(places, function(x) {
      tibble(place = x,
             alias = candidates,
             rule = alias_rule(candidates, x))
   }) %>%
      filter(!is.na(rule), alias != place) %>%
      # a node claimed by two places stays separate
      group_by(alias) %>%
      filter(n() == 1) %>%
      ungroup()
}


# ============================================================================
# net_process: place tagging
# ============================================================================

net_process <- function(file, gsp_id) {
   temp <- readRDS(file)
   nl <- tibble(temp$nodelist)
   places_gsp <- all_places %>% filter(GSP_ID == gsp_id)

   el <- ve_w_sections %>%
      filter(GSP_ID == gsp_id) %>%
      mutate(weight = 1)

   aliases <- place_aliases(nl$entity_name, places_gsp$NAME20)

   # rename aliases to their place in both the nodelist and the edgelist
   rename_alias <- function(x) coalesce(aliases$place[match(x, aliases$alias)], x)

   nl <- nl %>%
      mutate(entity_name = rename_alias(entity_name)) %>%
      distinct(entity_name, .keep_all = TRUE) %>%
      left_join(places_gsp, by = join_by(entity_name == NAME20)) %>%
      dplyr::select(-c(entity_type, num_appearances))

   el <- el %>%
      mutate(source = rename_alias(source),
             target = rename_alias(target))

   list("nodelist" = tibble(nl),
        "edgelist" = tibble(el),
        "aliases" = aliases %>% mutate(GSP_ID = gsp_id))
}


# ============================================================================
# net_graph
# ============================================================================

net_graph <- function(networklist, gsp_id) {
   network_graph <- igraph::graph_from_data_frame(networklist$edgelist,
                                                  vertices = networklist$nodelist)

   network_graph <- igraph::simplify(network_graph,
                                     remove.multiple = TRUE,
                                     remove.loops = TRUE,
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

all_aliases <- list()

for (g in seq_along(gsp_ids)) {
   gsp_id <- gsp_ids[g]

   gsp_list <- net_process(file = paste0(network_fp, "/", extract_list[g]),
                           gsp_id = gsp_id)

   gsp_graph <- net_graph(gsp_list, gsp_id = gsp_id)

   saveRDS(object = gsp_graph,
           file = paste0(out_dir, extract_list[g]))

   all_aliases[[extract_list[g]]] <- gsp_list$aliases

   print(paste0("Graphed ", gsp_id))
}

# appendix table: every alias merged into a place node
write_csv(bind_rows(all_aliases), paste0(data_dir, "place_aliases.csv"))
