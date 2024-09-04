library(dotenv)
library(ggraph)
library(igraph)
library(tidyverse)
library(migraph)
library(data.table)
library(statnet)

load_dot_env()

network_fp <- paste0(Sys.getenv("BOX_PATH"),
                     "/Supernetwork_Paper/cleaned_unfiltered_extracts")

wl <- readLines('Network_Structure_Paper/Data/wl.txt')

extract_list <- list.files(network_fp)
extract_list <- setdiff(extract_list, c("0053.RDS", '0089.RDS'))
gsp_ids <- gsub("^0+", "", gsub("\\.RDS", "", extract_list))

gsa_gsp <- tibble(read.csv('EJ_DAC_Paper/Data/gsa_gsp.csv'))
gsa_names <- read.csv('EJ_DAC_Paper/Data/gsa_names.csv')

clean_gsa_names <- function(gsa_names) {
   # replace groundwater_sustainability_agency with gsa
   gsa_names_2 <- gsa_names
   gsa_names_2$GSA_Name <- str_replace(gsa_names$GSA_Name, 
                                       "groundwater_sustainability_agency", 
                                       "gsa")
   
   # add in gsa names with 'groundwater' in them already, removing 'groundwater_sustainability_agency'
   gsa_names_3 <- gsa_names %>% 
      mutate(GSA_Name = str_replace(gsa_names$GSA_Name, 
                                    "_groundwater_sustainability_agency", 
                                    "")) %>% 
      filter(grepl("groundwater", GSA_Name))
   
   gsa_names_4 <- data.frame(GSA_ID=c('147',
                                      '461',
                                      '457',
                                      '253',
                                      '456',
                                      '106',
                                      '415',
                                      '418',
                                      '384',
                                      '432',
                                      '163',
                                      '433',
                                      '434',
                                      '49',
                                      '24',
                                      '403', 
                                      '47',
                                      '422',
                                      '124',
                                      '420', 
                                      '419',
                                      '268'
   ),
   GSA_Name = c('sacramento_central_groundwater_authority',
                'salinas_valley_basin_groundwater_sustainability_agency',
                'siskiyou_county_flood_control_and_water_conservation_district_groundwater_sustainability_agency_butte_valley',
                'siskiyou_county_flood_control_and_water_conservation_district_groundwater_sustainability_agency_scott_river',
                'siskiyou_county_flood_control_and_water_conservation_district_groundwater_sustainability_agency_shasta',
                'yuba_water_agency',
                'yuba_water_agency',
                'tehama_county_flood_control_and_water_conservation_district',
                'reclamation_district_no_501_groundwater_sustainability_agency_northern_delta_groundwater_sustainability_agency',
                'fox_canyon_groundwater_management_agency',
                'fox_canyon_groundwater_management_agency',
                'fox_canyon_groundwater_management_agency',
                'fox_canyon_groundwater_management_agency',
                'arroyo_santa_rosa_groundwater_sustainability_agency',
                'mga',
                'owens_valley_groundwater_authority',
                'madera_co_groundwater_sustainability_agency',
                'tehama_county_flood_control_and_water_conservation_district_groundwater_sustainability_agency_antelope',
                'tehama_county_flood_control_and_water_conservation_district_groundwater_sustainability_agency_bowman', 
                'tehama_county_flood_control_and_water_conservation_district_groundwater_sustainability_agency_los_molinos',
                'tehama_county_flood_control_and_water_conservation_district_groundwater_sustainability_agency_red_bluff',
                'svbgsa'
   )
   )
   
   gsa_names <- rbind(gsa_names, gsa_names_2, gsa_names_3, gsa_names_4)
   return(gsa_names)
}

gsa_names <- clean_gsa_names(gsa_names)

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

edge_fp <- paste0(Sys.getenv("BOX_PATH"), "/Verb_Analysis_Paper/edgelist_with_verb_meta")
valid_tenses <- c("VB", "VBD", "VBN", "VBG", "VBP", "VBZ")

all_edges <- readRDS(edge_fp) %>% as_tibble()

ve <- all_edges %>% 
   filter(!is.na(source) & !is.na(target)) %>% # remove edges without both nodes attached
   filter(head_verb_tense %in% valid_tenses) %>%  # filter for specific verb tenses
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

# add node labels from Hannah doc

label_fp <- paste0(Sys.getenv("BOX_PATH"),
                   "/Multipurpose_Files/Dictionaries/googlesheets_dt_complete.csv")
label_dict <- read.csv(label_fp)

govsci_fp <- paste0(Sys.getenv("BOX_PATH"),
                    "/Multipurpose_Files/Dictionaries/govsci_tbl_noblank.csv")
govsci_dict <- read.csv(govsci_fp)

# process node/edgelist for use
net_process <- function(file, gsp_id){
   # grab nodelist
   nl <- tibble(readRDS(file)$nodelist)
   
   # get gsa names from gsa_gsp and gsa_names
   gsas <- gsa_gsp %>% filter(GSP_ID == gsp_id)
   gsa_ids <- as.integer(unlist(strsplit(gsas$GSA_IDs, ",")))
   gsa_names2 <- merge(data.frame(GSA_ID = gsa_ids), 
                       gsa_names, 
                       by = "GSA_ID")$GSA_Name
   gsa_names2 <- c(gsa_names2, 'groundwater_sustainability_agency', 'gsa')
   
   # tag places and DACs
   nl <- nl %>% 
      left_join(label_dict, by=join_by(entity_name == entity_name))%>% 
      left_join(govsci_dict, by=join_by(entity_name == Agency)) %>%
      mutate(org_type = case_when(
         State == 'local' ~ "Loc_Gov",
         State == 'federal' ~ "NL_Gov",
         State == 'California' ~ "CA_Gov",
         entity_name %in% gsa_names2 ~ "GSA",
         org_type == 'Ambig' ~ NA,
         org_type == 'Drop' ~ NA,
         TRUE ~ org_type
      )) %>%
      select(-c(X, State, Abbr)) %>%
      distinct(., entity_name, .keep_all = TRUE)

   el <- ve_w_sections %>% 
      filter(GSP_ID == gsp_id) %>% 
      mutate(weight = 1)
   
   networklist <- list("nodelist" = tibble(nl), "edgelist" = tibble(el))

   return(networklist)
}

# aggregate gsas (called in net_graph)
net_graph <- function(networklist, gsp_id) {
   
   graph <- igraph::graph_from_data_frame(networklist$edgelist,
                                          vertices = networklist$nodelist)
   
   # Identify GSA nodes
   gsas <- gsa_gsp %>% filter(GSP_ID == gsp_id)
   gsa_ids <- as.integer(unlist(strsplit(gsas$GSA_IDs, ",")))
   gsa_names2 <- merge(data.frame(GSA_ID = gsa_ids), 
                       gsa_names, 
                       by = "GSA_ID")$GSA_Name
   
   gsa_nodes <- V(graph)$name[V(graph)$name %in% gsa_names2]

   # Identify GSA-adjacent nodes
   gsa_adjacent_nodes <- V(graph)$name[!is.na(V(graph)$org_type) & V(graph)$org_type == 'GSA']
   gsa_adjacent_nodes <- setdiff(gsa_adjacent_nodes, gsa_nodes)
   
   for (adj_node in gsa_adjacent_nodes) {
      for (gsa_node in gsa_nodes) {
         if (str_detect(adj_node, gsa_node)) { #gsa name in adj node
            # Create a unique index for the merge
            merge_index <- which(V(graph)$name == gsa_node)
            
            # Find indices of the nodes to be merged
            to_merge <- V(graph)$name %in% c(gsa_node, adj_node)
            
            print(paste('merged', adj_node, 'into', gsa_node))
            
            # Merge nodes
            graph <- contract(graph, 
                              mapping = ifelse(to_merge, merge_index, seq_along(V(graph))), 
                              vertex.attr.comb = list(name="first",
                                                      entity_type='first',
                                                      num_appearances='sum',
                                                      org_type='first'))
            
            # fix attributes
            graph <- set_vertex_attr(graph,
                                     'org_type',
                                     index = which(V(graph)$name == gsa_node),
                                     value = 'GSA')
            
            V(graph)$name <- as.character(V(graph)$name)
            V(graph)$entity_type <- as.character(V(graph)$entity_type)
            V(graph)$org_type <- as.character(V(graph)$org_type)
            
            graph <- delete_vertices(graph, V(graph)[name == "character(0)"])
            
         } else if (str_detect(gsa_node, adj_node)) { #adj node in gsa name
            if (adj_node != gsa_node) {
               
               merge_index <- which(V(graph)$name == gsa_node)
               to_merge <- V(graph)$name %in% c(gsa_node, adj_node)
               print(paste('back-merged', adj_node, 'into', gsa_node))
               
               # Merge nodes
               graph <- contract(graph, 
                                 mapping = ifelse(to_merge, merge_index, seq_along(V(graph))), 
                                 vertex.attr.comb = list(name="first",
                                                         entity_type='first',
                                                         num_appearances='sum',
                                                         org_type='first'))
               
               # fix attributes
               graph <- set_vertex_attr(graph,
                                        'org_type',
                                        index = which(V(graph)$name == gsa_node),
                                        value = 'GSA')
               
               V(graph)$name <- as.character(V(graph)$name)
               V(graph)$entity_type <- as.character(V(graph)$entity_type)
               V(graph)$org_type <- as.character(V(graph)$org_type)
               
               graph <- delete_vertices(graph, V(graph)[name == "character(0)"])
            }
         } else {
            graph <- set_vertex_attr(graph,
                                     'org_type',
                                     index = which(V(graph)$name == adj_node),
                                     value = NA)
         } 
      }
   }
   
   graph <- set_vertex_attr(graph,
                            'GSA',
                            value = ifelse(V(graph)$org_type == 'GSA', 1, 0)
                            )
   
   graph <- set_vertex_attr(graph,
                            'GSA',
                            value = 0,
                            index = which(is.na(V(graph)$org_type) )
   )
   
   graph <- igraph::simplify(graph,
                             remove.multiple = TRUE,
                             remove.loops = FALSE,
                             edge.attr.comb = list(weight = 'sum',
                                                   neg = 'mean',
                                                   has_hedge = 'mean',
                                                   is_future = 'mean',
                                                   gsp_id = 'mean',
                                                   page_num = 'concat',
                                                   original_page_num = 'concat',
                                                   type_name = 'concat',
                                                   type_id = 'concat',
                                                   admin = 'sum',
                                                   basin_plan = 'sum',
                                                   sust_criteria = 'sum',
                                                   monitoring_networks = 'sum',
                                                   projects_mgmt_actions = 'sum',
                                                   'ignore'))
   
   networklist <- list('nodelist' = igraph::as_data_frame(graph, what='vertices'),
                       'edgelist' = igraph::as_data_frame(graph, what='edges'))
   
   network_graph <- network::network(networklist$edgelist, 
                                     vertex.attr = networklist$nodelist, 
                                     directed = TRUE,
                                     loops = TRUE,
                                     multiple = FALSE)
   
   igraph <- graph
   
   return(list(igraph = igraph, 
               network_graph = network_graph))
}


# Apply functions to all networks
for (g in seq_along(gsp_ids)) {
   
   gsp_id <- paste0("gsp_", gsp_ids[g])
   
   gsp_list <- net_process(file = paste0(network_fp, "/", extract_list[g]),
                              gsp_id = gsp_ids[g])
   
   gsp_graph <- net_graph(gsp_list, gsp_id = gsp_ids[g])
   
   saveRDS(object = gsp_graph$network_graph,
           file = paste0(Sys.getenv("BOX_PATH"),
                         "/network_structure_by_plan/cleaned_extracts",
                         "/", extract_list[g]))
   
   # remove isolates
   plot_graph <- delete_vertices(gsp_graph$igraph, which(igraph::degree(gsp_graph$igraph) == 0))
   
   ggraph::ggraph(plot_graph, layout = 'fr') +
      geom_edge_link(aes(edge_alpha = weight), show.legend = FALSE) +
      geom_node_point(aes(color = org_type), size = 5) +
      theme_void() +
      ggtitle(paste0("GSP: ", gsp_id))

   ggsave(paste0('Network_Structure_Paper/Out/gsp_graphs/', gsp_id, '.png'),
       width = 9, height = 9, dpi = 300)

   print(paste0("Finished GSP ", gsp_id))
}

# test functions for one network

idt <- 86
gsp_idt <- gsp_ids[idt]

glt <- net_process(file = paste0(network_fp, "/",extract_list[idt]),
                             gsp_id = gsp_idt)

ggt <- net_graph(glt, gsp_id = gsp_idt)

plot_graph <- delete_vertices(ggt$igraph, which(igraph::degree(ggt$igraph) == 0))

ggraph::ggraph(plot_graph, layout = 'fr') +
   geom_edge_link(aes(edge_alpha = weight), show.legend = FALSE) +
   geom_node_point(aes(color = org_type), size = 5) +
   # geom_node_text(aes(label = name), repel = TRUE) +
   theme_void() +
   # theme(legend.position = "none") +
   ggtitle(paste0("GSP: ", gsp_idt)) 
