library(dotenv)
library(ggraph)
library(igraph)
library(tidyverse)
library(igraph)
library(data.table)
library(statnet)
library(tidyverse)
library(netUtils)
library(network)
library(sna)
library(dplyr)
library(tidyr)

# first chunk Aaron part, the orgin of each dataset can be find in Structure(branch)/Network_structure/1_generate_networks
# In addition to Aaron’s, replace NA in org_label if Entity_type is PERSON/GPE
network_fp <- list.files(path = "New_Policy_Tool_Paper/Box_link/Network_policy_tool_paper/wendysong/cleaned_unfiltered_extracts", pattern = "*.RDS", full.names = TRUE)

wl <- readLines("New_Policy_Tool_Paper/Box_link/Network_policy_tool_paper/wendysong/Node_label/wl.txt")

extract_list <- network_fp[!grepl("0089.RDS|0053.RDS", network_fp)]

gsp_ids <- gsub("^0+", "", gsub("\\.RDS", "", basename(extract_list)))

gsa_gsp <- tibble(read.csv('New_Policy_Tool_Paper/Box_link/Network_policy_tool_paper/wendysong/Node_label/gsa_gsp.csv'))
gsa_names <- read.csv('New_Policy_Tool_Paper/Box_link/Network_policy_tool_paper/wendysong/Node_label/gsa_names.csv')

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

pages_fp <- 'New_Policy_Tool_Paper/Box_link/Network_policy_tool_paper/wendysong/Node_label/gsp_docs_lean'

page_features <- tibble(readRDS(pages_fp)) %>% 
   mutate(gsp_id = as.numeric(gsp_id)) %>%
   filter(is_comment == FALSE & is_reference == FALSE) %>% 
   mutate(original_page_num = as.numeric(page_num),
          page_num = ave(1:nrow(.), gsp_id, FUN = seq_along)) %>% #renumber pages after removing comments/reference
   select(c("gsp_id", "page_num", "admin", "basin_plan", 
            "sust_criteria", "monitoring_networks", 
            "projects_mgmt_actions", "is_comment", "is_reference", "original_page_num"))

edge_fp <- 'New_Policy_Tool_Paper/Box_link/Network_policy_tool_paper/wendysong/Node_label/edgelist_with_verb_meta'
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

label_fp <- 'New_Policy_Tool_Paper/Box_link/Network_policy_tool_paper/wendysong/Node_label/googlesheets_dt_complete.csv'
label_dict <- read.csv(label_fp)

govsci_fp <- 'New_Policy_Tool_Paper/Box_link/Network_policy_tool_paper/wendysong/Node_label/govsci_tbl_noblank.csv'
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
   ## Wendy Addition, Replace org_type NA if entity_type == GPE or PERSON
         mutate(org_type = if_else(is.na(org_type) & entity_type %in% c("GPE", "PERSON"),
                                entity_type,
                                org_type)) %>%
      
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
   
   gsp_list <- net_process(file = extract_list[g],
                              gsp_id = gsp_ids[g])
   
   gsp_graph <- net_graph(gsp_list, gsp_id = gsp_ids[g])
   
   
   # save
   filename <- basename(extract_list[g])
   file = file.path("New_Policy_Tool_Paper/Box_link/Network_policy_tool_paper/wendysong/cleaned_extracts_org", filename)
   saveRDS(object = gsp_graph$igraph,
           file = file)
   
   # remove isolates
   plot_graph <- delete_vertices(gsp_graph$igraph, which(igraph::degree(gsp_graph$igraph) == 0))
   
   #ggraph::ggraph(plot_graph, layout = 'fr') +
   #   geom_edge_link(aes(edge_alpha = weight), show.legend = FALSE) +
   #   geom_node_point(aes(color = org_type), size = 5) +
   #   theme_void() +
   #   ggtitle(paste0("GSP: ", gsp_id))

   #ggsave(paste0("New_Policy_Tool_Paper/Box_link/Network_policy_tool_paper/wendysong/Node_label/graphs_na_adjust/", gsp_id, '.png'),
   #    width = 9, height = 9, dpi = 300)

   print(paste0("Finished GSP ", gsp_id))
}



########## Wendy: figure out whats NA ########

## Use gsp_7 for testing ✅ 
file_path <- "New_Policy_Tool_Paper/Box_link/Network_policy_tool_paper/wendysong/cleaned_extracts_org/0007.rds"
demo <- readRDS(file_path)

nodelist <- igraph::as_data_frame(demo, what = "vertices") %>%
   select(name, org_type)

edgelist <- igraph::as_data_frame(demo, what = "edges") %>%
   select(from, to, weight) %>%
   na.omit()

# un-collapse since netUtils does not support weighted
edgelist_expanded <- edgelist %>%
   filter(weight >= 1) %>%
   rowwise() %>%
   mutate(repeat_list = list(rep(1, round(weight)))) %>%
   unnest(repeat_list) %>%
   select(from, to)

# non_isolated
all_nodes <- unique(c(edgelist$from, edgelist$to))
nodelist <- nodelist[nodelist$name %in% all_nodes, ]

g <- igraph::graph_from_data_frame(d = edgelist_expanded, vertices = nodelist, directed = TRUE)


# Core–periphery analysis using netUtils::vec
cp_fit <- netUtils::core_periphery(g)
cp_fit$corr
core_vec <- cp_fit$vec

core_df <- tibble(
   name = names(core_vec),
   core = core_vec
) %>%
   left_join(nodelist, by = "name") %>%
   select(name, org_type, core)
print(head(core_df))


## Start LOOOOOOOOOOOOPING 
input_dir <- list.files(path = "New_Policy_Tool_Paper/Box_link/Network_policy_tool_paper/wendysong/cleaned_extracts_org/", pattern = "*.RDS", full.names = TRUE)
output_dir <- "New_Policy_Tool_Paper/Box_link/Network_policy_tool_paper/wendysong/core_output_labeled/"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)


for (i in 1:length(input_dir))  {
   file_name <- tools::file_path_sans_ext(basename(input_dir[i]))
   out_path <- file.path(output_dir, paste0(file_name, ".csv"))
   
   demo <- readRDS(input_dir[i])
   
   nodelist <- igraph::as_data_frame(demo, what = "vertices") %>%
      select(name, org_type)
   
   edgelist <- igraph::as_data_frame(demo, what = "edges") %>%
      select(from, to, weight) %>%
      na.omit()
   
   edgelist_expanded <- edgelist %>%
      filter(weight >= 1) %>%
      rowwise() %>%
      mutate(repeat_list = list(rep(1, round(weight)))) %>%
      unnest(repeat_list) %>%
      select(from, to)
   
   all_nodes <- unique(c(edgelist_expanded$from, edgelist_expanded$to))
   nodelist_filtered <- nodelist[nodelist$name %in% all_nodes, ]
   
   g <- igraph::graph_from_data_frame(edgelist_expanded, vertices = nodelist_filtered, directed = TRUE)
   
   cp_fit <- netUtils::core_periphery(g)
   core_vec <- cp_fit$vec
   
   core_df <- tibble(
      name = names(core_vec),
      core = core_vec
   ) %>%
      left_join(nodelist_filtered, by = "name") %>%
      select(name, org_type, core)
   
   # Save to CSV
   write.csv(core_df, out_path, row.names = FALSE)
   message("✅ Processed: ", file_name)
}



###### Summary Statistics for Core

input_dir <- "New_Policy_Tool_Paper/Box_link/Network_policy_tool_paper/wendysong/core_output_labeled/"
csv_files <- list.files(input_dir, pattern = "\\.csv$", full.names = TRUE)

all_summaries <- list()

for (file in csv_files) {
   gsp_id <- tools::file_path_sans_ext(basename(file))  # e.g., "0007"
   df <- read_csv(file, show_col_types = FALSE)
   
   # Filter for core nodes
   df_core <- df %>% filter(core == 1)
   
   # Summarize org_type counts
   summary <- df_core %>%
      count(org_type, name = "count") %>%
      mutate(gsp_id = gsp_id) %>%
      select(gsp_id, org_type, count)
   
   # Add names of NA org_type nodes
   na_names <- df_core %>%
      filter(is.na(org_type)) %>%
      pull(name)
   
   if (length(na_names) > 0) {
      summary <- summary %>%
         mutate(na_node_names = ifelse(is.na(org_type), paste(na_names, collapse = "; "), NA))
   } else {
      summary <- summary %>%
         mutate(na_node_names = NA)
   }
   
   all_summaries[[gsp_id]] <- summary
}

combined_summary <- bind_rows(all_summaries)
write_csv(combined_summary, file.path(output_dir, "core_node_summary_all.csv"))



## Most Frequent NA org_type names
output_dir <- "New_Policy_Tool_Paper/Box_link/Network_policy_tool_paper/wendysong/core_output_labeled/summary_statistics/"
na_name_counts <- list()

for (file in csv_files) {
   df <- read_csv(file, show_col_types = FALSE)
   
   na_core_names <- df %>%
      filter(core == 1, is.na(org_type)) %>%
      pull(name)
   
   if (length(na_core_names) > 0) {
      na_name_counts[[basename(file)]] <- na_core_names
   }
}

all_na_names <- unlist(na_name_counts)
na_name_summary <- as_tibble(table(all_na_names)) %>%
   arrange(desc(n)) %>%
   rename(name = all_na_names, frequency = n)

write_csv(na_name_summary, file.path(output_dir, "na_orgtype_core_name_frequency.csv"))


## percentage of each org_type that are core per plan

summary_all <- list()

for (file in csv_files) {
   df <- read_csv(file, show_col_types = FALSE)
   
   gsp_id <- tools::file_path_sans_ext(basename(file))
   
   df_core <- df %>% filter(core == 1)
   
   if (nrow(df_core) == 0) next  
   
   total_core <- nrow(df_core)
   
   summary_df <- df_core %>%
      count(org_type, name = "count_core") %>%
      mutate(
         gsp_id = gsp_id,
         percent_core = round((count_core / total_core) * 100, 1)
      ) %>%
      select(gsp_id, org_type, percent_core)
   
   summary_all[[gsp_id]] <- summary_df
}
summary_table <- bind_rows(summary_all)
write_csv(summary_table, file.path(output_dir, "core_orgtype_percent_by_plan.csv"))

# Distribution of org_type in core among all plans
all_core_nodes <- list()

for (file in csv_files) {
   df <- read_csv(file, show_col_types = FALSE)
   df_core <- df %>% filter(core == 1)
   
   if (nrow(df_core) > 0) {
      all_core_nodes[[file]] <- df_core
   }
}

combined_core_df <- bind_rows(all_core_nodes)
overall_summary <- combined_core_df %>%
   count(org_type, name = "count_core") %>%
   mutate(percent_core = round(100 * count_core / sum(count_core), 1)) %>%
   arrange(desc(percent_core))
write_csv(overall_summary, file.path(output_dir, "core_orgtype_percent_overall.csv"))
