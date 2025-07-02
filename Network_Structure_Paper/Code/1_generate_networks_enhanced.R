# nolint start

library(dotenv)
library(ggraph)
library(igraph)
library(tidyverse)
library(migraph)
library(data.table)
library(statnet)
library(stringr)
library(tidycensus)

load_dot_env()
setwd(Sys.getenv('WD'))

network_fp <- paste0(Sys.getenv("BOX_PATH"),
                     "/Supernetwork_Paper/cleaned_unfiltered_extracts")

label_fp <- paste0(Sys.getenv("BOX_PATH"),
                   "/Multipurpose_Files/Dictionaries/googlesheets_dt_complete.csv")
label_dict <- read.csv(label_fp) %>% 
   tibble()

govsci_fp <- paste0(Sys.getenv("BOX_PATH"),
                    "/Multipurpose_Files/Dictionaries/govsci_tbl_noblank.csv")
govsci_dict1 <- read.csv(govsci_fp) %>% 
   tibble() %>% 
   mutate(Abbr = tolower(Abbr)) %>%
   rename(govsci_agency = Agency,
          govsci_level = State) %>% 
   select(-X)

govsci_dict2 <- govsci_dict1 %>% 
   filter(str_detect(govsci_agency, 'united_states')) %>% 
   mutate(govsci_agency = str_replace(govsci_agency, 
                                      "united_states", "us"))

# merge
govsci_dict <- rbind(govsci_dict1, govsci_dict2) 

# load the scowl word list
wl <- readLines('Network_Structure_Paper/Data/wl.txt')

extract_list <- list.files(network_fp)
extract_list <- setdiff(extract_list, c("0053.RDS", '0089.RDS'))
gsp_ids <- gsub("^0+", "", gsub("\\.RDS", "", extract_list))

gsa_gsp <- tibble(read.csv('EJ_DAC_Paper/Data/gsa_gsp.csv'))
gsa_names <- read.csv('EJ_DAC_Paper/Data/gsa_names.csv') %>% 
   tibble()

ca_places1 <- get_acs(
  geography = "place",
  cache_table = TRUE,
  variables = "B01003_001",  # Total population
  state = "CA",
  year = 2020,
  survey = "acs5"
) %>%
  mutate(NAME = tolower(NAME),
         NAME = str_remove(NAME, " cdp.*?, california"),
         NAME = str_remove(NAME, ' city, california'),
         NAME = str_remove(NAME, ' town, california'),
         NAME = str_replace_all(NAME, ' ', '_')) %>%
  select(NAME) %>%
  distinct()

ca_places2 <- ca_places1 %>% 
   mutate(NAME = paste("city_of", NAME, sep = "_"))

ca_places <- rbind(ca_places1, ca_places2)

ca_counties1 <- get_acs(
  geography = "county",
  cache_table = TRUE,
  variables = "B01003_001",  # Total population
  state = "CA",
  year = 2020,
  survey = "acs5"
) %>%
  mutate(NAME = tolower(NAME), 
         NAME = str_remove(NAME, " county, california"),
         NAME = str_replace_all(NAME, ' ', '_')) %>%
  select(NAME) %>%
  distinct()

ca_counties2 <- ca_counties1 %>% 
   mutate(NAME = paste(NAME, "county", sep = "_")) 

ca_counties3 <- ca_counties1 %>% 
   mutate(NAME = paste("county_of", NAME, sep = "_"))

ca_counties <- rbind(ca_counties1, ca_counties2, ca_counties3)

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
   
   # add abbreviations for each GSA
   stopwords <- c("of", "and", "the", "for", "in", "on", "at", "by", "to", "with", "a", "an")
   
   abbreviate_gsa_name <- function(name) {
      name %>%
         str_split("_") %>%                # Split by underscore
         unlist() %>%
         tolower() %>%
         setdiff(stopwords) %>%            # Remove stopwords
         str_sub(1, 1) %>%                 # Take the first letter of each word
         str_c(collapse = "") %>%          # Collapse into a single string
         tolower()                         # Make uppercase (optional)
   }
   
   # Apply to your tibble
   gsa_names_4 <- gsa_names %>%
      mutate(GSA_Name = sapply(GSA_Name, abbreviate_gsa_name))
      
   
   # add in additional GSA names
   gsa_names_5 <- data.frame(GSA_ID=c('147',
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
   
   gsa_names <- rbind(gsa_names, gsa_names_2, gsa_names_3, gsa_names_4, gsa_names_5)
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

# node tagging function
tag_nodes_enhanced <- function(nl, gsp_id) {
   # Get GSA names for this GSP
   gsas <- gsa_gsp %>% filter(GSP_ID == gsp_id)
   gsa_ids <- as.integer(unlist(strsplit(gsas$GSA_IDs, ",")))
   gsa_names2 <- merge(data.frame(GSA_ID = gsa_ids), 
                       gsa_names, 
                       by = "GSA_ID")$GSA_Name
   gsa_names2 <- c(gsa_names2, 'groundwater_sustainability_agency', 'gsa')
   
   # Initialize tag columns
   nl <- nl %>%
      mutate(
         CITY = 0,
         COUNTY = 0,
         BASIN = 0,
         WATER_BODY = 0,
         INFRASTRUCTURE = 0,
         LOCAL_GSA = 0,
         OTHER_GSA = 0,
         OTHER_GSA2 = 0,
         LOCAL_GOV = 0,
         STATE_GOV = 0,
         FEDERAL_GOV = 0,
         WATER_DISTRICT = 0,
         IRR_DISTRICT = 0,
         CONS_DISTRICT = 0
      )
   
   # Add in government tags 
   nl <- nl %>% 
      left_join(govsci_dict, by = join_by(entity_name == govsci_agency)) %>%
      left_join(govsci_dict, by = join_by(entity_name == Abbr)) %>% 
      mutate(govsci_level = coalesce(govsci_level.x, govsci_level.y)) %>% 
      mutate(govsci_level = ifelse(is.na(govsci_level), "not_gov", tolower(govsci_level))) %>% 
      select(-c('Abbr', 'govsci_agency', 'govsci_level.x', 'govsci_level.y'))
   
   
   
   # Tag cities and counties based on tidycensus data
   nl <- nl %>%
      mutate(
         CITY = ifelse(tolower(entity_name) %in% ca_places$NAME, 1, CITY),
         COUNTY = ifelse(tolower(entity_name) %in% ca_counties$NAME, 1, COUNTY)
      )

   # Tag local GSAs (existing logic)
   nl <- nl %>%
      mutate(
         LOCAL_GSA = ifelse(entity_name %in% gsa_names2, 1, LOCAL_GSA)
      )
   
   # Tag other GSAs (contains GSA terms but not local GSA)
   nl <- nl %>%
      mutate(
         OTHER_GSA = ifelse(entity_name %in% gsa_names$GSA_Name, 1, OTHER_GSA),
         OTHER_GSA2 = ifelse(grepl("(groundwater_sustainability_agency|gsa)$", tolower(entity_name)), 1, OTHER_GSA2)
      )
   
   # Tag local, state, and federal government entities
   
   nl <- nl %>% 
      mutate(
         LOCAL_GOV = ifelse(
            govsci_level == "local",
            1, 
            LOCAL_GOV),
         STATE_GOV = ifelse(
            govsci_level == "california",
            1, 
            STATE_GOV),
         FEDERAL_GOV = ifelse(
            govsci_level == "federal",
            1, 
            FEDERAL_GOV)
      )
   
   # tag water-related specical districts
   
   nl <- nl %>%
      mutate(
         WATER_DISTRICT = ifelse(grepl("water_district$", tolower(entity_name)), 1, WATER_DISTRICT),
         IRR_DISTRICT = ifelse(grepl("irrigation_district$", tolower(entity_name)), 1, IRR_DISTRICT),
         CONS_DISTRICT = ifelse(grepl("conservation_district$", tolower(entity_name)), 1, CONS_DISTRICT)
      )
   
   # Tag basins based on text content
   nl <- nl %>%
      mutate(
         BASIN = ifelse(grepl("(basin|subbasin)$", tolower(entity_name)), 1, BASIN)
      )
   
   # Tag water bodies based on text content
   nl <- nl %>%
      mutate(
         WATER_BODY = ifelse(grepl("(river|lake|creek|slough)$", tolower(entity_name)), 1, WATER_BODY)
      )
   
   # Tag built infrastructure
   nl <- nl %>%
      mutate(
         INFRASTRUCTURE = ifelse(grepl("(dam|reservoir|weir)$", tolower(entity_name)), 1, INFRASTRUCTURE)
      )
   
   # consolidate and finalize tags
   nl <- nl %>% 
      mutate(num_tags = rowSums(across(c(CITY, COUNTY, BASIN, WATER_BODY, 
                  INFRASTRUCTURE, LOCAL_GSA, OTHER_GSA, 
                  LOCAL_GOV, STATE_GOV, FEDERAL_GOV, WATER_DISTRICT,
                  IRR_DISTRICT, CONS_DISTRICT))))
   
   nl <- nl %>%
      mutate(
         entity_type = case_when(
            LOCAL_GSA == 1 ~ "Local_GSA",
            OTHER_GSA == 1 ~ "Other_GSA",
            COUNTY == 1 ~ "County",
            CITY == 1 ~ "City",
            
            WATER_DISTRICT == 1 ~ "Water_District",
            IRR_DISTRICT == 1 ~ "Irrigation_District",
            CONS_DISTRICT == 1 ~ "Conservation_District",
            
            LOCAL_GOV == 1 ~ "Local_Gov",
            STATE_GOV == 1 ~ "State_Gov",
            FEDERAL_GOV == 1 ~ "Federal_Gov",
            OTHER_GSA2 == 1 ~ "Other_GSA",
            
            INFRASTRUCTURE == 1 ~ "Infrastructure",
            WATER_BODY == 1 ~ "Water_Body",
            BASIN == 1 ~ "Basin",
            
            TRUE ~ NA_character_ )) %>%
      select(-c(CITY, COUNTY, BASIN, WATER_BODY, INFRASTRUCTURE,
                OTHER_GSA, LOCAL_GOV, STATE_GOV, FEDERAL_GOV))
   
   return(nl)
}

# Enhanced net_process function
net_process <- function(file, gsp_id){
   # grab nodelist
   nl <- tibble(readRDS(file)$nodelist)

   # Apply enhanced tagging
   nl <- tag_nodes_enhanced(nl, gsp_id)
   
   # Keep existing org_type logic for backward compatibility
   nl <- nl %>% 
      left_join(label_dict, by=join_by(entity_name == entity_name))%>% 
      # mutate(org_type = case_when(
      #    govsci_level == 'local' ~ "Loc_Gov",
      #    govsci_level == 'federal' ~ "NL_Gov",
      #    govsci_level == 'california' ~ "CA_Gov",
      #    org_type == 'Ambig' ~ NA,
      #    org_type == 'Drop' ~ NA,
      #    LOCAL_GSA == 1 ~ "GSA",
      #    TRUE ~ org_type
      # )) %>%
      distinct(., entity_name, .keep_all = TRUE) %>% 
      select(-c(govsci_level, LOCAL_GSA))

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

na_nodes_to_be_tagged <- data.frame(
   gsp_id = integer(),
   entity_name = character(),
   num_appearances = integer()
)

# Apply functions to all networks
for (g in seq_along(gsp_ids)) {
   
   gsp_id <- paste0("gsp_", gsp_ids[g])
   
   gsp_list <- net_process(file = paste0(network_fp, "/", extract_list[g]),
                           gsp_id = gsp_ids[g])
   
   na_nodes <- gsp_list$nodelist %>%
      filter(is.na(entity_type)) %>%
      select(entity_name, num_appearances) %>% 
      mutate(gsp_id = gsp_ids[g]) %>%
      head(10)
   
   na_nodes_to_be_tagged <- rbind(na_nodes_to_be_tagged, na_nodes)
      
   gsp_graph <- net_graph(gsp_list, gsp_id = gsp_ids[g])
   
   saveRDS(object = gsp_graph$network_graph,
           file = paste0(Sys.getenv("BOX_PATH"),
                         "/network_structure_by_plan/networks_fully_labeled",
                         "/", extract_list[g]))
   
   # remove isolates
   plot_graph <- delete_vertices(gsp_graph$igraph, which(igraph::degree(gsp_graph$igraph) == 0))
   
   ggraph::ggraph(plot_graph, layout = 'fr') +
      geom_edge_link(aes(edge_alpha = weight), show.legend = FALSE) +
      geom_node_point(aes(color = entity_type), size = 5) +
      theme_void() +
      ggtitle(paste0("GSP: ", gsp_id))
   
   ggsave(paste0('Network_Structure_Paper/Out/gsp_graphs/', gsp_id, '.png'),
          width = 9, height = 9, dpi = 300)
   
   print(paste0("Finished GSP ", gsp_id))
}

na_nodes_to_be_tagged_final <- na_nodes_to_be_tagged %>%
   group_by(entity_name) %>%
   summarise(
      num_appearances = sum(num_appearances),
      num_gsps = length(unique(gsp_id))
   ) %>%
   arrange(desc(num_appearances))



# test code

idt <- 50
gsp_idt <- gsp_ids[idt]

glt <- net_process(file = paste0(network_fp, "/",extract_list[idt]),
                   gsp_id = gsp_idt)

ggt <- net_graph(glt, gsp_id = gsp_idt)

plot_graph <- delete_vertices(ggt$igraph, which(igraph::degree(ggt$igraph) == 0))

# add degree to plot_graph

plot_graph <- set_vertex_attr(plot_graph,
                              'degree',
                              value = igraph::degree(plot_graph))

igraph::as_data_frame(plot_graph, what='vertices') %>% 
   as_tibble %>% 
   # filter(is.na(org_type)) %>% 
   arrange(desc(degree))

ggraph::ggraph(plot_graph, layout = 'fr') +
   geom_edge_link(aes(edge_alpha = weight), show.legend = FALSE) +
   geom_node_point(aes(color = entity_type, size=degree)) +
   # geom_node_text(aes(label = name), repel = TRUE) +
   theme_void() +
   # theme(legend.position = "none") +
   ggtitle(paste0("GSP: ", gsp_idt)) 

# nolint end 
