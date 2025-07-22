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
library(ellmer)
library(dotenv)


load_dot_env('.env')
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

# --- CA Places and Counties: Use local cache if available ---

# Define file paths for local cache
ca_places_fp <- 'Network_Structure_Paper/Data/ca_places_2020.rds'
ca_counties_fp <- 'Network_Structure_Paper/Data/ca_counties_2020.rds'

# Load or create CA places
if (file.exists(ca_places_fp)) {
  ca_places1 <- readRDS(ca_places_fp)
} else {
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
  saveRDS(ca_places1, ca_places_fp)
}

ca_places2 <- ca_places1 %>% 
   mutate(NAME = paste("city_of", NAME, sep = "_"))

ca_places <- rbind(ca_places1, ca_places2)

# Load or create CA counties
if (file.exists(ca_counties_fp)) {
  ca_counties1 <- readRDS(ca_counties_fp)
} else {
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
  saveRDS(ca_counties1, ca_counties_fp)
}

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
                                      '268',
                                      '323'
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
                'svbgsa',
                'cga'
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
tag_nodes_first <- function(nl, gsp_id) {
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
         NATURAL_FEATURE = 0,
         INFRASTRUCTURE = 0,
         LOCAL_GSA = 0,
         OTHER_GSA = 0,
         OTHER_GSA2 = 0,
         LOCAL_GOV = 0,
         STATE_GOV = 0,
         FEDERAL_GOV = 0,
         DISTRICT = 0,
         DISTRICT2 = 0,
         GROUP = 0,
         DATA = 0,
         WATER_PROJECT = 0,
         REFERENCE = 0,
         GEO_UNIT = 0,
         LEGAL = 0,
         TECHNICAL = 0,
         AI_TAGGED = 0
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
         CITY = ifelse(tolower(entity_name) %in% ca_places$NAME | grepl("city_of", tolower(entity_name)), 1, CITY),
         COUNTY = ifelse(tolower(entity_name) %in% ca_counties$NAME | grepl('county$', tolower(entity_name)), 1, COUNTY)
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
            govsci_level == "california" | grepl("california", tolower(entity_name)),
            1, 
            STATE_GOV),
         FEDERAL_GOV = ifelse(
            govsci_level == "federal",
            1, 
            FEDERAL_GOV)
      )
   
   # tag specical districts
   
   nl <- nl %>%
      mutate(
         DISTRICT = ifelse(grepl("districts?$", tolower(entity_name)),
                           1, DISTRICT)
      )
   
   nl <- nl %>% 
      mutate(
         DISTRICT2 = ifelse(grepl("id|wd|rcd$", tolower(entity_name)),
                            1, DISTRICT2)
      )
   
   # Tag basins based on text content
   nl <- nl %>%
      mutate(
         BASIN = ifelse(grepl("(basins?)$", tolower(entity_name)), 1, BASIN)
      )
   
   # Tag natural features (water bodies, ecosystems, and water systems)
   nl <- nl %>%
      mutate(
         NATURAL_FEATURE = ifelse(grepl("(rivers?|lakes?|creeks?|sloughs?|ecosystems?|formations?|water_systems?|aquifers?|aquifer_systems?|clays?|sands?|silts?|loams?|alluvium|sediments?|natural_communities_commonly_associated_with_groundwater)$", tolower(entity_name)), 1, NATURAL_FEATURE)
      )
   
   # Tag built infrastructure
   nl <- nl %>%
      mutate(
         INFRASTRUCTURE = ifelse(grepl("(dams?|reservoirs?|weirs?)$", tolower(entity_name)), 1, INFRASTRUCTURE)
      )
   
   # Tag committees, boards, and working groups
   nl <- nl %>%
      mutate(
         GROUP = ifelse(grepl("(boards?|committees?|commissions?|community|communities|councils?|departments?|groups?|mou|board_of_directors?|board_of_supervisors?|agreements?|agency|agencies|authority|authorities)$", tolower(entity_name)), 1, GROUP)
      )
   
   # Tag data systems and monitoring programs
   nl <- nl %>%
      mutate(
         TECHNICAL = ifelse(grepl("((management|information)_systems?|plans?)$", tolower(entity_name)) | 
         grepl("(data|modflow|monitor|report|model|technical)", tolower(entity_name)), 
         1, TECHNICAL)
      )
   
   # Tag water projects
   nl <- nl %>%
      mutate(
         WATER_PROJECT = ifelse(grepl("(projects?)$", tolower(entity_name)), 1, WATER_PROJECT)
      )
   
   # Tag references to other parts of the document
   nl <- nl %>%
      mutate(
         REFERENCE = ifelse(grepl("(^|_)(appendix|exhibit|page|section|table|figure|map)(_|$)", tolower(entity_name)), 1, REFERENCE)
      )
   
   # tag regions, areas, or other geographic features
   
   nl <- nl %>%
      mutate(
         GEO_UNIT = ifelse(grepl("(regions?|areas?|valleys?|setting|zones?)$", tolower(entity_name)), 1, GEO_UNIT)
      )
   
   # tag laws
   
   nl <- nl %>%
      mutate(
         LEGAL = ifelse(grepl("(laws?|regulations?|acts?|bills?|statutes?|codes?|ordinances?|policy|policies?|guidelines?)$", tolower(entity_name)), 1, LEGAL)
      )
   
   
   # consolidate and finalize tags
   nl <- nl %>% 
      mutate(num_tags = rowSums(across(c(CITY, COUNTY, BASIN, NATURAL_FEATURE, 
                  INFRASTRUCTURE, LOCAL_GSA, OTHER_GSA, 
                  LOCAL_GOV, STATE_GOV, FEDERAL_GOV, DISTRICT, DISTRICT2, GROUP,
                  GEO_UNIT, DATA, WATER_PROJECT, REFERENCE, GEO_UNIT, LEGAL))))
   
   nl <- nl %>%
      mutate(
         entity_type = case_when(
            # listed in priority of confidence
            # specific named entities 
            LOCAL_GSA == 1 ~ "Local_GSA",
            OTHER_GSA == 1 ~ "Other_GSA",
            COUNTY == 1 ~ "County",
            CITY == 1 ~ "City",
            
            # government entities from govsci_dict and secondary GSA source
            LOCAL_GOV == 1 ~ "Local_Gov",
            STATE_GOV == 1 ~ "State_Gov",
            FEDERAL_GOV == 1 ~ "Federal_Gov",
            OTHER_GSA2 == 1 ~ "Other_GSA",
            
            # grepl based searches (ordered from most specific to most broad)
            DISTRICT == 1 ~ "District",
            DATA == 1 ~ "Data_System",
            INFRASTRUCTURE == 1 ~ "Infrastructure",
            NATURAL_FEATURE == 1 ~ "Natural_Feature",
            BASIN == 1 ~ "Basin",
            WATER_PROJECT == 1 ~ "Water_Project",
            REFERENCE == 1 ~ "Reference",
            GEO_UNIT == 1 ~ "Geographic_Unit",
            GROUP == 1 ~ "Group",
            LEGAL == 1 ~ "Legal",
            
            # wide net grepl searches (technical includes text inside of string and district2 is very vague)
            TECHNICAL == 1 ~ "Technical",
            DISTRICT2 == 1 ~ "District",

            TRUE ~ NA_character_ )) %>%
      select(-c(CITY, COUNTY, BASIN, NATURAL_FEATURE, INFRASTRUCTURE,
                OTHER_GSA, LOCAL_GOV, STATE_GOV, FEDERAL_GOV,
               GROUP, DATA, WATER_PROJECT, REFERENCE, OTHER_GSA2,
               DISTRICT, DISTRICT2, GEO_UNIT, LEGAL, TECHNICAL))
   
   return(nl)
}


# Second-pass node tagging function using ellmer/OpenAI
# This function uses GPT-4o-mini to classify entities that weren't tagged by tag_nodes_first
# It processes nodes in batches to avoid API rate limits and token limits
tag_nodes_second <- function(nl, gsp_id, batch_size = 20) {
   
   # Filter for nodes that don't have entity_type assigned
   untagged_nodes <- nl %>% 
      filter(is.na(entity_type))
   
   if (nrow(untagged_nodes) == 0) {
      cat("No untagged nodes found for GSP", gsp_id, "\n")
      return(nl)
   }
   
   cat("Processing", nrow(untagged_nodes), "untagged nodes for GSP", gsp_id, "\n")
   cat("Using batch size:", batch_size, "\n")
   
   # Define the tag structure for the AI model
   tag_definitions <- "
   Available tags:
   - Local_Gov: Local government entities (not cities/counties)
   - State_Gov: State government entities
   - Federal_Gov: Federal government entities
   - District: Special districts and water districts
   - Basin: Water basins and subbasins
   - Natural_Feature: Rivers, lakes, aquifers, ecosystems
   - Infrastructure: Dams, reservoirs, facilities
   - Group: Committees, boards, associations, authorities
   - Technical: Data systems, models, reports, monitoring
   - Water_Project: Water infrastructure projects
   - Reference: Document references (appendix, table, etc.)
   - Geographic_Unit: Geographic regions, areas, zones
   - Legal: Laws, regulations, policies, codes
   - Data_System: Data and information systems
   - Person: Both a first and last name of a person
   - NGO: A non-governmental organization, association, or foundation
   - Company: A company, corporation, or business
   - Nonsense: Nonsense entities  
   "
   
   # Split nodes into batches
   node_batches <- split(untagged_nodes, ceiling(seq_along(untagged_nodes$entity_name) / batch_size))
   
   all_results <- list()
   
   for (i in seq_along(node_batches)) {
      batch <- node_batches[[i]]
      
      # Create prompts for each entity in the batch
      prompts <- as.list(batch$entity_name)
      
      # Define the type structure for structured extraction
      type_entity_classification <- type_object(
         name = type_string("The name of the entity being classified, always written in snake_case and copied exactly from the document without removing or changing any characters."),
         type_string = type_enum(paste("The entity type classification. If in doubt, classify as Nonsense.", tag_definitions),
                                c( "Local_Gov", "State_Gov", "Federal_Gov", "District", "Basin", 
                                  "Natural_Feature", "Infrastructure", "Group", "Technical", 
                                  "Water_Project", "Reference", "Geographic_Unit", "Legal", 
                                  "Data_System", "Nonsense", "Person", "NGO", "Company"))
      )
      
      # Use ellmer parallel_chat_structured to process batch
      tryCatch({
         chat <- ellmer::chat_openai(system_prompt = 'You are an expert in categorizing entities from groundwater sustainability plans.',
                                     model='gpt-4.1-mini')
         
         response <- ellmer::parallel_chat_structured(
            chat,
            prompts,
            type = type_entity_classification
         )

         # Extract the results
         if (!is.null(response) && nrow(response) > 0) {
            # Response is a data frame with name and type_string columns
            batch_results <- data.frame(
               entity_name = response$name,
               ai_tag = response$type_string,
               stringsAsFactors = FALSE
            )
            all_results[[i]] <- batch_results
         } else {
            # Handle case where no classifications were returned
            all_results[[i]] <- data.frame(
               entity_name = batch$entity_name,
               ai_tag = NA_character_,
               stringsAsFactors = FALSE
            )
         }
         
         cat("Completed batch", i, "of", length(node_batches), "\n")
         
         # Rate limiting
         Sys.sleep(2)
         
      }, error = function(e) {
         cat("Error in batch", i, ":", e$message, "\n")
         # Return empty results for this batch
         all_results[[i]] <- data.frame(
            entity_name = batch$entity_name,
            ai_tag = NA_character_,
            stringsAsFactors = FALSE
         )
      })
   }
   
   # Combine all results
   if (length(all_results) > 0) {
      ai_results <- do.call(rbind, all_results)
      
      # Remove any duplicates and ensure proper matching
      ai_results <- ai_results %>%
         distinct(entity_name, .keep_all = TRUE) %>%
         filter(!is.na(ai_tag)) %>% 
         mutate(AI_TAGGED = 1)
      
      # Merge AI results back to the original nodelist
      nl <- nl %>%
         left_join(ai_results, by = "entity_name") %>%
         mutate(
            entity_type = coalesce(entity_type, ai_tag),
            AI_TAGGED = coalesce(AI_TAGGED.y, AI_TAGGED.x)
         ) %>%
         select(-c(ai_tag, AI_TAGGED.x, AI_TAGGED.y))
      
      # Count how many were successfully tagged
      newly_tagged <- sum(!is.na(ai_results$ai_tag))
      cat("Successfully tagged", newly_tagged, "out of", nrow(untagged_nodes), "entities\n")
   } else {
      cat("No AI results obtained\n")
   }
   
   return(nl)
}


# Enhanced net_process function with AI tagging toggle
net_process <- function(file, gsp_id, use_ai_tagging = FALSE){
   # grab nodelist
   nl <- tibble(readRDS(file)$nodelist) %>% 
      mutate(entity_name = str_remove(entity_name, "_s$"),
             entity_name = str_replace(entity_name, "_s_", "s_")) 
   
   # Apply first-pass tagging
   nl <- tag_nodes_first(nl, gsp_id)
   
   # Conditionally apply second-pass AI tagging for untagged nodes
   if (use_ai_tagging) {
      nl <- tag_nodes_second(nl, gsp_id)
   }

   # Keep existing org_type logic for backward compatibility
   nl <- nl %>% 
      left_join(label_dict, by=join_by(entity_name == entity_name))%>% 
      distinct(., entity_name, .keep_all = TRUE) %>% 
      select(-c(govsci_level, LOCAL_GSA))

   el <- ve_w_sections %>% 
      filter(GSP_ID == gsp_id) %>% 
      filter(source %in% nl$entity_name & target %in% nl$entity_name) %>%
      mutate(weight = 1)
   
   networklist <- list("nodelist" = tibble(nl), "edgelist" = tibble(el))

   return(networklist)
}

# aggregate gsas (called in net_graph)
net_graph <- function(networklist, gsp_id, remove_isolates = TRUE) {
   
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
   
   graph <- set_vertex_attr(graph,
                            'degree',
                            value = igraph::degree(graph))
   
   if (remove_isolates) {
      # Remove isolates
      graph <- delete_vertices(graph, which(igraph::degree(graph) == 0))
   }
   
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


# Function to generate abbreviation from underscore-separated terms
generate_abbreviation <- function(entity_name) {
   sapply(entity_name, function(name) {
      if (grepl("_", name)) {
         # Split by underscore and take first letter of each part
         parts <- strsplit(name, "_")[[1]]
         abbreviation <- paste(sapply(parts, function(x) substr(x, 1, 1)), collapse = "")
         return(tolower(abbreviation))
      } else {
         return(name)
      }
   }, USE.NAMES = FALSE)
}

reverse_abbreviation_lookup <- function(abbreviation, nodes_df, top_n = 5, name_col = "name") {
   
   # Convert abbreviation to lowercase for consistency
   abbrev_lower <- tolower(abbreviation)
   abbrev_letters <- strsplit(abbrev_lower, "")[[1]]
   
   # If num_appearances_sum is missing, add it with default value 1
   if (!"num_appearances_sum" %in% colnames(nodes_df)) {
      nodes_df$num_appearances_sum <- 1
   }
   
   # Create pattern: each letter should match the first letter of a word
   # For "cvp", this creates "^c[^_]*_v[^_]*_p[^_]*$"
   pattern_parts <- sapply(abbrev_letters, function(letter) {
      paste0(letter, "[^_]*")
   })
   
   # Join with underscores and add anchors
   pattern <- paste0("^", paste(pattern_parts, collapse = "_"), "$")
   
   # Dynamically refer to the column for lookup
   name_sym <- rlang::sym(name_col)
   
   # Find matching entities
   matches <- nodes_df %>%
      filter(grepl(pattern, !!name_sym, ignore.case = TRUE)) %>%
      mutate(
         # Perfect length match (same number of underscore-separated parts)
         word_count = stringr::str_count(!!name_sym, "_") + 1,
         expected_word_count = length(abbrev_letters),
         length_match = word_count == expected_word_count,
         
         # Calculate match score (higher is better)
         match_score = dplyr::case_when(
            length_match ~ num_appearances_sum * 2,  # Bonus for perfect word count match
            TRUE ~ num_appearances_sum
         ),
         
         # Add pattern confidence (exact first letters)
         abbreviation_check = generate_abbreviation(!!name_sym) == abbrev_lower,
         
         # Final score
         final_score = dplyr::case_when(
            abbreviation_check ~ match_score * 3,  # High confidence if abbreviation matches exactly
            length_match ~ match_score * 1.5,     # Medium confidence for word count match
            TRUE ~ match_score                     # Base score
         )
      ) %>%
      arrange(desc(final_score)) %>%
      select(
         !!name_sym, final_score, num_appearances_sum, entity_type, 
         abbreviation_check, length_match
      ) %>%
      head(top_n)
   
   return(matches)
}


#' Plot a network igraph object with custom color and shape schema for node types
#' @param igraph_obj An igraph object
#' @param title Optional plot title
#' @return ggplot object
plot_graph <- function(igraph_obj, title = NULL) {
  # Define color and shape mappings (order matters for legend)
  node_type_levels <- c(
    "Local_GSA", "Other_GSA", "District",
    "Local_Gov", "State_Gov", "Federal_Gov",
    "City", "County",
    "Basin", "Natural_Feature", "Geographic_Unit",
    "Infrastructure", "Water_Project",
    "Group", "NGO", "Company",
    "Technical", "Reference",
    "Legal",
    "Person",
    "Nonsense"
  )
  node_type_colors <- c(
    Local_GSA = "#20b2aa", Other_GSA = "#20b2aa", # Teal
    District = "#122382", 
    Local_Gov = "#6baed6", State_Gov = "#2171b5", Federal_Gov = "#08306b", # Blue shades
    City = "#ff9800", County = "#e65100", # Orange shades
    Basin = "#2ca02c", Natural_Feature = "#98df8a", Geographic_Unit = "#b2df8a", # Green shades
    Infrastructure = "#8e24aa", Water_Project = "#ce93d8", # Purple shades
    Group = "#8B0000", NGO = "#E57373", Company = "#FFCDD2", # Reds
    Technical = "#607d8b", Reference = "#b0bec5", Legal = "#757575", # Greys
    Person = "#ffd700", # Yellow
    Nonsense = "#000000" # Black
  )
  node_type_shapes <- c(
    Local_GSA = 24, # Triangle up
    Other_GSA = 25, # Triangle down
    District = 25,   # Hexagon
    Local_Gov = 22, State_Gov = 22, Federal_Gov = 22, # Square
    City = 21, County = 21, # Circle
    Basin = 21, Natural_Feature = 21, Geographic_Unit = 21, # Circle
    Infrastructure = 24, Water_Project = 24, # Triangle up
    Group = 23, NGO = 23, Company = 23, # Diamond
    Technical = 25, Reference = 25, # Triangle down
    Legal = 8, # Star
    Person = 21, # Circle
    Nonsense = 4 # Cross
  )

  # Get vertex data and set factor for entity_type
  vdf <- igraph::as_data_frame(igraph_obj, what = 'vertices')
  vdf$entity_type <- factor(vdf$entity_type, levels = node_type_levels)
  # Rebuild igraph with updated vertex attributes (if needed)
  ig <- igraph::graph_from_data_frame(
    igraph::as_data_frame(igraph_obj, what = 'edges'),
    vertices = vdf,
    directed = TRUE
  )

  p <- ggraph::ggraph(ig, layout = 'fr') +
    geom_edge_link(aes(edge_alpha = weight), show.legend = FALSE) +
    geom_node_point(
      aes(
        color = entity_type, fill = entity_type, shape = entity_type, size = degree
      ),
      stroke = 1.2
    ) +
    scale_color_manual(
      values = node_type_colors,
      breaks = node_type_levels,
      name = "Node Type"
    ) +
    scale_fill_manual(
      values = node_type_colors,
      breaks = node_type_levels,
      name = "Node Type"
    ) +
    scale_shape_manual(
      values = node_type_shapes,
      breaks = node_type_levels,
      name = "Node Type"
    ) +
    guides(
      color = guide_legend(override.aes = list(size = 5)),
      fill = guide_legend(override.aes = list(size = 5)),
      shape = guide_legend(override.aes = list(size = 5))
    ) +
    theme_void()
  if (!is.null(title)) {
    p <- p + ggtitle(title)
  }
  return(p)
}

# nolint end 
