# step_3_generate_networks_tagged.R
# Generates networks combining:
#   - EJ_DAC_Paper's place/DAC demographic tagging (all_places.csv)
#   - Network_Structure_Paper's entity_type classification (rule-based + LLM)
# Uses pre-existing entity_type lookup from all_nodes_raw.csv to avoid LLM API calls.
# Falls back to rule-based tagging via tag_nodes_first() for any unmatched nodes.

library(dotenv)
library(ggraph)
library(igraph)
library(tidyverse)
library(migraph)
library(data.table)
library(stringr)

load_dot_env()

# --- Data sources (shared with original EJ pipeline) ---

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
   select(c("gsp_id", "page_num", "admin", "basin_plan",
            "sust_criteria", "monitoring_networks",
            "projects_mgmt_actions", "is_comment", "is_reference", "original_page_num"))

# verb-based edgelist
edge_fp <- paste0(Sys.getenv("BOX_PATH"), "/Verb_Analysis_Paper/edgelist_with_verb_meta")
ve <- readRDS(edge_fp) %>%
   as_tibble() %>%
   filter(!is.na(source) & !is.na(target)) %>%
   filter(head_verb_lemma %in% wl) %>%
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

# --- Entity type lookup from Network_Structure_Paper ---
# This avoids re-running the LLM by using pre-computed tags.
# all_nodes_raw has per-GSP entity_type assignments (rule-based + LLM).

entity_lookup_raw <- read.csv("Network_Structure_Paper/Out/all_nodes_raw.csv") %>%
   tibble() %>%
   select(name, entity_type, AI_TAGGED, gsp_id) %>%
   filter(!is.na(entity_type))

# Also load the cross-GSP aggregated lookup as fallback
entity_lookup_agg <- read.csv("Network_Structure_Paper/Out/all_nodes_cleaned.csv") %>%
   tibble() %>%
   select(name, entity_type, AI_TAGGED) %>%
   filter(!is.na(entity_type))

# --- Source the rule-based tagging function for fallback ---
# We need tag_nodes_first() for any nodes not in the lookup.
# Load the required supporting data and function from Network_Structure_Paper.

gsa_gsp <- tibble(read.csv("EJ_DAC_Paper/Data/gsa_gsp.csv"))
gsa_names <- read.csv("EJ_DAC_Paper/Data/gsa_names.csv") %>% tibble()

# Load govsci dictionary
govsci_fp <- paste0(Sys.getenv("BOX_PATH"),
                    "/Multipurpose_Files/Dictionaries/govsci_tbl_noblank.csv")
govsci_dict1 <- read.csv(govsci_fp) %>%
   tibble() %>%
   mutate(Abbr = tolower(Abbr)) %>%
   rename(govsci_agency = Agency,
          govsci_level = State) %>%
   select(-X)
govsci_dict2 <- govsci_dict1 %>%
   filter(str_detect(govsci_agency, "united_states")) %>%
   mutate(govsci_agency = str_replace(govsci_agency, "united_states", "us"))
govsci_dict <- rbind(govsci_dict1, govsci_dict2)

# Load CA places/counties for rule-based tagging
ca_places_fp <- "Network_Structure_Paper/Data/ca_places_2020.rds"
ca_counties_fp <- "Network_Structure_Paper/Data/ca_counties_2020.rds"

if (file.exists(ca_places_fp)) {
   ca_places1 <- readRDS(ca_places_fp)
} else {
   library(tidycensus)
   ca_places1 <- get_acs(geography = "place", cache_table = TRUE,
                         variables = "B01003_001", state = "CA",
                         year = 2020, survey = "acs5") %>%
      mutate(NAME = tolower(NAME),
             NAME = str_remove(NAME, " cdp.*?, california"),
             NAME = str_remove(NAME, " city, california"),
             NAME = str_remove(NAME, " town, california"),
             NAME = str_replace_all(NAME, " ", "_")) %>%
      select(NAME) %>% distinct()
   saveRDS(ca_places1, ca_places_fp)
}
ca_places2 <- ca_places1 %>% mutate(NAME = paste("city_of", NAME, sep = "_"))
ca_places <- rbind(ca_places1, ca_places2)

if (file.exists(ca_counties_fp)) {
   ca_counties1 <- readRDS(ca_counties_fp)
} else {
   library(tidycensus)
   ca_counties1 <- get_acs(geography = "county", cache_table = TRUE,
                           variables = "B01003_001", state = "CA",
                           year = 2020, survey = "acs5") %>%
      mutate(NAME = tolower(NAME),
             NAME = str_remove(NAME, " county, california"),
             NAME = str_replace_all(NAME, " ", "_")) %>%
      select(NAME) %>% distinct()
   saveRDS(ca_counties1, ca_counties_fp)
}
ca_counties2 <- ca_counties1 %>% mutate(NAME = paste(NAME, "county", sep = "_"))
ca_counties3 <- ca_counties1 %>% mutate(NAME = paste("county_of", NAME, sep = "_"))
ca_counties <- rbind(ca_counties1, ca_counties2, ca_counties3)

# Clean GSA names (same logic as both papers)
clean_gsa_names <- function(gsa_names) {
   gsa_names_2 <- gsa_names
   gsa_names_2$GSA_Name <- str_replace(gsa_names$GSA_Name,
                                       "groundwater_sustainability_agency",
                                       "gsa")
   gsa_names_3 <- gsa_names %>%
      mutate(GSA_Name = str_replace(gsa_names$GSA_Name,
                                    "_groundwater_sustainability_agency",
                                    "")) %>%
      filter(grepl("groundwater", GSA_Name))

   stopwords <- c("of", "and", "the", "for", "in", "on", "at", "by", "to", "with", "a", "an")
   abbreviate_gsa_name <- function(name) {
      name %>% str_split("_") %>% unlist() %>% tolower() %>%
         setdiff(stopwords) %>% str_sub(1, 1) %>% str_c(collapse = "") %>% tolower()
   }
   gsa_names_4 <- gsa_names %>%
      mutate(GSA_Name = sapply(GSA_Name, abbreviate_gsa_name))

   gsa_names_5 <- data.frame(
      GSA_ID = c('147','461','457','253','456','106','415','418','384',
                 '432','163','433','434','49','24','403','47','422',
                 '124','420','419','268','323','349'),
      GSA_Name = c('sacramento_central_groundwater_authority',
                   'salinas_valley_basin_groundwater_sustainability_agency',
                   'siskiyou_county_flood_control_and_water_conservation_district_groundwater_sustainability_agency_butte_valley',
                   'siskiyou_county_flood_control_and_water_conservation_district_groundwater_sustainability_agency_scott_river',
                   'siskiyou_county_flood_control_and_water_conservation_district_groundwater_sustainability_agency_shasta',
                   'yuba_water_agency', 'yuba_water_agency',
                   'tehama_county_flood_control_and_water_conservation_district',
                   'reclamation_district_no_501_groundwater_sustainability_agency_northern_delta_groundwater_sustainability_agency',
                   'fox_canyon_groundwater_management_agency',
                   'fox_canyon_groundwater_management_agency',
                   'fox_canyon_groundwater_management_agency',
                   'fox_canyon_groundwater_management_agency',
                   'arroyo_santa_rosa_groundwater_sustainability_agency',
                   'mga', 'owens_valley_groundwater_authority',
                   'madera_co_groundwater_sustainability_agency',
                   'tehama_county_flood_control_and_water_conservation_district_groundwater_sustainability_agency_antelope',
                   'tehama_county_flood_control_and_water_conservation_district_groundwater_sustainability_agency_bowman',
                   'tehama_county_flood_control_and_water_conservation_district_groundwater_sustainability_agency_los_molinos',
                   'tehama_county_flood_control_and_water_conservation_district_groundwater_sustainability_agency_red_bluff',
                   'svbgsa', 'cga',
                   'yucaipa_groundwater_sustainability_agency'))

   gsa_names <- rbind(gsa_names, gsa_names_2, gsa_names_3, gsa_names_4, gsa_names_5)
   return(gsa_names)
}

gsa_names <- clean_gsa_names(gsa_names)

# --- Rule-based tagging function (from Network_Structure_Paper) ---
# Used as fallback for nodes not found in the pre-computed lookup.

tag_nodes_first <- function(nl, gsp_id) {
   gsas <- gsa_gsp %>% filter(GSP_ID == gsp_id)
   gsa_ids <- as.integer(unlist(strsplit(gsas$GSA_IDs, ",")))
   gsa_names2 <- merge(data.frame(GSA_ID = gsa_ids),
                       gsa_names, by = "GSA_ID")$GSA_Name
   gsa_names2 <- c(gsa_names2, "groundwater_sustainability_agency", "gsa")

   nl <- nl %>%
      mutate(CITY = 0, COUNTY = 0, BASIN = 0, NATURAL_FEATURE = 0,
             INFRASTRUCTURE = 0, LOCAL_GSA = 0, OTHER_GSA = 0, OTHER_GSA2 = 0,
             LOCAL_GOV = 0, STATE_GOV = 0, FEDERAL_GOV = 0,
             DISTRICT = 0, DISTRICT2 = 0, GROUP = 0, DATA = 0,
             WATER_PROJECT = 0, REFERENCE = 0, GEO_UNIT = 0,
             LEGAL = 0, TECHNICAL = 0)

   # Deduplicate govsci_dict before joining to avoid many-to-many
   govsci_by_agency <- govsci_dict %>% distinct(govsci_agency, .keep_all = TRUE)
   govsci_by_abbr <- govsci_dict %>% distinct(Abbr, .keep_all = TRUE)

   nl <- nl %>%
      left_join(govsci_by_agency, by = join_by(entity_name == govsci_agency)) %>%
      left_join(govsci_by_abbr, by = join_by(entity_name == Abbr)) %>%
      mutate(govsci_level = coalesce(govsci_level.x, govsci_level.y)) %>%
      mutate(govsci_level = ifelse(is.na(govsci_level), "not_gov", tolower(govsci_level))) %>%
      select(-c("Abbr", "govsci_agency", "govsci_level.x", "govsci_level.y"))

   nl <- nl %>%
      mutate(
         CITY = ifelse(tolower(entity_name) %in% ca_places$NAME | grepl("city_of", tolower(entity_name)), 1, CITY),
         COUNTY = ifelse(tolower(entity_name) %in% ca_counties$NAME | grepl("county$", tolower(entity_name)), 1, COUNTY),
         LOCAL_GSA = ifelse(entity_name %in% gsa_names2, 1, LOCAL_GSA),
         OTHER_GSA = ifelse(entity_name %in% gsa_names$GSA_Name, 1, OTHER_GSA),
         OTHER_GSA2 = ifelse(grepl("(groundwater_sustainability_agency|gsa)$", tolower(entity_name)), 1, OTHER_GSA2),
         LOCAL_GOV = ifelse(govsci_level == "local", 1, LOCAL_GOV),
         STATE_GOV = ifelse(govsci_level == "california" | grepl("california", tolower(entity_name)), 1, STATE_GOV),
         FEDERAL_GOV = ifelse(govsci_level == "federal", 1, FEDERAL_GOV),
         DISTRICT = ifelse(grepl("districts?$", tolower(entity_name)), 1, DISTRICT),
         DISTRICT2 = ifelse(grepl("id|wd|rcd$", tolower(entity_name)), 1, DISTRICT2),
         BASIN = ifelse(grepl("(basins?)$", tolower(entity_name)), 1, BASIN),
         NATURAL_FEATURE = ifelse(grepl("(rivers?|lakes?|creeks?|sloughs?|ecosystems?|formations?|water_systems?|aquifers?|aquifer_systems?|clays?|sands?|silts?|loams?|alluvium|sediments?|natural_communities_commonly_associated_with_groundwater)$", tolower(entity_name)), 1, NATURAL_FEATURE),
         INFRASTRUCTURE = ifelse(grepl("(dams?|reservoirs?|weirs?)$", tolower(entity_name)), 1, INFRASTRUCTURE),
         GROUP = ifelse(grepl("(boards?|committees?|commissions?|community|communities|councils?|departments?|groups?|mou|board_of_directors?|board_of_supervisors?|agreements?|agency|agencies|authority|authorities)$", tolower(entity_name)), 1, GROUP),
         TECHNICAL = ifelse(grepl("((management|information)_systems?|plans?)$", tolower(entity_name)) |
                               grepl("(data|modflow|monitor|report|model|technical)", tolower(entity_name)), 1, TECHNICAL),
         WATER_PROJECT = ifelse(grepl("(projects?)$", tolower(entity_name)), 1, WATER_PROJECT),
         REFERENCE = ifelse(grepl("(^|_)(appendix|exhibit|page|section|table|figure|map)(_|$)", tolower(entity_name)), 1, REFERENCE),
         GEO_UNIT = ifelse(grepl("(regions?|areas?|valleys?|setting|zones?)$", tolower(entity_name)), 1, GEO_UNIT),
         LEGAL = ifelse(grepl("(laws?|regulations?|acts?|bills?|statutes?|codes?|ordinances?|policy|policies?|guidelines?)$", tolower(entity_name)), 1, LEGAL)
      )

   nl <- nl %>%
      mutate(
         entity_type = case_when(
            LOCAL_GSA == 1 ~ "Local_GSA",
            OTHER_GSA == 1 ~ "Other_GSA",
            COUNTY == 1 ~ "County",
            CITY == 1 ~ "City",
            LOCAL_GOV == 1 ~ "Local_Gov",
            STATE_GOV == 1 ~ "State_Gov",
            FEDERAL_GOV == 1 ~ "Federal_Gov",
            OTHER_GSA2 == 1 ~ "Other_GSA",
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
            TECHNICAL == 1 ~ "Technical",
            DISTRICT2 == 1 ~ "District",
            TRUE ~ NA_character_
         )
      ) %>%
      select(-c(CITY, COUNTY, BASIN, NATURAL_FEATURE, INFRASTRUCTURE,
                OTHER_GSA, LOCAL_GOV, STATE_GOV, FEDERAL_GOV,
                GROUP, DATA, WATER_PROJECT, REFERENCE, OTHER_GSA2,
                DISTRICT, DISTRICT2, GEO_UNIT, LEGAL, TECHNICAL,
                LOCAL_GSA, govsci_level))

   return(nl)
}


# --- Unified net_process function ---
# Combines EJ_DAC_Paper's place/DAC tagging with Network_Structure_Paper's entity classification.
# Uses pre-computed LLM tags from all_nodes_raw.csv instead of calling the API.

net_process_tagged <- function(file, gsp_id) {
   temp <- readRDS(file)
   nl <- tibble(temp$nodelist) %>%
      # Apply Network_Structure_Paper name cleaning
      mutate(entity_name = str_remove(entity_name, "_s$"),
             entity_name = str_replace(entity_name, "_s_", "s_")) %>%
      select(-entity_type) %>% # drop the original (empty) entity_type
      # Deduplicate: name cleaning can merge names; sum appearances
      group_by(entity_name) %>%
      summarise(num_appearances = sum(num_appearances), .groups = "drop")

   # --- Step 1: Lookup entity_type from pre-computed per-GSP results ---
   gsp_lookup <- entity_lookup_raw %>%
      filter(gsp_id == !!gsp_id) %>%
      select(name, entity_type, AI_TAGGED) %>%
      distinct(name, .keep_all = TRUE)

   nl <- nl %>%
      left_join(gsp_lookup, by = c("entity_name" = "name"))

   # --- Step 2: For remaining untagged, try the aggregated cross-GSP lookup ---
   untagged_mask <- is.na(nl$entity_type)
   if (any(untagged_mask)) {
      agg_match <- entity_lookup_agg %>%
         distinct(name, .keep_all = TRUE)

      nl <- nl %>%
         left_join(agg_match, by = c("entity_name" = "name"), suffix = c("", "_agg")) %>%
         mutate(
            entity_type = coalesce(entity_type, entity_type_agg),
            AI_TAGGED = coalesce(AI_TAGGED, AI_TAGGED_agg)
         ) %>%
         select(-c(entity_type_agg, AI_TAGGED_agg))
   }

   # --- Step 3: For any still untagged, apply rule-based tagging ---
   still_untagged_names <- nl %>% filter(is.na(entity_type)) %>% pull(entity_name)
   if (length(still_untagged_names) > 0) {
      # Run rule-based tagging on untagged nodes only
      untagged_nl <- nl %>% filter(entity_name %in% still_untagged_names) %>%
         select(entity_name, num_appearances)
      rule_tagged <- tag_nodes_first(untagged_nl, gsp_id) %>%
         select(entity_name, entity_type_rule = entity_type) %>%
         distinct(entity_name, .keep_all = TRUE)

      nl <- nl %>%
         left_join(rule_tagged, by = "entity_name") %>%
         mutate(entity_type = coalesce(entity_type, entity_type_rule)) %>%
         select(-entity_type_rule)
   }

   # Ensure AI_TAGGED column exists and fill NAs with 0
   if (!"AI_TAGGED" %in% colnames(nl)) nl$AI_TAGGED <- 0
   nl <- nl %>% mutate(AI_TAGGED = replace_na(AI_TAGGED, 0))

   # --- Step 4: Merge place/DAC demographics from all_places.csv ---
   places_gsp <- all_places %>% filter(GSP_ID == gsp_id)

   nl <- nl %>%
      left_join(places_gsp, by = join_by(entity_name == NAME20))

   # --- Step 5: Build edgelist ---
   # Apply same name cleaning to edge source/target, then filter to nodelist
   el <- ve_w_sections %>%
      filter(GSP_ID == gsp_id) %>%
      mutate(source = str_remove(source, "_s$"),
             source = str_replace(source, "_s_", "s_"),
             target = str_remove(target, "_s$"),
             target = str_replace(target, "_s_", "s_")) %>%
      filter(source %in% nl$entity_name & target %in% nl$entity_name) %>%
      mutate(weight = 1)

   networklist <- list("nodelist" = tibble(nl), "edgelist" = tibble(el))
   return(networklist)
}


# --- net_graph (same as EJ_DAC_Paper original) ---
# Returns igraph object compatible with step_4 and hypothesis scripts.

net_graph_tagged <- function(networklist, gsp_id) {
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


# --- Output directory ---
out_dir <- paste0(Sys.getenv("BOX_PATH"), "/EJ_Paper/processed_networks_tagged/")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# --- Apply to all networks ---
for (g in seq_along(gsp_ids)) {
   gsp_id <- gsp_ids[g]

   time_process <- system.time({
      gsp_list <- net_process_tagged(
         file = paste0(network_fp, "/", extract_list[g]),
         gsp_id = gsp_id
      )
   })

   print(paste0("Processed ", gsp_id, ": ",
                round(time_process["elapsed"], 2), " seconds"))

   time_graph <- system.time({
      gsp_graph <- net_graph_tagged(gsp_list, gsp_id = gsp_id)
   })

   print(paste0("Graphed ", gsp_id, ": ",
                round(time_graph["elapsed"], 2), " seconds"))

   # Save graph
   saveRDS(object = gsp_graph,
           file = paste0(out_dir, extract_list[g]))
}

# --- Test single network ---
# glt <- net_process_tagged(file = paste0(network_fp, "/", extract_list[102]),
#                           gsp_id = gsp_ids[102])
# ggt <- net_graph_tagged(glt, gsp_id = gsp_ids[102])
# df <- tibble(igraph::as_data_frame(ggt, what = "vertices"))
# table(df$entity_type, useNA = "ifany")
