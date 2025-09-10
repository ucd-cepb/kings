# Raw Text Consulting Firms Analysis Script
# Extracts consulting firm names from raw GSP document text

library(dplyr)
library(stringr)
library(readr)
library(tidyr)
library(data.table)

major_consulting_firms <- c(
   # Original major consulting firms
   'aecom', 'jacobs', 'wsp', 'tetra_tech', 'hdr', 'arcadis', 'ch2m', 'icf', 'erm', 'trc', 
   'woodward', 'curran', 'swca', 'brown_caldwell', 'cdm_smith', 'gza', 'scs_engineers', 
   'kleinfelder', 'terracon', 'haley_aldrich', 'langan', 'weston', 'sampson', 'barr_engineering',
   'partner_esi', 'cardno', 'amec', 'foster_wheeler', 'ramboll', 'arup', 'mott_macdonald',
   'golder', 'wood_environment', 'rps', 'ecology_environment', 'michael_baker',
   # California Water Boards consulting firms
   '4_creeks', 'six_33_solutions', 'a_a_rich', 'aecom', 'albion_environmental', 
   'analytical_environmental_services', 'asm_affiliates', 'atlas_engineering_services',
   'balance_hydrologics', 'bargas_environmental_consulting', 'bbj_group', 'bennett_engineering_services',
   'blankinship_associates', 'borton_petrini', 'brown_caldwell', 'brown_norman',
   'bsk_associates_engineers_laboratories', 'capta_hydro', 'california_hydrologic', 'cardona',
   'ch2m_hill', 'chico_environmental_science_planning', 'daniel_b_stephens_associates',
   'dihydrogen_oxide', 'dudek', 'ebi_consulting', 'ecorp_consulting', 'eki_environment_water',
   'energy_fave', 'environmental_science_associates', 'emc_planning_group', 'foothill_associates',
   'fugro_consultants', 'gei_consultant', 'geissler_associates', 'geo_blue_consulting',
   'gregory_engineering', 'hdr_engineering', 'horizon_water_environmental', 'icf_international',
   'integrated_resource_management', 'johnson_nicholas', 'jrp_historical_consulting',
   'kjeldsen_biological_consulting', 'kjeldsen_sinnock_neudeck', 'krieger_stewart',
   'laura_hall_consulting', 'mark_thomas', 'mbk_engineers', 'mead_hunt', 'michael_baker_international',
   'napa_valley_vineyard_engineering', 'normandeau_associates', 'north_coast_resource_management',
   'oconnor_environmental', 'pacific_water_management', 'padre_associates', 'provost_pritchard_consulting',
   'rights_to_water_engineering', 'rincon_consultants', 'rutan_tucker', 'shaw_john',
   'shn_consulting_engineers_geologists', 'spector_consulting_service', 'stetson_engineers',
   'steven_j_herzog', 'stevens_consulting', 'strange_aquatic_resources', 'stuntzner_engineering_forestry',
   'swale', 'tall_tree_engineering', 'tec_civil_engineering_consultants', 'terraphase_engineering',
   'tully_young', 'upland_hydrogeology_consulting', 'urbana_preservation_planning', 'urs',
   'wagner_bonsignore_consulting_civil_engineers', 'weiss_associates', 'west_yost_associates',
   'westwater_research', 'whf_environmental_engineering_group', 'wilson_water_resources',
   'winzler_kelly_consulting_engineers', 'wood_rodgers', 'woodard_curran', 'zanjero'
)

# Keyword-based search terms
consulting_keywords <- c('consulting', 'consultants', 'associates', 'engineering')

# Company indicators
company_indicators <- c('inc', 'llc', 'corp', 'ltd', 'limited', 'corporation')

# ==============================================================================
# FUNCTIONS
# ==============================================================================

#' Extract consulting firm mentions from text
#' @param text Character string to search
#' @return Vector of consulting firm names found
extract_consulting_firms <- function(text) {
   if (is.na(text) || !is.character(text) || nchar(trimws(text)) == 0) {
      return(character(0))
   }
   
   text_lower <- tolower(text)
   found_firms <- character(0)
   
   # 1. Search for known major consulting firms (with word boundaries)
   for (pattern in major_consulting_firms) {
      # Convert underscores to spaces for natural text matching
      pattern_spaced <- gsub("_", " ", pattern)
      pattern_regex <- paste0("\\b", pattern_spaced, "\\b")
      
      matches <- str_extract_all(text_lower, pattern_regex)[[1]]
      if (length(matches) > 0) {
         # Convert back to underscore format for standardization
         standardized_matches <- gsub(" ", "_", matches)
         found_firms <- c(found_firms, standardized_matches)
      }
   }
   
   # 2. Search for consulting keywords with context
   for (keyword in consulting_keywords) {
      # Pattern: word(s) + keyword, capturing 1-3 words before
      pattern <- paste0("\\b([a-z]+(?:\\s+[a-z]+){0,2})\\s+", keyword, "\\b")
      matches <- str_extract_all(text_lower, pattern)[[1]]
      
      if (length(matches) > 0) {
         # Clean and standardize the matches
         clean_matches <- trimws(matches)
         clean_matches <- gsub("\\s+", "_", clean_matches)
         
         # Filter out common false positives
         clean_matches <- clean_matches[!grepl("^(the|and|or|of|for|in|with|by|to|from)_", clean_matches)]
         clean_matches <- clean_matches[!grepl("army.*corps|corps.*engineers", clean_matches)]
         
         found_firms <- c(found_firms, clean_matches)
      }
   }
   
   # 3. Search for company indicators with context
   for (indicator in company_indicators) {
      # Pattern: word(s) + Inc./LLC/Corp, capturing 1-4 words before
      pattern <- paste0("\\b([a-z]+(?:\\s+[a-z]+){0,3})\\s+", indicator, "\\b")
      matches <- str_extract_all(text_lower, pattern)[[1]]
      
      if (length(matches) > 0) {
         # Clean and standardize
         clean_matches <- trimws(matches)
         clean_matches <- gsub("\\s+", "_", clean_matches)
         
         # Filter out water companies and other obvious non-consultants
         clean_matches <- clean_matches[!grepl("water_company|water_district|irrigation_district", clean_matches)]
         clean_matches <- clean_matches[!grepl("army.*corps|corps.*engineers", clean_matches)]
         
         found_firms <- c(found_firms, clean_matches)
      }
   }
   
   # Remove duplicates and return
   unique(found_firms)
}

# Disambiguation mapping (same as original script)
disambiguation_rules <- list(
   # Luhdorff and Scalmanini variations
   'luhdorff_and_scalmanini_consulting_engineers' = c(
      'luhdorff_and_scalmanini_consulting_engineers',
      'luhdorff_and_scalmanini_consulting_engineers_team',
      'luhdorff_scalmanini_consulting_engineers',
      'scalmanini_consulting_engineers',
      'davids_engineering_luhdorff_and_scalmanini_consulting_engineers'
   ),
   
   # GEI Consultants variations
   'gei_consultants_inc' = c(
      'gei_consultants',
      'gei_consultants_et',
      'gei_consultants_inc',
      'gei_consultant'
   ),
   
   # Geosyntec variations
   'geosyntec_consultants_inc' = c(
      'geosyntec_consultants',
      'geosyntec_consultants_inc'
   ),
   
   # Larry Walker Associates variations
   'larry_walker_associates_inc' = c(
      'larry_walker_associates',
      'larry_walker_associates_inc',
      'larry_walker_associates_team'
   ),
   
   # Provost Pritchard variations
   'provost_pritchard_consulting' = c(
      'provost_pritchard_consulting',
      'provost_pritchard_consulting_meetings',
      'provost_pritchard_consulting_group'
   ),
   
   # Major firms variations
   'aecom' = c('aecom', 'aecom_technical_services', 'aecom_technical_services_inc'),
   'jacobs' = c('jacobs', 'jacobs_engineering', 'jacobs_engineering_group'),
   'hdr' = c('hdr', 'hdr_engineering', 'hdr_engineering_inc'),
   'tetra_tech' = c('tetra_tech', 'tetra_tech_inc'),
   'woodard_curran' = c('woodard_curran', 'woodward_and_curran', 'woodward', 'curran'),
   'swca_environmental_consultants' = c('swca', 'swca_environmental_consultants'),
   'brown_caldwell' = c('brown_caldwell', 'brown_and_caldwell'),
   'cdm_smith' = c('cdm_smith', 'cdm_smith_inc'),
   'ch2m' = c('ch2m', 'ch2m_hill', 'ch2m_hill_engineers')
)

# Function to disambiguate firm names
disambiguate_firm <- function(firm_name) {
   firm_lower <- tolower(trimws(firm_name))
   
   # Check disambiguation rules
   for (disambiguated_name in names(disambiguation_rules)) {
      variations <- disambiguation_rules[[disambiguated_name]]
      if (any(sapply(variations, function(var) grepl(paste0("\\b", var, "\\b"), firm_lower, perl = TRUE)))) {
         return(disambiguated_name)
      }
   }
   
   # Return original if no disambiguation rule matches
   return(firm_lower)
}

# ==============================================================================
# MAIN ANALYSIS
# ==============================================================================

# Read GSP documents with metadata
bad <- c('0053','0089')
meta <- readRDS('Multipurpose_Files/gsp_docs_w_meta')
meta <- meta[!meta$gsp_id %in% bad, ]

cat(sprintf("Loaded GSP documents: %d rows\n", nrow(meta)))

# Extract consulting firms from text column
cat("Extracting consulting firms from text...\n")
all_extractions <- list()

# Process each text page
for (i in 1:nrow(meta)) {
   if (i %% 1000 == 0) cat(sprintf("Processing row %d/%d\n", i, nrow(meta)))
   
   text_content <- meta$text[i]
   gsp_id <- meta$gsp_id[i]
   
   if (!is.na(text_content) && nchar(text_content) > 0) {
      extracted_firms <- extract_consulting_firms(text_content)
      
      if (length(extracted_firms) > 0) {
         all_extractions[[length(all_extractions) + 1]] <- data.frame(
            gsp_id = gsp_id,
            page_num = meta$page_num[i],
            raw_firm = extracted_firms,
            stringsAsFactors = FALSE
         )
      }
   }
}

# Combine all extractions
if (length(all_extractions) > 0) {
   consulting_extractions <- do.call(rbind, all_extractions)
} else {
   consulting_extractions <- data.frame(gsp_id = character(0), page_num = numeric(0), 
                                       raw_firm = character(0), stringsAsFactors = FALSE)
}

cat(sprintf("Found %d consulting firm mentions across all documents\n", nrow(consulting_extractions)))

# Apply disambiguation
consulting_extractions$disambiguated_firm <- sapply(consulting_extractions$raw_firm, disambiguate_firm)

# Create final GSP-consulting firm pairings with page numbers
gsp_consulting_pairs <- consulting_extractions %>%
   select(gsp_id, page_num, raw_firm, disambiguated_firm) %>%
   arrange(gsp_id, page_num, disambiguated_firm)


# ==============================================================================
# EXPORT RESULTS
# ==============================================================================

output_dir <- 'Network_Innovation_Paper/data_products/'
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Save results with page numbers
write_csv(gsp_consulting_pairs, file.path(output_dir, 'gsp_consulting_firm_raw_text_pairs.csv'))
cat(sprintf("GSP-consulting firm pairings with page numbers saved to: %s\n", file.path(output_dir, 'gsp_consulting_firm_raw_text_pairs.csv')))

cat(sprintf("\nAnalysis complete! Found %d consulting firm mentions from raw text.\n", nrow(gsp_consulting_pairs)))
cat(sprintf("Total unique consulting firms identified: %d\n", length(unique(gsp_consulting_pairs$disambiguated_firm))))