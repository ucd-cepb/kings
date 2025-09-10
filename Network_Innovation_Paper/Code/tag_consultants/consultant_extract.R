
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
   'winzler_kelly_consulting_engineers', 'wood_rodgers', 'woodard_curran', 'zanjero',
   
   # Law firms
   'abbott_kindermann', 'albietz_law_firm', 'aleshire_wynder', 'allen_matkins_leck_gamble_mallory_natsis',
   'alvarez_glasman_colvin', 'ashurst', 'atkinson_andelson_loya_ruud_romo', 'baker_manock_jensen',
   'bartkiewicz_kronick_shanahan', 'best_best_krieger', 'bingham_mccutchen', 'bold_frederick',
   'bold_polisner_maddow_nelson_judson', 'borton_petrini', 'braun_gosling', 'brewer_patridge_gerlomes_herrick',
   'brown_mia_attorney', 'brownstein_hyatt_farber_schreck', 'brunick_alvarez_battersby', 'buchanan_ingersoll_rooney',
   'caufield_james', 'covington_crowe', 'de_lay_laredo', 'diepenbrock_harrison', 'downey_brand',
   'duane_morris', 'ellison_schneider_harris', 'farella_braun_martel', 'felger_associates',
   'fitzgerald_abbott_beardsley', 'gallery_barton', 'greenberg_glusker', 'griffith_masuda',
   'hanson_bridgett', 'heaslett_david_attorney', 'herum_crabtree_brown', 'holland_david',
   'hoslett_al_warren', 'hunter_ruiz', 'jackson_demarco_peckenpaugh', 'jackson_michael_law_offices',
   'jones_dyer', 'law_office_peter_kiel', 'kronick_moskovitz_tiedemann_girard', 'lagerlof',
   'latham_watkins', 'lennihan_law', 'luce_forward_hamilton_scripps', 'mannon_king_johnson',
   'mason_robbins_gnass_browning', 'matthew_emrick_law_offices', 'mccormick_kidman_behrens', 'mccutcheon_doyle_brown_enersen',
   'mcdonough_holland_allen', 'mcmurtrey_hartsock_worth', 'mennemeier_glassman_stroud', 'meyers_nave',
   'michael_nordstrom_attorney', 'milbank_tweed_hadley_mccoy', 'miliband_water_law', 'minasian_meith_soares_sexton_cooper',
   'mooney_donald_law_office', 'morrison_foerster', 'neumiller_beardslee', 'nossaman_guthner_knox_elliott',
   'paris_kincaid_wasiewski', 'omelveny_myers', 'parks_solar', 'peltzer_richardson',
   'price_postell_parma', 'richards_watson_gershon', 'rutan_tucker', 'sawyers_gary_holland',
   'scheuring_chris_attorney', 'schroeder_law_offices', 'soluri_meserve', 'somach_simmons_dunn',
   'spears_scott_law_offices', 'stoel_rives', 'arnold_law_practice', 'tom_hicks_attorney',
   'water_power_law_group', 'weintraub_genshlea_sproul', 'weston_benhoof_rochefort_rubalcava_maccuish', 'young_wooldridge',
   'zanjero_advise_manage_solve', 'zimmer_melton',
   
   # Additional environmental/engineering firms
   'clean_harbors', 'fluor_corp', 'bechtel_corp', 'salini_impregilo', 'stantec', 'suez_north_america',
   'wood_plc', 'black_veatch', 'garney_holding', 'ghd', 'golder_associates', 'kiewit_corp',
   'mwh_constructors', 'leidos', 'snc_lavalin', 'gilbane_building', 'tradebe_environmental_services',
   'walsh_group', 'barnard_construction', 'aegion_corp', 'parsons_corp', 'antea_group',
   'ecc', 'nrc_group', 'weeks_marine', 'burns_mcdonnell', 'carollo_engineers', 'wharton_smith',
   'clean_earth', 'ulliman_schutte_construction', 'mccarthy_holdings', 'north_wind_group', 'bauer_resources',
   'geosyntec_consultants', 'hazen_sawyer', 'pc_construction', 'bowen_engineering_corp', 'alberici_flintco',
   'kokosing_group', 'lyles_construction_group', 'pcl_construction', 'cascade_environmental', 'sevenson_environmental',
   'weston_solutions', 'worleyparsons', 'terracon_consultants', 'cardno_ltd', 'atc_group_services',
   'apex_cos', 'pal_environmental_safety_corp', 'reynolds_construction', 'sukut_construction', 'entact',
   'c_overaa', 'trinity_consultants', 'american_integrated_services', 'hepaco', 'ea_engineering_science_technology',
   'crowder_constructors', 'allan_myers', 'rice_lake_construction_group', 'navarro_research_engineering',
   'environmental_restoration', 'battelle', 'asrc_industrial_services', 'swca_environmental_consultants',
   'civil_environmental_consultants', 'artelia', 'nv5_global', 'crossland_heavy_contractors', 'freese_nichols',
   'anchor_qea', 'max_foote_construction', 'ecology_environment', 'patriot_environmental_services',
   'groundwater_environmental_services', 'hgl', 'partner_engineering_science'
)

# Keyword-based search terms
consulting_keywords <- c('consulting', 'consultants', 'associates', 'engineering')

# Company indicators
company_indicators <- c('_inc', '_llc', '_corp')


# ==============================================================================
# FUNCTIONS
# ==============================================================================

#' Check if an entity is a consulting firm using enhanced criteria
#' @param entity Character string to check
#' @return Boolean indicating if entity is likely a consulting firm
is_consulting_firm <- function(entity) {
   if (is.na(entity) || !is.character(entity) || nchar(trimws(entity)) == 0) {
      return(FALSE)
   }
   
   entity_lower <- tolower(trimws(entity))
   
   # Check against major firm names from external research (with word boundaries)
   is_major_firm <- any(sapply(major_consulting_firms, function(pattern) {
      grepl(paste0("\\b", pattern, "\\b"), entity_lower, perl = TRUE)
   }))
   
   # Check for consulting keywords (with word boundaries)
   has_consulting_keyword <- any(sapply(consulting_keywords, function(keyword) {
      grepl(paste0("\\b", keyword, "\\b"), entity_lower, perl = TRUE)
   }))
   
   # Check for company indicators (but exclude water companies)
   has_company_indicator <- any(sapply(company_indicators, function(indicator) {
      grepl(indicator, entity_lower, fixed = TRUE)
   })) && !grepl('water_company', entity_lower, fixed = TRUE)
   
   # Exclude Army Corps of Engineers
   is_army_corps <- grepl('army.*corps|corps.*engineers', entity_lower, perl = TRUE)
   
   # Return TRUE if matches any criteria and is not Army Corps
   return((is_major_firm || has_consulting_keyword || has_company_indicator) && !is_army_corps)
}

#' Create disambiguation mapping for consulting firms
#' @param consulting_firms Vector of consulting firm names
#' @return Data frame with original and disambiguated firm names
create_disambiguation_mapping <- function(consulting_firms) {
   
   # Define disambiguation rules
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
         'gei_consultants_inc'
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
      
      # Kenneth D. Schmidt variations
      'kenneth_d_schmidt_associates' = c(
         'kenneth_d_schmidt_and_associates',
         'kenneth_d_schmidt_associates',
         'kenneth_d_schmidt_associates_groundwater_quality_consultants'
      ),
      
      # Rincon Consultants variations
      'rincon_consultants_inc' = c(
         'rincon_consultants',
         'rincon_consultants_inc'
      ),
      
      # Hopkins Groundwater variations
      'hopkins_groundwater_consultants' = c(
         'hopkins_groundwater_consultants',
         'p_hopkins_groundwater_consultants_inc'
      ),
      
      # Richard C. Slade variations
      'richard_c_slade_associates_llc' = c(
         'richard_c_slade_associates',
         'richard_c_slade_associates_llc'
      ),
      
      # Bachand variations
      'bachand_and_associates' = c(
         'bachand_and_associates',
         'bachand_associates'
      ),
      
      # Bondy Groundwater variations
      'bondy_groundwater_consulting_inc' = c(
         'bondy_groundwater_consulting',
         'bondy_groundwater_consulting_inc'
      ),
      
      # Aquatic Bioassay variations
      'aquatic_bioassay_and_consulting_laboratory' = c(
         'aquatic_bioassay_and_consulting',
         'aquatic_bioassay_and_consulting_laboratory'
      ),
      
      # Moore Twining variations
      'moore_twining_associates_environmental_division' = c(
         'moore_twining_associates_environmental_division',
         'moore_twining_associates_environmental_division_project_water_conservation_plant'
      ),
      
      # Provost Pritchard variations
      'provost_pritchard_consulting' = c(
         'provost_pritchard_consulting',
         'provost_pritchard_consulting_meetings'
      ),
      
      # R.L. Schafer variations
      'r_l_schafer_associates' = c(
         'r_l_schafer_associates',
         'reporting_limit_schafer_associates'
      ),
      
      # Major firms from external research
      'aecom' = c('aecom', 'aecom_technical_services', 'aecom_technical_services_inc'),
      'jacobs' = c('jacobs', 'jacobs_engineering', 'jacobs_engineering_group'),
      'hdr' = c('hdr', 'hdr_engineering', 'hdr_engineering_inc'),
      'tetra_tech' = c('tetra_tech', 'tetra_tech_inc'),
      'woodward_curran' = c('woodward_curran', 'woodward_and_curran', 'woodward', 'curran'),
      'swca_environmental_consultants' = c('swca', 'swca_environmental_consultants'),
      'brown_caldwell' = c('brown_caldwell', 'brown_and_caldwell'),
      'cdm_smith' = c('cdm_smith', 'cdm_smith_inc'),
      'arcadis' = c('arcadis', 'arcadis_us', 'arcadis_us_inc'),
      'ch2m' = c('ch2m', 'ch2m_hill', 'ch2m_hill_engineers'),
      'trc_companies' = c('trc', 'trc_companies', 'trc_environmental_corporation')
   )
   
   # Create mapping dataframe
   mapping_df <- data.frame(
      original_entry = character(),
      disambiguated_firm = character(),
      stringsAsFactors = FALSE
   )
   
   # Apply disambiguation rules
   for (disambiguated_name in names(disambiguation_rules)) {
      variations <- disambiguation_rules[[disambiguated_name]]
      for (variation in variations) {
         if (variation %in% consulting_firms) {
            mapping_df <- rbind(mapping_df, data.frame(
               original_entry = variation,
               disambiguated_firm = disambiguated_name,
               stringsAsFactors = FALSE
            ))
         }
      }
   }
   
   # Add remaining firms that don't have variations
   remaining_firms <- setdiff(consulting_firms, mapping_df$original_entry)
   for (firm in remaining_firms) {
      mapping_df <- rbind(mapping_df, data.frame(
         original_entry = firm,
         disambiguated_firm = firm,
         stringsAsFactors = FALSE
      ))
   }
   
   return(mapping_df)
}

#' Categorize search method used to find each firm
#' @param firm_name Character string of firm name
#' @return Character string indicating search method
categorize_search_method <- function(firm_name) {
   firm_lower <- tolower(firm_name)
   
   # Check if it's a major firm from external research
   is_major_firm <- any(sapply(major_consulting_firms, function(pattern) {
      grepl(pattern, firm_lower, fixed = TRUE)
   }))
   
   if (is_major_firm) {
      return('external_research')
   } else if (grepl('consulting|consultants|associates', firm_lower)) {
      return('keyword_search')
   } else if (grepl('_inc|_llc|_corp', firm_lower)) {
      return('company_indicator')
   } else {
      return('other')
   }
}


bad <- c('0053','0089')
gs_edge <- fread('Network_Innovation_Paper/data_products/all_gsa_edges.csv')
gs_edge$gsp_id <- formatC(gs_edge$gsp_id,width = 4,flag = '0')
gs_edge <- gs_edge[!gsp_id %in% bad,]
gs_melt <- melt(gs_edge,id.vars = c('gsp_id','gsa'))

# Extract entities from the 'variable' column (entity names)
entities <- unique(gs_melt$variable)
entities <- entities[!is.na(entities)]
entities <- trimws(as.character(entities))
entities <- entities[nchar(entities) > 0]

# Find consulting firms using enhanced methodology
consulting_firm_flags <- sapply(entities, is_consulting_firm)
consulting_firms <- entities[consulting_firm_flags]
consulting_firms

cat(sprintf("Found %d consulting firm entries\n", length(consulting_firms)))

# Get unique consulting firms
unique_consulting_firms <- unique(consulting_firms)
cat(sprintf("Unique consulting firms: %d\n", length(unique_consulting_firms)))

# Filter gs_melt to get all GSP-consulting firm pairings
consulting_matches <- gs_melt[gs_melt$variable %in% consulting_firms, ]

# Create disambiguation mapping
cat("Creating disambiguation mapping...\n")
mapping_df <- create_disambiguation_mapping(unique_consulting_firms)

# Add search method categorization
mapping_df$search_method <- sapply(mapping_df$original_entry, categorize_search_method)

# Count occurrences of each original entry in the dataset
mapping_df$occurrence_count <- sapply(mapping_df$original_entry, function(firm) {
   sum(consulting_firms == firm, na.rm = TRUE)
})

# Create summary by disambiguated firm
summary_df <- mapping_df %>%
   group_by(disambiguated_firm) %>%
   summarise(
      total_occurrences = sum(occurrence_count),
      num_variations = n(),
      search_methods = paste(unique(search_method), collapse = ', '),
      variations = paste(original_entry, collapse = '; '),
      .groups = 'drop'
   ) %>%
   arrange(desc(total_occurrences))

# ==============================================================================
# RESULTS OUTPUT
# ==============================================================================

cat("\n", paste(rep("=", 80), collapse=""), "\n")
cat("ENHANCED CONSULTING FIRMS ANALYSIS RESULTS\n")
cat(paste(rep("=", 80), collapse=""), "\n\n")

cat("SUMMARY STATISTICS:\n")
cat(sprintf("- Total unique consulting firms (after disambiguation): %d\n", nrow(summary_df)))
cat(sprintf("- Total consulting firm entries in dataset: %d\n", sum(mapping_df$occurrence_count)))
cat(sprintf("- Firms found via keyword search: %d\n", 
            sum(mapping_df$search_method == 'keyword_search')))
cat(sprintf("- Firms found via external research patterns: %d\n", 
            sum(mapping_df$search_method == 'external_research')))
cat(sprintf("- Firms found via company indicators: %d\n", 
            sum(mapping_df$search_method == 'company_indicator')))

cat("\nTOP 10 CONSULTING FIRMS BY OCCURRENCE:\n")
print(head(summary_df, 10))

cat("\nFIRMS FOUND VIA EXTERNAL RESEARCH (Major Environmental Consulting Firms):\n")
external_research_firms <- mapping_df %>% 
   filter(search_method == 'external_research') %>%
   select(original_entry, disambiguated_firm, occurrence_count) %>%
   arrange(desc(occurrence_count))

if (nrow(external_research_firms) > 0) {
   print(external_research_firms)
} else {
   cat("No major consulting firms found using external research patterns.\n")
   cat("Consider manually searching for these patterns in your dataset:\n")
   cat(paste(major_consulting_firms, collapse = ', '))
}

# ==============================================================================
# EXPORT RESULTS
# ==============================================================================

# Save results to Network Innovation Paper directory
output_dir <- 'Network_Innovation_Paper/data_products/'
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Save complete mapping table
write_csv(mapping_df, file.path(output_dir, 'consulting_firms_mapping.csv'))
cat(sprintf("\nComplete mapping saved to: %s\n", file.path(output_dir, 'consulting_firms_mapping.csv')))

# Save summary table
write_csv(summary_df, file.path(output_dir, 'consulting_firms_summary.csv'))
cat(sprintf("Summary saved to: %s\n", file.path(output_dir, 'consulting_firms_summary.csv')))

# Save list of firms to manually search for
major_firms_to_search <- data.frame(
   search_pattern = major_consulting_firms,
   found_in_dataset = sapply(major_consulting_firms, function(pattern) {
      any(grepl(pattern, tolower(mapping_df$original_entry), fixed = TRUE))
   })
)

write_csv(major_firms_to_search, file.path(output_dir, 'major_firms_search_status.csv'))
cat(sprintf("Major firms search status saved to: %s\n", file.path(output_dir, 'major_firms_search_status.csv')))

# Save GSP-entity associations for consulting firms
consulting_gsp_associations <- gs_melt[gs_melt$variable %in% consulting_firms, ]
write_csv(consulting_gsp_associations, file.path(output_dir, 'consulting_gsp_associations.csv'))
cat(sprintf("GSP-consulting firm associations saved to: %s\n", file.path(output_dir, 'consulting_gsp_associations.csv')))

cat("\nAnalysis complete! Check the output files for detailed results.\n")

# ==============================================================================
# CREATE FINAL GSP-CONSULTING FIRM PAIRINGS
# ==============================================================================

# Create disambiguation function for individual firm names
disambiguate_firm <- function(firm_name) {
   # Check disambiguation rules from the mapping_df
   match_row <- mapping_df[mapping_df$original_entry == firm_name, ]
   if (nrow(match_row) > 0) {
      return(match_row$disambiguated_firm[1])
   } else {
      return(firm_name)  # Return original if no disambiguation rule
   }
}

# Add disambiguated firm names to consulting matches
consulting_matches$disambiguated_firm <- sapply(consulting_matches$variable, disambiguate_firm)

# Create final result: GSP-consulting firm pairings
gsp_consulting_pairs <- consulting_matches %>%
   select(gsp_id, disambiguated_firm) %>%
   distinct() %>%
   arrange(gsp_id, disambiguated_firm)

# Save the final pairings
write_csv(gsp_consulting_pairs, 'Network_Innovation_Paper/data_products/gsp_consulting_firm_post_textnet_pairs.csv')
