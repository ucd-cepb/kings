# Simplified Consulting Firms Analysis Script
# Identifies and disambiguates consulting firms in GSA edges data

library(data.table)
library(dplyr)
library(stringr)

# Define consulting firms and law firms dictionary
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
consulting_keywords <- c('consulting', 'consultants', 'associates', 'engineering', 'environmental')

# Company indicators
company_indicators <- c('_inc', '_llc', '_corp')

# Exclusion patterns
exclusion_patterns <- c(
   'committee', 'board', 'agency', 'department', 'district', 'authority',
   'workgroup', 'working_group', 'advisory', 'exchange_contractors',
   'groundwater_sustainability', 'partnership', 'agreement', 'coordination',
   'stakeholder', 'sustainability_plan', 'water_company', 'mutual_water_company',
   'community_services_district', 'irrigation_', '_ranch', 'ditch_company',
   'corps_of_engineers', 'meetings', 'standards', '_limit_'
)

# Function to check if entity is consulting firm
is_consulting_firm <- function(entity) {
   if (is.na(entity) || !is.character(entity) || nchar(trimws(entity)) == 0) {
      return(FALSE)
   }
   
   entity_lower <- tolower(trimws(entity))
   
   # Check against major firm names
   is_major_firm <- any(sapply(major_consulting_firms, function(pattern) {
      grepl(pattern, entity_lower, fixed = TRUE)
   }))
   
   # Check for consulting keywords
   has_consulting_keyword <- any(sapply(consulting_keywords, function(keyword) {
      grepl(keyword, entity_lower, fixed = TRUE)
   }))
   
   # Check for company indicators (but exclude water companies)
   has_company_indicator <- any(sapply(company_indicators, function(indicator) {
      grepl(indicator, entity_lower, fixed = TRUE)
   })) && !grepl('water_company', entity_lower, fixed = TRUE)
   
   # Check exclusions
   is_excluded <- any(sapply(exclusion_patterns, function(pattern) {
      grepl(pattern, entity_lower, fixed = TRUE)
   }))
   
   return((is_major_firm || has_consulting_keyword || has_company_indicator) && !is_excluded)
}

# Disambiguation mapping
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
      if (any(sapply(variations, function(var) grepl(var, firm_lower, fixed = TRUE)))) {
         return(disambiguated_name)
      }
   }
   
   # Return original if no disambiguation rule matches
   return(firm_lower)
}

# Main analysis
bad <- c('0053','0089')
gs_edge <- fread('Network_Innovation_Paper/data_products/all_gsa_edges.csv')
gs_edge$gsp_id <- formatC(gs_edge$gsp_id, width = 4, flag = '0')
gs_edge <- gs_edge[!gsp_id %in% bad,]
gs_melt <- melt(gs_edge, id.vars = c('gsp_id','gsa'))

# Filter for consulting firms only
consulting_matches <- gs_melt[sapply(gs_melt$variable, is_consulting_firm), ]

# Add disambiguated firm names
consulting_matches$disambiguated_firm <- sapply(consulting_matches$variable, disambiguate_firm)

# Create final result: GSP-consulting firm pairings
result <- consulting_matches %>%
   select(gsp_id, disambiguated_firm) %>%
   distinct() %>%
   arrange(gsp_id, disambiguated_firm)

# Return the result
result