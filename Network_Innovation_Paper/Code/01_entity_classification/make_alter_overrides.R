#' make_alter_overrides.R — ONE-OFF curation tool, NOT part of run_all.R.
#'
#' Regenerates inputs/entity_bag_overrides_{ngo,research}.csv from the family specs
#' below. These are the folds that no deterministic tier in build_alter_bag_map.R can
#' reach: acronym expansions the uniqueness guard refuses, OCR and glossary-corruption
#' spellings, organization rename chains, and the campus/chapter POLICY decisions
#' recorded in BAG_REVIEW_FINDINGS.md §10d.
#'
#' It reads the CURRENT alter_bag_map.csv to resolve targets, so it is only meaningful
#' against a built map. Re-running it OVERWRITES both override files, so hand edits to
#' them must be folded back into the specs here or they will be lost. The equivalent
#' consultant list came from the audit rather than a spec and lives only in the
#' committed override file — do not expect this script to reproduce it.
#'
#'   Rscript Network_Innovation_Paper/Code/01_entity_classification/make_alter_overrides.R
suppressMessages(library(data.table))
m <- fread("Network_Innovation_Paper/data_products/01_entity_classification/alter_bag_map.csv", colClasses="character")

# family := list(target_bag_id, include regex, exclude regex)
# Policy (BAG_REVIEW_FINDINGS.md §7d): the bag is the TIE-BEARING actor. A hosted
# centre/lab/programme folds into its host campus; UC Cooperative Extension / ANR is
# a separate statewide actor; bare "University of California" is the system, never a
# campus. Named local chapters of a national body keep their own bags.
F <- list(
 NGO = list(
  list("ngo_nature_conservancy", "nature_conserv|nature_conservatory|^tncs$",
       "temescal|water_district|commission|audubon|save_open_space"),
  list("ngo_leadership_counsel_of_justice_accountability",
       "leadership_coun|counsel_for_justice|counsel_of_justice|council_for_account|council_for_justice|^lcja$|escobedo",
       "self_help"),
  list("ngo_self_help_enterprise", "self_?help|selflelp", "justice_and_accountability"),
  list("ngo_national_audubon_society",
       "^audubon$|^audobon$|^california_audubon$|^natl_audubon_socy$|national_audubon_society",
       "plumas|redbud|sacramento_audubon|san_joaquin|stanislaus|kern_audobon|superior_court"),
  list("ngo_union_of_concerned_scientist", "union_of_concerned|concerned_scien", NA),
  list("ngo_freshwater_trust", "freshwater_trust", "local_government_commission|_commission$"),
  list("ngo_california_trout", "california_trout|^cal_trout$", NA),
  list("ngo_center_on_race_poverty_and_the_environment", "race_poverty|^crpe$", NA),
  list("ngo_central_coast_alliance_united_for_sustainable_economy",
       "alliance_united|^cause$", NA),
  list("ngo_rural_community_assistance_corporation",
       "^rcac$|rural_communities_assistance_corpora|rural_community_assistance_corpora", "partnership"),
  list("ngo_california_waterfowl_association",
       "california_waterfowl", "clean_water_action|umbrella|kern_groundwater|ducks"),
  list("ngo_environmental_law_foundation", "environmental_law_foundation|environniental_law", "^audubon_and"),
  list("ngo_putah_creek_council", "^p.tah_creek_council$|^pulah_creek_council$", NA),
  list("ngo_santa_barbara_channelkeeper", "channelkeeper|cbannelkeeper|channel_keep", NA),
  list("ngo_environmental_defense_fund", "environment.{0,2}_defense_fund", NA),
  list("ngo_league_of_women_voters", "^league_of_woman_voters$", NA),
  list("ngo_glenn_county_farm_bureau", "^glenn_(county_)?farm_bureau$", NA),
  list("ngo_madera_county_farm_bureau", "^madera_(county_)?farm_bureau$", NA),
  list("ngo_merced_county_farm_bureau", "^merced_(county_)?farm_bureau$", NA),
  list("ngo_solano_county_farm_bureau", "^solano_(county_)?farm_bureau$", NA),
  list("ngo_ventura_county_farm_bureau", "^ventura_(county_)?farm_bureau$", NA),
  list("ngo_tulare_county_farm_bureau", "tulare_county_farm_bureau", NA)
 ),
 Research = list(
  list("res_university_of_california_davis",
       "califor.{0,3}a_davis|^ucd$|^ucdavis$|u_c_davis|davis_center_for_watershed|center_for_watershed|watershed_scien|davis_tahoe|^tahoe_research_group$|davis_stable", "western|cooperative_extension|coop_extension"),
  list("res_university_of_california",
       "^uc$|^universi.y_of_california$|^university_of_cali_fomia$|^california_university_of_california$|^university_of_22_california$|regents_of_the_university_of_calif|^university_of_cali.{0,4}a$", NA),
  list("res_university_of_california_cooperative_extension",
       "cooperative_exten|coop_extension|^ucanr$|university_of_california_division_of|university_of_california_extension|cal_fornia_cooperative",
       "iowa|texas"),
  list("res_university_of_california_berkeley", "university_of_california_(at_)?berkeley$|essig", NA),
  list("res_california_state_university_monterey_bay", "^csumb$|cal_state_university_monterey", NA),
  list("res_california_polytechnic_state_university",
       "^calpoly$|^cal_poly$|^california_polytechnic_state$|^california_polytechnic_university$", NA),
  list("res_california_state_polytechnic_university_of_pomona",
       "^cal_poly_pomona$|^california_state_polytechnic_university$|pomona", NA),
  list("res_irrigation_training_research_center",
       "^itrc$|irrigation_train|irrigation_training|^training_and_research_center$|irrigation_traning", "technology"),
  list("res_lawrence_berkeley_national_laboratory",
       "lawrence_berkeley|lawarence_berkley|lawrence_berkely|berkeley_nation", "livermore"),
  list("res_lawrence_livermore_national_laboratory", "livermore", "berkeley"),
  list("res_jet_propulsion_laboratory", "jet_propulsion|propulsion_laboratory", NA),
  list("res_unavco", "^unavco$|^unavsco$|^uanvco$|navstar|navigation_satellite", NA),
  list("res_desert_research_institute", "desert_research", "naval"),
  list("res_prism_climate_group", "^prism_climate_group$|independent_slopes", NA),
  list("res_scripps_institution_of_oceanography", "scripps_institut", NA),
  list("res_scripps_orbit_and_permanent_array_center", "scripps_orbit|scripps_orbital", NA),
  list("res_intergovernmental_panel_on_climate_change", "^ipcc$|.{0,5}governmental_panel_on_climate|international_panel_on_climate", NA),
  list("res_western_regional_climate_center", "^wrcc$|western_regional_climate|western_region_climate", NA),
  list("res_illinois_state_water_survey", "^isws$|illinois_state_water", NA),
  list("res_fresno_state", "^fresno_st$|^fresno_state$", NA)
 ))

PFX <- c(NGO = "ngo", Research = "res")
for (et in names(F)) {
  rows <- list()
  for (f in F[[et]]) {
    tgt <- f[[1]]; inc <- f[[2]]; exc <- f[[3]]
    d <- m[entity_type == et & grepl(inc, variant)]
    if (!is.na(exc)) d <- d[!grepl(exc, variant)]
    d <- d[bag_id != tgt]
    cat(sprintf("\n%-58s <- %d variants, %d bags\n", tgt, nrow(d), uniqueN(d$bag_id)))
    if (nrow(d)) cat("   ", paste(head(d$variant, 40), collapse = "\n    "), "\n")
    if (nrow(d)) rows[[length(rows)+1L]] <- data.table(variant = d$variant, bag_id = tgt)
  }
  fn <- sprintf("Network_Innovation_Paper/inputs/entity_bag_overrides_%s.csv", tolower(et))
  # IDEMPOTENCE. Once these folds are applied, every family above finds nothing left
  # to move, so a naive rerun would write an EMPTY file and silently undo the whole
  # curation. Union with whatever is already on disk, let the existing file win any
  # conflict, and refuse to shrink.
  old <- if (file.exists(fn)) fread(fn, colClasses = "character") else
    data.table(variant = character(), bag_id = character())
  r <- rbindlist(rows, fill = TRUE)
  r <- if (nrow(r)) unique(r, by = "variant")[, .(variant, bag_id)] else
    data.table(variant = character(), bag_id = character())
  r <- rbind(old[, .(variant, bag_id)], r[!variant %chin% old$variant])
  r <- r[bag_id != paste0(PFX[[et]], "_", variant)]
  if (nrow(r) < nrow(old))
    stop(sprintf("%s: refusing to shrink %s from %d to %d rows",
                 et, basename(fn), nrow(old), nrow(r)))
  # A target is valid if it is already a bag, OR if its suffix is a real variant
  # spelling in this type — an override both creates and anchors the bag, so the
  # id need not pre-exist, it just must name something that actually appears.
  sfx <- sub(paste0("^", PFX[[et]], "_"), "", r$bag_id)
  syn <- unique(r$bag_id[!(r$bag_id %chin% m$bag_id | sfx %chin% m[entity_type == et, variant])])
  if (length(syn)) cat(sprintf("\n[%s] SYNTHETIC bag names (no variant spells them cleanly; the\n  override both creates and labels the bag): %s\n",
                              et, paste(syn, collapse = ", ")))
  ch <- r[paste0(PFX[[et]], "_", variant) %chin% r$bag_id, .(variant, bag_id)]
  if (nrow(ch)) stop(et, " chain: ", paste(sprintf("%s->%s", ch$variant, ch$bag_id), collapse=", "))
  r[, `:=`(entity_type = et,
           note = "bag audit 2026-10-08: same organization, see BAG_REVIEW_FINDINGS.md §7d")]
  fwrite(r[, .(variant, bag_id, entity_type, note)], fn)
  cat(sprintf("\n>>> wrote %s: %d rows (%d carried over, %d new), %d parent bags\n",
              basename(fn), nrow(r), nrow(old), nrow(r) - nrow(old), uniqueN(r$bag_id)))
}
