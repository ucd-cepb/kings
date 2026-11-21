# build_gov_entities_dict.R
# Builds gov_entities_dict.csv — a canonical Agency <-> Abbreviation map for
# federal, California state, and local government entities relevant to California
# Groundwater Sustainability Plans. Consumed by step4's disambiguation pass.
#
# Each output row has three columns:
#   - State   : "federal", "California", "local", or empty (cross-cutting concept
#               acronyms like DAC, PRISM, NCCAG, GWMP). Kept so the downstream
#               code can recognize which agencies get a California_ prefix alias.
#   - Agency  : canonical agency name, hyphens/spaces converted to underscores
#               to match spaCy entity-tokenization conventions.
#   - Abbr    : an alias for Agency (acronym, alternative name, etc.). One
#               agency can appear in multiple rows with different Abbr values;
#               the disambiguator treats each as an alias of the canonical Agency.
#
# Inputs (both expected in this folder, core_code/dicts/):
#   - govscienceuseR_agencies.RDS        — base agency table from the
#                                          govscienceuseR package output.
#                                          Copy in from data/Multipurpose_Files/
#                                          if not already present.
#   - govsci_custom_abbreviations.csv    — hand-curated (State, Agency, Abbr)
#                                          tuples, formerly the inline customabbr
#                                          rbind(...) table in
#                                          Text_Gov_Network_Methods_Paper/Code/utils/govscicleaning.R
#
# Output (this folder):
#   - gov_entities_dict.csv              — read by step4 via the
#                                          gov_entities_dict_core filekey row.
#
# What this replaces: the cleaning that used to run inside step4's Section 1
# (source(govscicleaning_script) -> clean_entities(...) -> saveRDS(govsci_tbl_clean))
# is now a build artifact. Step4 just reads the CSV. The legacy govscicleaning.R
# script stays in the paper directory untouched for paper-specific code that
# still depends on it.
#
# CA-only scope: we analyze only California GSPs, so non-CA, non-federal,
# non-local base rows are filtered out. State column is preserved on output
# because the California-prefix duplication step (a CA-row alias
# California_<Agency>) depends on it.
#
# Run from core_code/dicts/ (the sibling build_water_*_dictionary notebooks use
# the same "DICTS_DIR <- '.'" convention).

suppressPackageStartupMessages({
  library(stringr)
  library(dplyr)
  library(data.table)
})

# === Paths ===
DICTS_DIR  <- "."
BASE_RDS   <- file.path(DICTS_DIR, "govscienceuseR_agencies.RDS")
CUSTOM_CSV <- file.path(DICTS_DIR, "govsci_custom_abbreviations.csv")
OUT_CSV    <- file.path(DICTS_DIR, "gov_entities_dict.csv")

STATES_KEEP <- c("California", "federal", "local")  # base-table filter

# === 1. Load base table ===
if (!file.exists(BASE_RDS)) {
  stop("Missing base RDS: ", BASE_RDS,
       "\nCopy it in: cp data/Multipurpose_Files/govscienceuseR_agencies.RDS ",
       DICTS_DIR, "/")
}
govtbl <- readRDS(BASE_RDS)
cat(sprintf("loaded base: %d rows, cols = %s\n",
            nrow(govtbl), paste(names(govtbl), collapse = ", ")))

# === 2. Strip leading geographic prefixes and reorder multi-clause agency names ===
# Removes "The", "California", "United States", "US"/"U.S." prefixes from Agency.
# Then for names with org-words (Department/Bureau/Agency/etc.) appearing after a
# comma, reorder so the org-word phrase moves to the front.
govtbl$Agency <- str_remove(govtbl$Agency, "^The\\s")
govtbl$Agency <- str_remove(govtbl$Agency, "^California\\s")
govtbl$Agency <- str_remove(govtbl$Agency, "^United\\sStates\\s")
govtbl$Agency <- str_remove(govtbl$Agency, "^(US\\s|U\\.S\\.\\s){1,}")

# CA-only project: drop base-table rows from other states
govtbl <- govtbl[govtbl$State %in% STATES_KEEP, ]
cat(sprintf("after state filter: %d rows\n", nrow(govtbl)))

# Reorder agency name so an org-word phrase (Department/Bureau/...) sits at front
org_words <- c("Administration", "Agency", "Association", "Associates", "Authority",
               "Board", "Bureau", "Center", "^Consult[a-z]+$",
               "Commission", "Council", "County", "Department", "Datacenter", "District",
               "Foundation", "Government[s]*", "Group",
               "Institute", "LLC", "Laboratory", "Office", "Service", "Society", "Survey",
               "Univeristy")
org_phrases <- as.vector(outer(org_words, c("of", "on", "for"), paste, sep = " "))
org_phrases <- paste(org_phrases, collapse = "|")

spl <- strsplit(govtbl$Agency, ",\\s*")
orgs_reordered <- sapply(seq_along(spl), function(i) {
  if (sum(grepl(org_phrases, spl[[i]])) > 0) {
    first_hit <- grep(org_phrases, spl[[i]])[1]
    sorted <- c(spl[[i]][first_hit:length(spl[[i]])], spl[[i]][-(first_hit:length(spl[[i]]))])
    if (length(sorted) > 1) {
      sorted <- c(paste(sorted[1], sorted[2]), sorted[-(1:2)])
    }
    sorted
  } else {
    spl[[i]]
  }
})
orgs_with_commas <- sapply(orgs_reordered, function(x) paste(x, collapse = ", "))
orgs_underscore  <- sapply(strsplit(orgs_with_commas, ",*\\s+"),
                           function(x) paste(x, collapse = "_"))
govtbl$Agency <- orgs_underscore

# Drop rows whose Agency is just an uppercase acronym (case-sensitive). The
# customabbr table provides proper canonical expansions for the ones we care about.
govtbl <- govtbl[-grep("(\\b*[A-Z]+\\b*)", govtbl$Agency), ]
govtbl <- govtbl[order(govtbl$Agency), ]
cat(sprintf("after prefix strip + reorder + acronym drop: %d rows\n", nrow(govtbl)))

# === 3. Drop hand-curated bad rows from the base table ===
# These are names that survived the base import but either are wrong (acronyms
# misplaced as full names), duplicate other rows, or are unofficial spellings.
# The customabbr table covers the right canonical for each in the next step.
drop_agencies <- c(
  "Agriculture_Department",
  "Archives_National_Archives_and_Records_Administration",
  "Bureau_of_Alcohol_and_Tobacco_Tax_and_Trade",
  "Alcohol_Tobacco_Firearms_and_Explosives_Bureau",
  "Bureau_of_Consumer_Financial_Protection",
  "Bureau_of_the_Census",
  "Energy_Commission",
  "Governor’s_Office_of_Business_and_Economic_Development",
  "Office_of_Statewide_Health_Planning_and_Development",
  "CDC",
  "Commerce_Department",
  "Consumer Services, and Housing Agency California Business",
  "Department_of_Transportation",
  "Defense_Department",
  "Energy_Department",
  "Education_Department",
  "Environmental_Protection_Agency",
  "Department_of_Veterans_Affairs",
  "Fair_Housing_and_Equal_Opportunity",
  "Fannie_Mae", "Freddie_Mac", "Ginnie_Mae",
  "Health_and_Human_Services_Department",
  "Homeland_Security_Department",
  "Indian_Affairs",
  "Interior_Department",
  "Interpol",
  "Justice_Department",
  "Kennedy_Center",
  "Labor_Department",
  "NASA",
  "Archives_National_Archives_and_Records_Administration",
  "National_Library_of_Agriculture",
  "NationalMarineFisheriesService",
  "NOAA_Fisheries",
  "Northwest_Power_Planning_Council",
  "NRC",
  "Office_for_Civil_Rights_Department_of_Health_and_Human_Services",
  "Office_of_Health_Information_Integrity",
  "Open_World_Leadership_Center",
  "Presidential_Scholars_Commission",
  "Prisoner_of_War_and_Missing_in_Action_Accounting_Agency",
  "Science_Office",
  "Treasury_Department",
  "Veterans_Affairs_Department",
  "and_Geologists_Board_of_Professional_Engineers_Land_Surveyors",
  "and_Suisun_Board_of_Pilot_Commissioners_for_the_Bays_of_San_Francisco_San_Pablo",
  "House_of_Representatives",
  "Senate"
)
govtbl <- govtbl[!(govtbl$Agency %in% drop_agencies), ]
cat(sprintf("after drop list: %d rows\n", nrow(govtbl)))

# === 4. Append custom abbreviations ===
# Loaded from govsci_custom_abbreviations.csv (previously the inline customabbr
# rbind(...) in govscicleaning.R). 461 hand-curated (State, Agency, Abbr) tuples.
# Some rows have empty State — those are cross-cutting concept acronyms (DAC,
# NCCAG, PRISM, GWMP, MCL, InSAR, IAQ, GDE, DACs). They bypass the base-table
# state filter and ride through to the output unchanged.
customabbr <- read.csv(CUSTOM_CSV, stringsAsFactors = FALSE,
                       na.strings = "", colClasses = "character")
stopifnot(identical(names(customabbr), c("State", "Agency", "Abbr")))
stopifnot(identical(names(govtbl),     c("State", "Agency", "Abbr")))
cat(sprintf("customabbr: %d rows\n", nrow(customabbr)))

govtbl <- rbind(govtbl, customabbr)
cat(sprintf("after rbind: %d rows\n", nrow(govtbl)))

# === 5. Clean up Abbr column, dedupe, drop orphaned no-abbr rows ===
govtbl$Abbr <- ifelse(is.na(govtbl$Abbr), NA,
  ifelse(nchar(govtbl$Abbr) == 0, NA,
    ifelse(substr(govtbl$Abbr, 1, 1) == "\b" &
             substr(govtbl$Abbr, nchar(govtbl$Abbr), nchar(govtbl$Abbr)) == "\b",
           substr(govtbl$Abbr, 2, nchar(govtbl$Abbr) - 1),
           govtbl$Abbr)))

govtbl <- govtbl %>% distinct()

# If an agency has both no-Abbr and yes-Abbr rows, drop the no-Abbr orphans.
noabbr  <- govtbl %>% filter(is.na(Abbr) | nchar(Abbr) == 0)
yesabbr <- govtbl %>% filter(!is.na(Abbr) & nchar(Abbr) != 0)
noabbr  <- noabbr[!(noabbr$Agency %in% yesabbr$Agency), ]
govtbl  <- rbind(yesabbr, noabbr)

# Specific known disambiguator conflict: NRC also abbreviates other orgs.
govtbl <- govtbl[!(govtbl$Agency == "Nuclear_Regulatory_Commission" & govtbl$Abbr == "NRC"), ]
cat(sprintf("after dedup + orphan drop: %d rows\n", nrow(govtbl)))

# === 6. Validate: no two agencies can share the same Abbr ===
dups <- duplicated(na.omit(govtbl$Abbr))
if (sum(dups) > 0) {
  bad <- govtbl[govtbl$Abbr %in% na.omit(govtbl$Abbr)[dups], ]
  print(bad)
  stop("One or more abbreviations point to multiple agencies and cannot be disambiguated.")
}
cat("abbreviation uniqueness check passed\n")

# === 7. Add California_<Agency> aliases for California rows ===
# So that California_Department_of_Water_Resources appearing in a plan
# disambiguates back to the canonical Department_of_Water_Resources row.
calif_dupl <- govtbl %>% filter(State == "California")
calif_dupl$Abbr <- paste0("California_", calif_dupl$Agency)
cat(sprintf("California prefix aliases to add: %d rows\n", nrow(calif_dupl)))

# === 8. Normalize hyphens and whitespace to underscores ===
# spaCy treats hyphens and whitespace as token separators, so the canonical
# forms used by the disambiguator have to be already underscore-joined.
ix <- which(grepl("-|\\s", govtbl$Agency, perl = TRUE))
govtbl$Agency[ix] <- str_replace_all(govtbl$Agency[ix], "-|\\s", "_")
ix <- which(grepl("-|\\s", govtbl$Abbr, perl = TRUE))
govtbl$Abbr[ix]   <- str_replace_all(govtbl$Abbr[ix], "-|\\s", "_")

govtbl <- rbind(govtbl, calif_dupl)
cat(sprintf("after California-prefix rbind: %d rows\n", nrow(govtbl)))

# === 9. Write output CSV (natural-format) ===
# Dictionary CSVs are kept in natural format (readable; easier to hand-curate
# and inspect). Step 4 applies textNet::clean_entities() at load time to
# normalize entity strings to match what step 3's spaCy parse produces.
write.csv(govtbl, OUT_CSV, row.names = FALSE, na = "")
cat(sprintf("wrote %s (%d rows)\n", OUT_CSV, nrow(govtbl)))
print(table(govtbl$State, useNA = "ifany"))
