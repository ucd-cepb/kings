# step4_disambiguate_nodelists.R
# Disambiguates entity names in step3's per-GSP extracts. Input and output
# are both textnet_extract objects (lists with $edgelist + $nodelist as
# data.tables); the output is the same object with surface variants of
# entity names rewritten to canonical forms.
#
# Graph construction, filtering of incomplete edges, weighting, etc. live
# in downstream step(s) — this script is intentionally just the disambig
# pass so the downstream consumers can choose their own filtering and
# graph-build options.
#
# Inputs:
#   nondisambiged_extracts_core/<stem>.RDS  (from step3)
#       textnet_extract list: $edgelist + $nodelist as data.tables
#   plan_txts_clean_core/<stem>.parquet     (from step2)
#       used only to mine per-GSP acronyms via find_acronyms()
#   gsa_table_core                          (agency_tbl)
#   water_{entity,infrastructure,bodies,gsa}_dictionary CSVs
#   gov_entities_dict_core                  (federal+CA+local Agency<->Abbr dict)
#
# Outputs:
#   disambiged_extracts_core/<stem>.RDS     (textnet_extract object with
#                                            entity names canonicalized)
#
# Pipeline order:
#   1. Build a per-GSP disambiguation dictionary `customdt[[m]]` by stacking:
#        - per-GSP acronyms (acrons) found in the cleaned text
#        - GovSci abbreviations (govscitbl_mini)
#        - global water alias->canonical pairs (global_dict)
#        - per-GSP GSA nicknames (agency_nicknames) — collapses specific GSA
#          names to the generic "Groundwater_Sustainability_Agency/Agencies"
#          reference (self-reference / coreference within the plan).
#      Resolve any duplicate `from` keys so disambiguate() has a clean map.
#   2. For each GSP, read step3's tabular extract, normalize the
#      entity-name columns through clean_entities() so they match the
#      customdt surface form (closes a textnet_extract concatenator vs.
#      clean_entities punctuation-drift gap), run disambiguate() on the
#      $edgelist / $nodelist, save the rewritten extract.
#
# Why disambiguate on the tabular extract (rather than a graph object):
#   disambiguation merges surface variants of the same entity into a single
#   canonical string ("CVRWQCB", "Central_Valley_Regional_Water_Quality_Control_Board"
#   -> one name). Doing this at the row level is the cleanest place: a
#   downstream graph_from_data_frame() will then naturally collapse those
#   rows into one vertex.
#
# Stem convention: per step1's update, file stems are the plan-version
# prefixed id (e.g. v1_0007). The v1/v2/... prefix refers to successive
# submissions/revisions of the GSP itself (the document the agency filed) —
# it is NOT a version of the data-processing pipeline. v1 and v2 of the
# same GSP are treated as separate cases throughout this pipeline because
# their text content differs. `gsp_nums` is the bare zero-padded numeric
# GSP id used only for joining to per-GSP metadata (agency_tbl) that keys
# on the GSP number alone and is shared across plan versions.

library(stringr)
library(data.table)
library(textNet)
library(arrow)

# === Flags ===
CLOBBER <- FALSE   # set TRUE to re-disambiguate even if an output RDS exists

# === Per-GSP override: combined GSAs ============================================
# A few GSPs cover multiple GSAs that the agency website lists separately but
# that the plan documents treat as a single combined entity. For these stems
# we replace the agency_nicknames rows with a hand-curated GSA list, so the
# "Agency"/"Agencies" generic mention in the plan collapses to the right set
# of named GSAs.
#
# Key is the file stem (e.g. "v1_0042"), so the override applies to a specific
# (plan version, GSP number) pair — v1 and v2 of the same GSP can differ if
# the signatory list changed between submissions.
#
# Previously this was an `if (m %in% c(38, 39))` index-based check, which
# broke when filename ordering changed (plan-version prefixes shifted every
# GSP's position in the sorted list).
#
# TODO: fill in the actual stems for the Yuba combined GSP (and any others
# that need the same treatment). With this list empty, the override is a
# no-op — which is correct-but-conservative until the right stems are known.
combined_gsa_overrides <- list(
  # "v1_0052" = c("City_of_Marysville_GSA",
  #               "Cordua_Irrigation_District_GSA",
  #               "Yuba_Water_Agency_GSA"),
  # "v2_0053" = c("City_of_Marysville_GSA",
  #               "Cordua_Irrigation_District_GSA",
  #               "Yuba_Water_Agency_GSA")
)

filekey <- read.csv("filekey.csv")

# Ensure output destinations exist before any saveRDS call.
dir.create(filekey[filekey$var_name=="disambiged_extracts_core",]$filepath,
           recursive = TRUE, showWarnings = FALSE)

###Section 1: Government-entities dictionary####
# Load the natural-format snapshot built by build_gov_entities_dict.R and
# apply textNet::clean_entities() normalization at load time, matching what
# Section 3b does for the water dicts and what step3's spaCy parse produces
# for entity text. dedupe collapses rows that differ only in formatting
# (e.g. "Cal-Am" and "Cal Am" both -> "Cal_Am").
govscitbl <- read.csv(filekey[filekey$var_name=="gov_entities_dict_core",]$filepath,
                      stringsAsFactors = FALSE, na.strings = "")
setDT(govscitbl)
govscitbl[, State  := clean_entities(State,  remove_nums = TRUE)]
govscitbl[, Agency := clean_entities(Agency, remove_nums = TRUE)]
govscitbl[, Abbr   := clean_entities(Abbr,   remove_nums = TRUE)]
govscitbl <- unique(govscitbl)
###Section 2: GSAs####
# Per-GSP textnet_extract RDS files produced by step3 (parse_text_trf +
# entity ruler), with $edgelist + $nodelist as data.tables.
edges_and_nodes <- sort(list.files(path = filekey[filekey$var_name=="nondisambiged_extracts_core",]$filepath, full.names = T))
# gspids is the full filesystem stem (e.g. v1_0007) so it round-trips with
# the parquet / RDS paths written by step1 and step2. The v1/v2 prefix is
# the *plan version* (submission/revision of the GSP document), NOT a
# pipeline version.
# gsp_nums is the bare zero-padded numeric GSP id (e.g. 0007) for joining
# to per-GSP metadata like agency_tbl, which is shared across plan versions
# and therefore keyed on the GSP number alone.
gspids   <- stringr::str_remove(basename(edges_and_nodes), "\\.RDS$")
gsp_nums <- stringr::str_extract(gspids, "[0-9]+$")

agency_tbl <- readRDS(filekey[filekey$var_name=="gsa_table_core",]$filepath)
agency_tbl <- agency_tbl[!is.na(gsp_id),]
#change hyphens and spaces to underscores, to match spacy parse formatting
agency_tbl$name_gsas <- lapply(agency_tbl$name_gsas, function(w)
   stringr::str_replace_all(w,"-|\\s","_"))
#initialize empty dt
agency_nicknames <- setDT(list("name"=rep(vector(mode="list",length(edges_and_nodes)*2)),
                               "nickname"=rep(NA_character_,length(edges_and_nodes)*2)))
for(m in 1:length(edges_and_nodes)){
   #remove parentheses
   agency_rows <- agency_tbl[gsp_id==gsp_nums[m]]
   if(nrow(agency_rows) == 0L){
      warning("No agency_tbl entry for GSP ", gsp_nums[m],
              " (", gspids[m], "); leaving agency_nicknames rows empty")
      next
   }
   agency_names <- agency_rows$name_gsas[[1]]
   agency_names <- unlist(lapply(agency_names, function(b) str_split(b,"\\(")[[1]][1]))
   #clean entities so they are same format as spacy tokens
   agency_names <- clean_entities(agency_names, remove_nums=T)
   # mult_gsas is a per-GSP flag; force scalar and default to FALSE if missing
   plural <- agency_rows$mult_gsas[1]
   if(is.na(plural)) plural <- FALSE

   if(isTRUE(plural)){
      agency_abbr <- c("Groundwater_Sustainability_Agencies","Agencies")
   }else{
      agency_abbr <- c("Groundwater_Sustainability_Agency","Agency")
   }
   agency_nicknames[m*2-1, (colnames(agency_nicknames)):= list(agency_names, agency_abbr[1])]
   agency_nicknames[m*2, (colnames(agency_nicknames)):= list(agency_names, agency_abbr[2])]

   # Combined-GSA override keyed on stem (plan version + GSP number), not
   # loop index — see combined_gsa_overrides definition near the top.
   if(gspids[m] %in% names(combined_gsa_overrides)){
      override_names <- combined_gsa_overrides[[gspids[m]]]
      agency_nicknames[(m*2-1):(m*2)]$name <- list(override_names, override_names)
      agency_nicknames[(m*2-1)]$nickname <- "Groundwater_Sustainability_Agencies"
      agency_nicknames[(m*2)]$nickname    <- "Agencies"
   }
}

###Section 3: Acronyms####
# Mine doc-local acronyms from each GSP's cleaned text, prepend the generic
# GSA/GSP seed (so those always resolve regardless of whether they were
# defined in-doc), dedupe by acronym, and rename columns to (to, from) for
# the customdt rbind below. Single pass per GSP.
clean_dir <- filekey[filekey$var_name=="plan_txts_clean_core",]$filepath
acrons    <- vector(mode="list", length=length(edges_and_nodes))
gsa_seed  <- data.table(
   name    = c("Groundwater_Sustainability_Agencies",
               "Groundwater_Sustainability_Agency",
               "Groundwater_Sustainability_Plan",
               "Groundwater_Sustainability_Plans"),
   acronym = c("GSAs","GSA","GSP","GSPs"))

for(m in seq_along(edges_and_nodes)){
   parquet_path <- file.path(clean_dir, paste0(gspids[m], ".parquet"))
   if(!file.exists(parquet_path)){
      warning("Missing cleaned parquet for GSP ", gspids[m], ": ", parquet_path)
      mined <- data.table(name=character(0), acronym=character(0))
   } else {
      pdftxt_m <- arrow::read_parquet(parquet_path)$text
      pdftxt_m <- pdftxt_m[!is.na(pdftxt_m) & nchar(pdftxt_m) > 0]
      mined <- find_acronyms(pdftxt_m)
      mined$name    <- clean_entities(mined$name)
      mined$acronym <- clean_entities(mined$acronym)
   }
   acrons[[m]] <- unique(rbind(gsa_seed, mined), by="acronym")
   setnames(acrons[[m]], c("to","from"))
}
###Section 3b: Global water dictionaries####
# Curated CA water-entity/infrastructure/water-bodies/GSA CSVs (sources live
# in core_code/dicts/). Each row's all_names is a |-delimited list: first
# token is canonical, the rest are aliases. Build (to=canonical, from=alias)
# rows; canonical aliases itself too so disambiguate() leaves it intact.
dict_csvs <- c(filekey[filekey$var_name == "water_entity_dictionary",         ]$filepath,
               filekey[filekey$var_name == "water_infrastructure_dictionary", ]$filepath,
               filekey[filekey$var_name == "water_bodies_dictionary",         ]$filepath,
               filekey[filekey$var_name == "water_gsa_dictionary",            ]$filepath)
global_dict <- rbindlist(lapply(dict_csvs, function(f) {
   if(!file.exists(f)) { warning("Missing dictionary: ", f); return(NULL) }
   d <- read.csv(f, stringsAsFactors = FALSE)
   rbindlist(lapply(d$all_names, function(s) {
      parts <- strsplit(s, "|", fixed = TRUE)[[1]]
      if(length(parts) < 1L) return(NULL)
      canonical <- clean_entities(parts[1], remove_nums = TRUE)
      aliases   <- clean_entities(parts,    remove_nums = TRUE)  # incl. canonical
      data.table(to = canonical, from = aliases)
   }))
}))
global_dict <- unique(global_dict[nchar(to) > 0 & nchar(from) > 0])

###Section 4: Build customdt + resolve duplicates####
# Stack the four (to, from) source tables into one per-GSP disambiguation
# map and resolve any duplicated `from` keys so disambiguate() has a clean
# 1:1 alias-to-canonical mapping.

govscitbl_mini <- govscitbl[!is.na(Abbr) & nchar(Abbr)>0, c("Agency","Abbr")]
names(govscitbl_mini)    <- c("to","from")
names(agency_nicknames)  <- c("to","from")   # global object — rename once, not per-iteration
# acrons[[m]] was already renamed to (to, from) at build time in Section 3.

customdt <- vector(mode="list",length=length(edges_and_nodes))
for(m in seq_along(edges_and_nodes)){
   customdt[[m]] <- rbind(acrons[[m]], govscitbl_mini, global_dict)
   customdt[[m]] <- unique(customdt[[m]])
   # NB: this vector is sized to (current customdt rows + the 2 agency_nicknames
   # rows we're about to rbind in). If the agency_nicknames pre-allocation ever
   # changes from "2 rows per GSP", update both this and the rbind below.
   match_partial_entity <- rep(c(T,F), c(nrow(customdt[[m]]),nrow(agency_nicknames[(m*2-1):(m*2)])))
   customdt[[m]] <- rbind(customdt[[m]],agency_nicknames[(m*2-1):(m*2)])
   customdt[[m]]$match_partial_entity <- match_partial_entity
   rm(match_partial_entity)
   fromgroups <- table(customdt[[m]]$from)
   # Collisions can come from any combination of the four sources stacked
   # into customdt[[m]] (acrons + govscitbl + global_dict + agency_nicknames),
   # so the same `from` key can map to 2, 3, or more `to` values. The
   # resolution loop below handles the multi-way case.
   fromgroups <- fromgroups[fromgroups>1]

   if(length(fromgroups) > 0){
      ### Resolve duplicate `from` keys: longest `to` wins as canonical;
      ### the others become aliases mapping `from = makefrom -> to = keepto`.
      ### This collapses the prefix-strip / two-lengths / catch-all branches
      ### of the prior heuristic — "longer = more specific" handles all of
      ### them (California_X > X, USDA_X > X, etc.).
      for(k in seq_along(fromgroups)){
         dup_from <- names(fromgroups)[k]
         tos      <- customdt[[m]][from==dup_from]$to
         keepto   <- tos[order(-nchar(tos))][1]
         makefrom <- setdiff(tos, keepto)
         match_partial <- !(FALSE %in% customdt[[m]][from %in% dup_from]$match_partial_entity)
         customdt[[m]] <- customdt[[m]][!(from %in% dup_from)]
         customdt[[m]] <- rbind(customdt[[m]], list(keepto, dup_from, match_partial))
         for(mf in makefrom){
            if(!is.na(mf) && nzchar(mf) && !(mf %in% customdt[[m]]$from)){
               customdt[[m]] <- rbind(customdt[[m]], list(keepto, mf, match_partial))
            }
         }
      }
   }
}
###Section 5: Apply disambiguation####
# For each GSP: read the step3 textnet_extract, normalize the entity-name
# columns through clean_entities() so they're in the same surface form as
# the customdt map, then run disambiguate() and save.
#
# The normalization pass exists because textnet_extract's concatenator and
# clean_entities() drift on punctuation: a hyphen, ampersand, or apostrophe
# can survive the spaCy + concatenator path on one side but get rewritten on
# the dict side. Running both through clean_entities() guarantees they meet
# in the same surface form before disambiguate() does its lookups.
disambig_dir <- filekey[filekey$var_name=="disambiged_extracts_core",]$filepath
for(m in 1:length(edges_and_nodes)){
   out_path <- file.path(disambig_dir, paste0(gspids[m], ".RDS"))
   if(!CLOBBER && file.exists(out_path)){
      next
   }
   message("[", m, "/", length(edges_and_nodes), "] ", gspids[m])
   #should not drop "us" from custom list, or from nodelist/edgelist. however, if it doesn't match "us" on
   #drop "us" from the nodelist/edgelist and try again to match with the custom list
   try_drop <- "^US_|^U_S_|^United_States_|^UnitedStates_"
   edgenodelist <- readRDS(edges_and_nodes[m])
   # Normalize entity-name columns so they match the customdt surface form.
   edgenodelist$edgelist$source     <- clean_entities(edgenodelist$edgelist$source)
   edgenodelist$edgelist$target     <- clean_entities(edgenodelist$edgelist$target)
   edgenodelist$nodelist$entity_name <- clean_entities(edgenodelist$nodelist$entity_name)
   edgenodelist <- disambiguate(from=customdt[[m]]$from, to=customdt[[m]]$to,
                                    match_partial_entity=customdt[[m]]$match_partial_entity, textnet_extract = edgenodelist, try_drop = try_drop)
   saveRDS(edgenodelist, out_path)
}

