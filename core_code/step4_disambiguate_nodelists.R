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
#   CORE_DICT_KEYS (from _config.R)         (all alias-bearing dicts:
#                                            4 water + ca_utilities +
#                                            gov_entities)
#
# Outputs:
#   disambiged_extracts_core/<stem>.RDS     (textnet_extract object with
#                                            entity names canonicalized;
#                                            nodelist gains raw_entity_name
#                                            preserving the pre-disambig form)
#
# Pipeline order:
#   1. Build a per-GSP disambiguation dictionary `customdt[[m]]` by stacking:
#        - per-GSP acronyms (acrons) found in the cleaned text
#        - global alias->canonical pairs (global_dict) from every CORE
#          dictionary (schema-aware: handles all_names AND Agency/Abbr)
#        - per-GSP GSA nicknames (agency_nicknames) — collapses specific GSA
#          names to the generic "Groundwater_Sustainability_Agency/Agencies"
#          reference (self-reference / coreference within the plan).
#      Self-mappings (from == to) are filtered out — a dict row only
#      contributes if it links an alias to a different canonical.
#      Resolve any duplicate `from` keys so disambiguate() has a clean map.
#   2. For each GSP, read step3's tabular extract, snapshot the pre-
#      disambig nodelist entity_name as raw_entity_name, normalize the
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

source("core_code/_config.R")   # provides CLOBBER, filekey, fk()

# Note: a per-stem "combined-GSA override" mechanism used to live here for
# GSPs (e.g. Yuba) where the website-listed GSAs need to be collapsed to a
# hand-curated combined list. It was never populated in this repo. If/when
# you need to re-add it, attach a per-stem hook in Section 4's customdt
# assembly — see git history for the previous scaffold.

# Ensure output destinations exist before any saveRDS call.
dir.create(fk("disambiged_extracts_core"),
           recursive = TRUE, showWarnings = FALSE)

###Section 1: GSAs####
# Per-GSP textnet_extract RDS files produced by step3 (parse_text_trf +
# entity ruler), with $edgelist + $nodelist as data.tables.
edges_and_nodes <- sort(list.files(path = fk("nondisambiged_extracts_core"), full.names = TRUE))
# gspids is the full filesystem stem (e.g. v1_0007) so it round-trips with
# the parquet / RDS paths written by step1 and step2. The v1/v2 prefix is
# the *plan version* (submission/revision of the GSP document), NOT a
# pipeline version.
# gsp_nums is the bare zero-padded numeric GSP id (e.g. 0007) for joining
# to per-GSP metadata like agency_tbl, which is shared across plan versions
# and therefore keyed on the GSP number alone.
gspids   <- stringr::str_remove(basename(edges_and_nodes), "\\.RDS$")
gsp_nums <- stringr::str_extract(gspids, "[0-9]+$")

agency_tbl <- readRDS(fk("gsa_table_core"))
agency_tbl <- agency_tbl[!is.na(gsp_id),]
#change hyphens and spaces to underscores, to match spacy parse formatting
agency_tbl$name_gsas <- lapply(agency_tbl$name_gsas, function(w)
   stringr::str_replace_all(w,"-|\\s","_"))

# Build a 2-row (to, from) table mapping the per-GSP GSA name list to the
# generic "Agency"/"Agencies" tokens used in the plan text. Returns NULL
# when agency_tbl has no entry for the GSP — those GSPs simply contribute
# no nickname rows to customdt downstream.
build_nicknames_for_gsp <- function(stem, gsp_num) {
   rows <- agency_tbl[gsp_id == gsp_num]
   if (nrow(rows) == 0L) {
      warning("No agency_tbl entry for GSP ", gsp_num, " (", stem, "); skipping nicknames")
      return(NULL)
   }
   agency_names <- rows$name_gsas[[1]]
   # strip trailing parenthetical disambiguators before clean_entities()
   agency_names <- unlist(lapply(agency_names, function(b) str_split(b, "\\(")[[1]][1]))
   agency_names <- clean_entities(agency_names, remove_nums = TRUE)

   plural <- isTRUE(rows$mult_gsas[1])
   abbr   <- if (plural) c("Groundwater_Sustainability_Agencies", "Agencies")
             else        c("Groundwater_Sustainability_Agency",   "Agency")

   data.table(to = list(agency_names, agency_names),
              from = abbr)
}

# Named list keyed by stem (e.g. "v1_0007") so Section 4 can look these up
# without integer arithmetic. NULL entries for GSPs without an agency_tbl row.
agency_nicknames <- setNames(
   Map(build_nicknames_for_gsp, gspids, gsp_nums),
   gspids
)

###Section 2: Acronyms####
# Mine doc-local acronyms from each GSP via two complementary sources:
#   - parenthetical defs ("Long Form (ACR)") from the step2 cleaned text
#   - acronym-table front-matter pages from the step1 raw RDS (newlines and
#     multi-space gaps preserved — required by find_acronyms' table_text
#     parser to find the "ACR    Long Form" row format).
# Prepend the generic GSA/GSP seed (so those four SGMA-vocabulary terms
# always resolve regardless of whether the plan defined them), dedupe by
# acronym (gsa_seed wins on ties because it's first in the rbind), and
# rename to (to, from) for the customdt rbind in Section 4.
clean_dir     <- fk("plan_txts_clean_core")
raw_pages_dir <- fk("plan_txts_raw_pages_core")
acrons        <- vector(mode="list", length=length(edges_and_nodes))
gsa_seed      <- data.table(
   name    = c("Groundwater_Sustainability_Agencies",
               "Groundwater_Sustainability_Agency",
               "Groundwater_Sustainability_Plan",
               "Groundwater_Sustainability_Plans"),
   acronym = c("GSAs","GSA","GSP","GSPs"))

for(m in seq_along(edges_and_nodes)){
   parquet_path   <- file.path(clean_dir,     paste0(gspids[m], ".parquet"))
   raw_pages_path <- file.path(raw_pages_dir, paste0(gspids[m], ".RDS"))
   if(!file.exists(parquet_path)){
      warning("Missing cleaned parquet for GSP ", gspids[m], ": ", parquet_path)
      mined <- data.table(name=character(0), acronym=character(0))
   } else {
      pdftxt_m <- arrow::read_parquet(parquet_path)$text
      pdftxt_m <- pdftxt_m[!is.na(pdftxt_m) & nchar(pdftxt_m) > 0]
      # Raw pages enable find_acronyms' acronym-table front-matter parser.
      # Backwards-compat: if the RDS doesn't exist (pre-raw-pages step1 run),
      # skip the table_text path with a one-time warning per GSP rather than
      # forcing a step1 rerun.
      table_text <- if (file.exists(raw_pages_path)) {
         readRDS(raw_pages_path)
      } else {
         warning("Missing raw-pages RDS for GSP ", gspids[m],
                 " — find_acronyms running without table_text path. ",
                 "Re-run step1 to populate ", raw_pages_path, ".")
         NULL
      }
      mined <- find_acronyms(pdftxt_m, table_text = table_text)
      mined$name    <- clean_entities(mined$name)
      mined$acronym <- clean_entities(mined$acronym)
   }
   acrons[[m]] <- unique(rbind(gsa_seed, mined), by="acronym")
   setnames(acrons[[m]], c("to","from"))
}
###Section 3: Global disambiguation dictionary####
# CORE_DICT_KEYS is the canonical list defined in _config.R; step3 reads
# the same list, so both steps stay in sync. The builder below handles
# both schemas:
#   - all_names  (pipe-delimited: first token canonical, rest are aliases)
#       used by water_entity, water_infrastructure, water_bodies,
#       water_gsa, ca_utilities
#   - Agency/Abbr columns  (one alias per row)
#       used by gov_entities_dict_core
# A dictionary row only contributes to disambiguation if it links an
# alias to a *different* canonical (from != to). Self-mappings and
# single-name rows are filtered out because they tell disambiguate()
# nothing it doesn't already know.
load_alias_pairs <- function(f) {
   if (!file.exists(f)) { warning("Missing dictionary: ", f); return(NULL) }
   d <- read.csv(f, stringsAsFactors = FALSE)
   if ("all_names" %in% names(d)) {
      rbindlist(lapply(d$all_names, function(s) {
         parts <- strsplit(s, "|", fixed = TRUE)[[1]]
         if (length(parts) < 2L) return(NULL)   # canonical only, no alias
         canonical <- clean_entities(parts[1],  remove_nums = TRUE)
         aliases   <- clean_entities(parts[-1], remove_nums = TRUE)
         data.table(to = canonical, from = aliases)
      }))
   } else if (all(c("Agency", "Abbr") %in% names(d))) {
      canonical <- clean_entities(d$Agency, remove_nums = TRUE)
      aliases   <- clean_entities(d$Abbr,   remove_nums = TRUE)
      data.table(to = canonical, from = aliases)
   } else {
      warning("Unknown dictionary schema (", f,
              "): expected 'all_names' or Agency+Abbr")
      NULL
   }
}

dict_csvs   <- vapply(CORE_DICT_KEYS, fk, character(1))
global_dict <- rbindlist(lapply(dict_csvs, load_alias_pairs))
global_dict <- unique(global_dict[
   !is.na(to) & !is.na(from) &
   nchar(to) > 0 & nchar(from) > 0 &
   to != from
])

###Section 4: Build customdt + resolve duplicates####
# Stack the three (to, from) source tables into one per-GSP disambiguation
# map and resolve any duplicated `from` keys so disambiguate() has a clean
# 1:1 alias-to-canonical mapping. Sources:
#   - acrons[[m]]          : per-GSP doc-mined acronyms (Section 2)
#   - global_dict          : alias→canonical pairs from every CORE dict
#                            (Section 3, schema-aware, self-mappings filtered)
#   - agency_nicknames[[stem]] : per-GSP GSA name list → "Agency"/"Agencies"
#                                generic (Section 1)
# acrons[[m]] was renamed to (to, from) at build time in Section 2.
# agency_nicknames is already a per-stem named list of (to, from) tables.

# Empty (to, from) table to substitute for GSPs that had no agency_tbl row.
empty_nicks <- data.table(to = list(), from = character())

customdt <- vector(mode="list",length=length(edges_and_nodes))
for(m in seq_along(edges_and_nodes)){
   customdt[[m]] <- rbind(acrons[[m]], global_dict)
   customdt[[m]] <- unique(customdt[[m]])

   nicks_m <- agency_nicknames[[ gspids[m] ]]
   if (is.null(nicks_m)) nicks_m <- empty_nicks

   match_partial_entity <- rep(c(TRUE, FALSE), c(nrow(customdt[[m]]), nrow(nicks_m)))
   customdt[[m]] <- rbind(customdt[[m]], nicks_m)
   customdt[[m]]$match_partial_entity <- match_partial_entity
   rm(match_partial_entity)
   fromgroups <- table(customdt[[m]]$from)
   # Collisions can come from any combination of the three sources stacked
   # into customdt[[m]] (acrons + global_dict + agency_nicknames), so the
   # same `from` key can map to 2, 3, or more `to` values. The resolution
   # loop below handles the multi-way case.
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
# For each GSP: read the step3 textnet_extract, snapshot the original
# entity_name as raw_entity_name on the nodelist (for crosswalk/debugging),
# normalize the entity-name columns through clean_entities() so they're in
# the same surface form as the customdt map, then run disambiguate() and
# save.
#
# The normalization pass exists because textnet_extract's concatenator and
# clean_entities() drift on punctuation: a hyphen, ampersand, or apostrophe
# can survive the spaCy + concatenator path on one side but get rewritten on
# the dict side. Running both through clean_entities() guarantees they meet
# in the same surface form before disambiguate() does its lookups.
#
# raw_entity_name carries the pre-normalization spaCy surface form through
# disambiguation untouched (disambiguate() only rewrites entity_name on the
# nodelist and source/target on the edgelist). step5 dedups nodelist rows
# whose canonical entity_name collides; whichever raw lands on the surviving
# row becomes that vertex's raw attribute. To reverse-engineer all the
# aliases that mapped to a given canonical, consult the dict files — they're
# the authoritative source of the canonical→aliases relationship.
#
# should not drop "us" from custom list, or from nodelist/edgelist. however,
# if it doesn't match "us" on the first pass we drop "us" from the
# nodelist/edgelist and try again to match with the custom list.
try_drop <- "^US_|^U_S_|^United_States_|^UnitedStates_"

disambig_dir <- fk("disambiged_extracts_core")
for(m in seq_along(edges_and_nodes)){
   out_path <- file.path(disambig_dir, paste0(gspids[m], ".RDS"))
   if(!CLOBBER && file.exists(out_path)){
      next
   }
   message("[", m, "/", length(edges_and_nodes), "] ", gspids[m])
   tryCatch({
      edgenodelist <- readRDS(edges_and_nodes[m])
      # Snapshot the raw spaCy surface form BEFORE any normalization, so
      # downstream consumers can crosswalk canonical -> observed alias.
      edgenodelist$nodelist$raw_entity_name <- edgenodelist$nodelist$entity_name
      # Normalize entity-name columns so they match the customdt surface form.
      edgenodelist$edgelist$source      <- clean_entities(edgenodelist$edgelist$source)
      edgenodelist$edgelist$target      <- clean_entities(edgenodelist$edgelist$target)
      edgenodelist$nodelist$entity_name <- clean_entities(edgenodelist$nodelist$entity_name)
      edgenodelist <- disambiguate(from = customdt[[m]]$from,
                                   to   = customdt[[m]]$to,
                                   match_partial_entity = customdt[[m]]$match_partial_entity,
                                   textnet_extract = edgenodelist,
                                   try_drop = try_drop)
      atomic_saveRDS(edgenodelist, out_path)
   }, error = function(e) {
      message(sprintf("  ERROR (%s): %s -- continuing", gspids[m], conditionMessage(e)))
   })
}

