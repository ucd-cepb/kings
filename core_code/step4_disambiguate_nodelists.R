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
#       used to mine per-GSP parenthetical acronyms via find_intext_acronyms()
#   plan_txts_raw_pages_core/<stem>.RDS     (from step1)
#       used to mine per-GSP front-matter acronym tables via
#       extract_front_matter_acronyms()
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
# Stem convention: step1 produces stems from the PDF filename. Two
# conventions coexist during the legacy-to-new transition:
#   (a) Legacy: v<N>_gsp_num_id_<NNNN>.pdf -> stem "v<N>_<NNNN>" (e.g.
#       "v1_0007"). v1/v2 indicate successive submissions of the same plan;
#       NNNN is the parent's gspId.
#   (b) New (from step0): gsp_num_id_<NNNN>.pdf -> stem "<NNNN>" (e.g.
#       "0007"). NNNN is the row's own globally-unique portal gspId; version
#       semantics live in core_data_source_pdfs/plan_family_manifest.csv.
# Either way, `gsp_nums <- str_extract(gspids, "[0-9]+$")` gives the bare
# zero-padded numeric id used to join per-GSP metadata (agency_tbl) that
# keys on the GSP number alone.

library(stringr)
library(data.table)
library(textNet)
library(arrow)

source("core_code/_config.R")   # provides CLOBBER, filekey, fk()

# Note: a per-stem "combined-GSA override" mechanism used to live here for
# GSPs (e.g. Yuba) where the website-listed GSAs need to be collapsed to a
# hand-curated combined list. Fill the named list below and the hook block in
# Section 1 (right after `agency_nicknames` is built) will swap the website
# nicknames for the curated list. Keys are filename stems — under the docId
# convention that's the gspDocId string (e.g. "1683" or "1682" from the
# manifest). Find the right docId by inspecting plan_family_manifest.csv:
# filter by gspId or canonical_gspId for the plan you care about, then use
# the gspDocId column as the key here.
#
# Uncomment both blocks (definition + hook) to use.
#
# combined_gsa_overrides <- list(
#   # "1683" = c("City_of_Marysville_GSA",      # gspDocId 1683 (gspId 52)
#   #            "Cordua_Irrigation_District_GSA",
#   #            "Yuba_Water_Agency_GSA"),
#   # "1682" = c("City_of_Marysville_GSA",      # gspDocId 1682 (gspId 53)
#   #            "Cordua_Irrigation_District_GSA",
#   #            "Yuba_Water_Agency_GSA")
# )
#
# Hook (paste right after the `agency_nicknames <- setNames(...)` block in
# Section 1). Replaces the website-derived nickname `to` list-cells with the
# curated list, preserving the existing (Agencies/Agency) `from` rows.
#
# for (stem in names(combined_gsa_overrides)) {
#   if (is.null(agency_nicknames[[stem]])) next
#   override_names <- combined_gsa_overrides[[stem]]
#   agency_nicknames[[stem]]$to <- list(override_names, override_names)
# }

# Ensure output destinations exist before any saveRDS call.
dir.create(fk("disambiged_extracts_core"),
           recursive = TRUE, showWarnings = FALSE)

###Section 1: GSAs####
# Per-GSP textnet_extract RDS files produced by step3 (parse_text_trf +
# entity ruler), with $edgelist + $nodelist as data.tables.
edges_and_nodes <- sort(list.files(path = fk("nondisambiged_extracts_core"), full.names = TRUE))

# Under the docId-based naming convention from step0, each stem is a
# gspDocId (e.g. "8591" or "11703"). The agency_tbl we join against is
# keyed by zero-padded gspId ("0007"), NOT gspDocId — so we have to
# translate via plan_family_manifest.csv. The manifest is keyed on
# gspDocId and carries the corresponding gspId (and canonical_gspId,
# version, fromGspId, ...) as columns.
#
# stems  : the file stems (gspDocId strings) — these label every downstream
#          artifact and the iteration is keyed on them.
# gsp_nums : the per-stem gspId, zero-padded to 4 chars, for the
#            agency_tbl join. NA for stems not in the manifest.
gspids <- stringr::str_remove(basename(edges_and_nodes), "\\.RDS$")

manifest_path <- file.path(fk("core_data_source_pdfs"),
                           "plan_family_manifest.csv")
if (!file.exists(manifest_path)) {
   stop("plan_family_manifest.csv not found at ", manifest_path,
        ". Run core_code/step0_download_from_sgma.R (or ",
        "core_code/migrate_pdf_naming.R) first so this script can ",
        "translate gspDocId stems to gspId for the agency_tbl join.")
}
.manifest <- data.table::fread(manifest_path,
                               colClasses = list(character = c("gspDocId", "gspId")))
# manifest$gspId is already stored as a 4-digit padded string (step0 / migrate
# normalize it on write). Use it directly as the agency_tbl join key.
.docid_to_gspid <- setNames(.manifest$gspId, as.character(.manifest$gspDocId))
gsp_nums <- unname(.docid_to_gspid[gspids])
.missing_in_manifest <- is.na(gsp_nums)
if (any(.missing_in_manifest)) {
   warning(sum(.missing_in_manifest),
           " RDS stem(s) not found in plan_family_manifest.csv — ",
           "agency_nicknames will be NULL for those GSPs: ",
           paste(gspids[.missing_in_manifest], collapse = ", "))
}

# === Resolve the to-do subset up front ===
# Build the disambig output paths now, then derive the indices that still
# need work (or all of them under CLOBBER). Sections 2 / 4 / 5 below all
# iterate over this todo_idx so we don't read parquets, mine acronyms, or
# build customdt for GSPs whose disambiguated extract already exists.
disambig_dir   <- fk("disambiged_extracts_core")
out_paths_all  <- file.path(disambig_dir, paste0(gspids, ".RDS"))
todo_idx <- if (CLOBBER) {
   seq_along(edges_and_nodes)
} else {
   which(!file.exists(out_paths_all))
}
n_skipped <- length(edges_and_nodes) - length(todo_idx)
if (n_skipped > 0L) {
   message("Skipping ", n_skipped, " GSP(s) with existing disambiguated extracts ",
           "(set CORE_CLOBBER=1 to rebuild).")
}
if (length(todo_idx) == 0L) {
   message("Nothing to do; all disambiguated extracts already present.")
}

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

# Named list keyed by filename stem (gspDocId string under the new
# convention; legacy stems also accepted). Section 4 looks these up by
# stem to attach the right nicknames to each per-GSP customdt. NULL
# entries for stems whose gspId couldn't be resolved via the manifest
# or that have no agency_tbl row.
agency_nicknames <- setNames(
   Map(build_nicknames_for_gsp, gspids, gsp_nums),
   gspids
)

###Section 2: Acronyms####
# Mine doc-local acronyms from each GSP via two complementary sources,
# using the textNet >= 1.0.1 API where the two parsers are separate
# exported functions:
#   - find_intext_acronyms()         : parenthetical defs ("Long Form (ACR)")
#                                      run against the step2 cleaned text.
#   - extract_front_matter_acronyms(): acronym-table front-matter pages run
#                                      against the step1 raw RDS (newlines
#                                      and multi-space gaps preserved — the
#                                      table parser needs the original
#                                      "ACR    Long Form" column layout).
# The two results are rbind'd front-matter-first and deduped by acronym so
# front-matter definitions win on conflict (they're the document's own
# glossary). Then the generic GSA/GSP seed is prepended and a second dedupe
# by acronym is run so gsa_seed wins on the four SGMA-vocabulary terms.
# Final rename to (to, from) for the customdt rbind in Section 4.
#
# NOTE: the previous unified `find_acronyms(pdftxt, table_text = ...)` call
# is gone in textNet 1.0.1 — `find_acronyms()` is now a deprecated 1-arg
# alias for `find_intext_acronyms()` and does not accept `table_text`.
clean_dir     <- fk("plan_txts_clean_core")
raw_pages_dir <- fk("plan_txts_raw_pages_core")
acrons        <- vector(mode="list", length=length(edges_and_nodes))
gsa_seed      <- data.table(
   name    = c("Groundwater_Sustainability_Agencies",
               "Groundwater_Sustainability_Agency",
               "Groundwater_Sustainability_Plan",
               "Groundwater_Sustainability_Plans"),
   acronym = c("GSAs","GSA","GSP","GSPs"))

# Only mine acronyms for GSPs that still need disambiguation. Indices
# outside todo_idx leave acrons[[m]] as NULL — those slots are never
# read again because Section 4 / 5 iterate the same todo_idx.
for(m in todo_idx){
   parquet_path   <- file.path(clean_dir,     paste0(gspids[m], ".parquet"))
   raw_pages_path <- file.path(raw_pages_dir, paste0(gspids[m], ".RDS"))
   if(!file.exists(parquet_path)){
      warning("Missing cleaned parquet for GSP ", gspids[m], ": ", parquet_path)
      mined <- data.table(name=character(0), acronym=character(0))
   } else {
      pdftxt_m <- arrow::read_parquet(parquet_path)$text
      pdftxt_m <- pdftxt_m[!is.na(pdftxt_m) & nchar(pdftxt_m) > 0]
      # Front-matter acronym tables come from the step1 raw-pages RDS
      # (newlines + multi-space gaps preserved). Backwards-compat: if the
      # RDS doesn't exist (pre-raw-pages step1 run), skip the table parser
      # with a one-time warning per GSP rather than forcing a step1 rerun.
      table_text <- if (file.exists(raw_pages_path)) {
         readRDS(raw_pages_path)
      } else {
         warning("Missing raw-pages RDS for GSP ", gspids[m],
                 " — extract_front_matter_acronyms skipped. ",
                 "Re-run step1 to populate ", raw_pages_path, ".")
         NULL
      }
      # Two parsers, two surfaces. rbind front-matter first so it wins on
      # conflict after unique(by = "acronym") below — the document's own
      # glossary is the most authoritative source.
      fm_mined     <- if (!is.null(table_text))
                         extract_front_matter_acronyms(table_text)
                      else data.table(name=character(0), acronym=character(0))
      inline_mined <- find_intext_acronyms(pdftxt_m)
      mined        <- unique(rbind(fm_mined, inline_mined), by = "acronym")
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
# Normalize dict names to the surface form step4 actually compares against.
# The dicts on disk are intentionally space-separated so step3's entity ruler
# (which matches raw PDF text) can use them; step3 also explicitly filters
# its term list to multi-word entries via str_count(..., "\\s+") >= 1L, so
# the spaces have to stay on disk. But the nodelist that step4 disambiguates
# uses spaCy's concatenated form (spaces & hyphens → underscores), so we
# convert here before clean_entities() so canonicals and aliases meet the
# nodelist in the same surface form. Same rule step4 Section 1 already
# applies to agency_tbl$name_gsas. Idempotent — re-applying to an already-
# underscored name is a no-op.
to_concat <- function(x) stringr::str_replace_all(x, "-|\\s", "_")

load_alias_pairs <- function(f) {
   if (!file.exists(f)) { warning("Missing dictionary: ", f); return(NULL) }
   d <- read.csv(f, stringsAsFactors = FALSE)
   if ("all_names" %in% names(d)) {
      rbindlist(lapply(d$all_names, function(s) {
         parts <- strsplit(s, "|", fixed = TRUE)[[1]]
         if (length(parts) < 2L) return(NULL)   # canonical only, no alias
         canonical <- clean_entities(to_concat(parts[1]),  remove_nums = TRUE)
         aliases   <- clean_entities(to_concat(parts[-1]), remove_nums = TRUE)
         data.table(to = canonical, from = aliases)
      }))
   } else if (all(c("Agency", "Abbr") %in% names(d))) {
      canonical <- clean_entities(to_concat(d$Agency), remove_nums = TRUE)
      aliases   <- clean_entities(to_concat(d$Abbr),   remove_nums = TRUE)
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
# Same todo_idx as Section 2 — only build customdt for the GSPs that need it.
for(m in todo_idx){
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

# disambig_dir / out_paths_all already computed up front; iterate todo_idx.
# The per-iteration file.exists check is gone — todo_idx already excluded
# anything that exists (and includes everything under CLOBBER).
for(m in todo_idx){
   out_path <- out_paths_all[m]
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
      # disambiguate() requires from / to as lists of character vectors
      # (a single-string row becomes a length-1 vector, an agency-nicknames
      # row keeps its multi-name vector). customdt[[m]]$from is always a
      # plain character column; $to is a list-column whenever any GSP row
      # contributed via agency_nicknames, but falls back to character
      # otherwise. as.list() normalizes both cases.
      edgenodelist <- disambiguate(from = as.list(customdt[[m]]$from),
                                   to   = as.list(customdt[[m]]$to),
                                   match_partial_entity = customdt[[m]]$match_partial_entity,
                                   textnet_extract = edgenodelist,
                                   try_drop = try_drop)
      atomic_saveRDS(edgenodelist, out_path)
   }, error = function(e) {
      message(sprintf("  ERROR (%s): %s -- continuing", gspids[m], conditionMessage(e)))
   })
}

