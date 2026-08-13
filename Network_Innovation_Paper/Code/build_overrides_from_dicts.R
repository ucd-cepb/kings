#' build_overrides_from_dicts.R — bake core_code/dicts gazetteers into the
#' paper-local entity_type_overrides.csv.
#'
#' The classifier's deterministic override table (inputs/entity_type_overrides.csv)
#' wins over both the cache and the LLM (see classify_entities.R). A handful of the
#' six leaves are IDENTITY facts a string classifier can't infer -- which named
#' agencies are GSAs, which orgs are advocacy NGOs -- and the core NER dictionaries
#' already enumerate them. This script flattens those dictionaries into exact
#' name->type override rows, on top of the HAND-CURATED focal rows (Consultant /
#' Research / NGO regexes) already in the file.
#'
#' Design:
#'   - Hand rows are preserved. Auto rows are tagged notes="gaz:<dict>" and are
#'     fully regenerated on each run (idempotent): any existing gaz:* row is
#'     dropped first, so editing a dictionary + re-running just refreshes them.
#'   - Names are normalized to the node_dictionary convention (lowercase, non-
#'     alphanumeric runs -> "_", trimmed) and `|`-aliases are split into separate
#'     rows. match="exact" -- a normalized dict name only fires on an identical
#'     node name; misses fall through to the LLM, so baking is purely additive.
#'   - Precedence when the same normalized name comes from >1 dict: GSA > NGO >
#'     Institutional_other > Non_institutional (institutional beats non; specific
#'     beats generic). A dict name that collides with an existing hand regex of a
#'     DIFFERENT type is dropped (never clobber a curated focal label).
#'
#' Run from the repo root:  Rscript Network_Innovation_Paper/Code/build_overrides_from_dicts.R

suppressMessages(library(data.table))
source(file.path(dirname(sub("^--file=", "",
  grep("^--file=", commandArgs(FALSE), value = TRUE)[1])), "_paths.R"))
if (!exists("REPO_ROOT")) source("Network_Innovation_Paper/Code/_paths.R")

# --- name normalization (must match how core builds node_dictionary names) ----
.norm <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("[^a-z0-9]+", "_", x)   # any run of non-alphanumerics -> single _
  x <- gsub("^_+|_+$", "", x)        # trim leading/trailing _
  x[nzchar(x)]
}
# Split a dict's `|`-delimited alias field into individual normalized names.
.explode <- function(all_names) unique(unlist(lapply(strsplit(all_names, "\\|"), .norm)))

# --- per-dict crosswalk: native type -> our 6-way leaf -------------------------
# Returns data.table(name, entity_type) of normalized override rows for one dict.
.rows_from <- function(dict_file, type_col, crosswalk, keep = NULL) {
  d <- fread(core_dict(dict_file), colClasses = "character")
  setnames(d, type_col, "nat")
  if (!is.null(keep)) d <- d[nat %in% keep]
  d <- d[!is.na(all_names) & nzchar(all_names)]
  rbindlist(lapply(seq_len(nrow(d)), function(i) {
    leaf <- crosswalk[[ d$nat[i] ]]
    if (is.null(leaf)) return(NULL)
    nm <- .explode(d$all_names[i])
    if (!length(nm)) return(NULL)
    data.table(name = nm, entity_type = leaf, dict = sub("\\.csv$", "", dict_file))
  }))
}

message("== building gazetteer override rows from core_code/dicts ==")

auto <- rbindlist(list(
  # GSAs: the named-agency roster (drop generic sgma_concept rows).
  .rows_from("water_gsa_dictionary.csv", "entity_type",
             list(gsa = "GSA"), keep = "gsa"),
  # Water-entity roster: NGOs (no string signal) + generic institutions + regions.
  .rows_from("water_entity_dictionary.csv", "entity_type", list(
    ngo                 = "NGO",
    water_district      = "Institutional_other",
    tribe               = "Institutional_other",
    state_agency        = "Institutional_other",
    federal_agency      = "Institutional_other",
    regional_water_board = "Institutional_other",
    irwm_region         = "Non_institutional",
    irwm_program        = "Non_institutional"
  )),
  # Physical water features + works: never institutional actors.
  .rows_from("water_bodies_dictionary.csv", "body_type",
             setNames(as.list(rep("Non_institutional", 9)),
                      c("river","groundwater_basin","reservoir","bay","lake",
                        "aquifer","estuary","wetland","delta"))),
  .rows_from("water_infrastructure_dictionary.csv", "infra_type",
             setNames(as.list(rep("Non_institutional", 12)),
                      c("dam","canal","flood_control","pumping_plant","water_treatment",
                        "groundwater_bank","wastewater_treatment","control_structure",
                        "spreading_ground","reservoir","desalination","recharge_basin")))
), use.names = TRUE, fill = TRUE)

# Precedence when one normalized name appears in multiple dicts.
prio <- c(GSA = 1L, NGO = 2L, Institutional_other = 3L, Non_institutional = 4L)
auto[, .p := prio[entity_type]]
setorder(auto, name, .p)
auto <- auto[, .SD[1], by = name]           # keep highest-precedence leaf per name
auto[, notes := paste0("gaz:", dict)]
auto <- auto[, .(pattern = name, entity_type, match = "exact", notes)]
message(sprintf("  %d unique dict-derived exact rows", nrow(auto)))
print(auto[, .N, by = entity_type][order(entity_type)])

# --- merge with the hand-curated overrides ------------------------------------
ov_path <- nip_input("entity_type_overrides.csv")
cur <- if (file.exists(ov_path)) fread(ov_path, colClasses = "character") else
  data.table(pattern = character(), entity_type = character(),
             match = character(), notes = character())
if (!"notes" %in% names(cur)) cur[, notes := ""]

hand <- cur[is.na(notes) | !grepl("^gaz:", notes)]     # everything not auto-generated
message(sprintf("  %d hand-curated rows preserved", nrow(hand)))

# Never let an auto exact row override a hand rule of a DIFFERENT type. Drop auto
# rows whose name is already caught by a hand regex (regex applied in file order)
# or hand exact of another type.
hand_rx  <- hand[match == "regex"]
hand_ex  <- hand[match == "exact"]
collides <- function(nm) {
  hit <- rep(NA_character_, length(nm))
  for (i in seq_len(nrow(hand_rx)))
    hit[is.na(hit) & grepl(hand_rx$pattern[i], nm, perl = TRUE)] <- hand_rx$entity_type[i]
  if (nrow(hand_ex)) {
    m <- hand_ex$entity_type[match(nm, hand_ex$pattern)]
    hit[!is.na(m)] <- m[!is.na(m)]
  }
  hit
}
clash <- collides(auto$pattern)
drop <- !is.na(clash) & clash != auto$entity_type
if (any(drop)) {
  message(sprintf("  dropping %d auto row(s) that clash with a hand rule of another type",
                  sum(drop)))
  print(auto[drop][, .(pattern, auto = entity_type, hand = clash[drop])][1:min(20, sum(drop))])
  auto <- auto[!drop]
}

out <- rbind(hand[, .(pattern, entity_type, match, notes)], auto, use.names = TRUE)
fwrite(out, ov_path)
message(sprintf("wrote %s (%d rows: %d hand + %d gazetteer)",
                ov_path, nrow(out), nrow(hand), nrow(auto)))
