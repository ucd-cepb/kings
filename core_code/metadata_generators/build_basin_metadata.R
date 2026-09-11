# build_basin_metadata.R
# Build the canonical core BASIN metadata table (basin-level metadata).
#
# The repo already carries metadata at two levels -- per-GSA (sgma_gsa_full.csv,
# keyed on GSA_ID) and per-plan/document (gsp_page_sections.csv etc., keyed on
# gsp_doc_id). Some attributes, though, are properties of the groundwater BASIN
# (Bulletin 118 subbasin), not of any one agency or plan -- most importantly the
# DWR SGMA prioritization (Very Low / Low / Medium / High). This script produces
# the missing third level: one canonical basin-level table so any paper can join
# a basin attribute onto a plan or a GSA without re-deriving it.
#
# WHY this replaces the old handling: priority used to live only as a per-plan
# column (`priority_category`) inside the Network/STM papers' hand-curated
# gsp_covariates.csv, derived deep in the STM pipeline and never refreshed. That
# file stops at gsp_id 0156, so the newer plans (0157+) carried NO priority and
# entered the models as NA -- even when a sibling plan in the SAME basin had a
# known value, because the join was on plan, not basin. Sourcing priority from a
# basin table keyed on the Bulletin 118 subbasin number fixes this structurally:
# every plan/GSA in a basin inherits that basin's one authoritative priority.
#
# Source of truth: DWR's "i08 B118 SGMA 2019 Basin Prioritization" layer on the
# CA state geoportal (gis.water.ca.gov) -- the same live DWR service family that
# sgma_gsa_full (i03) and gsp_boundaries (i03) already draw from. This is the
# final 2019 prioritization released 2019-12-06, the authoritative statewide
# ranking of all 515 Bulletin 118 basins/subbasins. We pull the feature layer's
# attributes (no geometry needed) so the table carries basin names alongside the
# priority.
#
# KEY: Basin_Subbasin_Number (Bulletin 118 subbasin id, e.g. "5-022.14"). This
# is the id used everywhere else in the repo to name a basin: sgma_gsa_full has
# a Basin_Subbasin_Number column, and the NIP id_crosswalk's `basin` field is
# "<subbasin_number> <SUBBASIN NAME>" (e.g. "5-022.14 KERN COUNTY"), whose
# leading token is exactly this key. So both a GSA (via sgma_gsa_full) and a
# plan document (via the crosswalk basin string) can join to this table.
#
# Output: data/core_data/metadata/sgma_basin_full.csv, one row per B118
#   subbasin (515 rows). Columns:
#     Basin_Number, Basin_Subbasin_Number, Basin_Name, Basin_Subbasin_Name,
#     Region_Office, Area_Acres, Area_SqMiles,
#     Priority            -- raw DWR label: "Very Low" | "Low" | "Medium" | "High"
#     priority_category   -- collapsed to the repo convention used by the papers:
#                            "low_or_verylow" | "med" | "high"
#                            (Very Low/Low -> low_or_verylow, Medium -> med,
#                             High -> high). Matches the values gsp_covariates.csv
#                            previously carried, so downstream joins are drop-in.
#
# Run:  Rscript core_code/metadata_generators/build_basin_metadata.R
# Rerun anytime to refresh from the live layer; it overwrites the output.

suppressPackageStartupMessages({
  library(data.table)
  library(jsonlite)
})

source("core_code/_config.R")   # provides fk(); resolves paths via filekey.csv

# --- Source ----------------------------------------------------------------
# i08 SGMA 2019 Basin Prioritization, feature layer (id 1). We query attributes
# only (returnGeometry=false) -- the basin footprint is not needed here, only the
# subbasin id, its names, and the priority. maxRecordCount is 2000 (> 515), so a
# single where=1=1 query returns every basin at once. The service is public but
# intermittently slow/503s, so the fetch is wrapped in a small retry loop.
PRIO_QUERY <- paste0(
  "https://gis.water.ca.gov/arcgis/rest/services/Geoscientific/",
  "i08_B118_SGMA_2019_Basin_Prioritization/MapServer/1/query",
  "?where=1%3D1&outFields=*&returnGeometry=false&f=json"
)

OUT_CSV <- fk("sgma_basin_full_core")

# --- 1. Pull the live prioritization layer (with retry) --------------------
cat("=== BUILD BASIN METADATA (core basin-level metadata) ===\n")
cat("Fetching DWR i08 SGMA 2019 Basin Prioritization ...\n")

fetch_json <- function(url, tries = 4L, timeout = 200L) {
  for (i in seq_len(tries)) {
    j <- tryCatch({
      old <- options(timeout = timeout); on.exit(options(old), add = TRUE)
      fromJSON(url)
    }, error = function(e) e)
    if (!inherits(j, "error") && !is.null(j$features) && length(j$features)) return(j)
    msg <- if (inherits(j, "error")) conditionMessage(j)
           else if (!is.null(j$error)) j$error$message else "empty response"
    cat(sprintf("  attempt %d/%d failed (%s); retrying ...\n", i, tries, msg))
    Sys.sleep(3 * i)
  }
  stop("Could not fetch prioritization layer after ", tries, " attempts.")
}

j  <- fetch_json(PRIO_QUERY)
dt <- as.data.table(j$features$attributes)
if (isTRUE(j$exceededTransferLimit))
  stop("Server truncated the result (exceededTransferLimit); paging not implemented.")

# The join layer returns SDE-qualified names, e.g.
# "atlas_wmas.SDE.i08_B118_CA_GroundwaterBasins.Basin_Name". Strip the qualifier
# down to the bare field name. Where the join produces the same bare name twice
# (Basin_Subbasin_Number appears in both joined tables), the two are identical
# keys, so de-duplicating by bare name is safe.
setnames(dt, sub("^.*\\.", "", names(dt)))
dt <- dt[, .SD, .SDcols = unique(names(dt))]

# --- 2. Select, clean, and derive priority_category ------------------------
keep <- c("Basin_Number", "Basin_Subbasin_Number", "Basin_Name",
          "Basin_Subbasin_Name", "Region_Office", "Area_Acres",
          "Area_SqMiles", "Priority")
missing_cols <- setdiff(keep, names(dt))
if (length(missing_cols))
  stop("Layer schema changed; missing expected field(s): ",
       paste(missing_cols, collapse = ", "))
out <- dt[, ..keep]

# Normalize whitespace on the string key/labels.
chr_cols <- names(out)[vapply(out, is.character, logical(1))]
out[, (chr_cols) := lapply(.SD, trimws), .SDcols = chr_cols]

# Collapse DWR's 4-level Priority to the 3-level convention the papers use.
out[, priority_category := fifelse(Priority %in% c("Very Low", "Low"), "low_or_verylow",
                            fifelse(Priority == "Medium", "med",
                            fifelse(Priority == "High", "high", NA_character_)))]

# --- 3. Guards -------------------------------------------------------------
if (anyNA(out$Basin_Subbasin_Number) || any(out$Basin_Subbasin_Number == ""))
  stop("Blank Basin_Subbasin_Number in output -- key must be complete.")
dups <- out$Basin_Subbasin_Number[duplicated(out$Basin_Subbasin_Number)]
if (length(dups))
  stop("Duplicate Basin_Subbasin_Number (one row per subbasin expected): ",
       paste(unique(dups), collapse = ", "))
bad <- out[is.na(priority_category), unique(Priority)]
if (length(bad))
  stop("Unmapped Priority value(s) from DWR (update the mapping): ",
       paste(bad, collapse = ", "))

setkey(out, Basin_Subbasin_Number)

# --- 4. Write --------------------------------------------------------------
dir.create(dirname(OUT_CSV), recursive = TRUE, showWarnings = FALSE)
fwrite(out, OUT_CSV)
cat(sprintf("\nWrote %d basin(s) -> %s\n", nrow(out), OUT_CSV))
cat("priority_category distribution:\n")
print(out[, .N, by = priority_category][order(-N)])
cat("Done.\n")
