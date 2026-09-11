# build_gsp_boundaries.R
# Build the canonical core GSP boundary shapefile (spatial metadata).
#
# GSP boundaries are reference metadata — a per-plan footprint keyed on GSP_ID
# that any paper can read (or copy) for spatial adjacency, mapping, etc. This
# script produces the single canonical copy under data/core_data/spatial/ so the
# papers stop each carrying their own hand-downloaded, drifting vintage.
#
# Source of truth: DWR's live "i03 Groundwater Sustainability Plan Areas" layer
# on the CA state geoportal (same feed sgma_gsa_full_core already draws GSA
# metadata from). The live layer only carries plans that are currently listed,
# so it drops older superseded/withdrawn plan versions. We therefore BACKFILL
# any plan the live layer lacks from the 2023 archived download that used to be
# staged by hand into each paper (data/Multipurpose_Files/GSP_Submitted/).
#
# Merge policy (i03-authoritative + archive-backfill):
#   - Every plan present in i03 takes its CURRENT i03 geometry + attributes.
#   - Plans absent from i03 are pulled from the 2023 archive, schema-harmonized.
# The 2023 archive covers GSP_ID 0007-0156; i03 adds 0157+ and refreshes the
# rest. Their union covers every plan either paper needs.
#
# Output: data/core_data/spatial/gsp_boundaries.{shp,shx,dbf,prj} in WGS 84,
# key field GSP_ID (4-digit zero-padded character), plus a `src` column marking
# each feature "i03" or "archive_2023" for provenance.
#
# Run:  Rscript core_code/metadata_generators/build_gsp_boundaries.R
# Rerun anytime to refresh from the live layer; it overwrites the output.

suppressPackageStartupMessages({
  library(sf)
  library(data.table)
})

source("core_code/_config.R")   # provides fk(); resolves paths via filekey.csv

# --- Sources ---------------------------------------------------------------
# DWR i03 GSP Areas REST layer. f=geojson + outSR=4326 gives WGS 84 straight
# out; maxRecordCount (2000) comfortably exceeds the ~116 features so one query
# returns them all.
I03_QUERY <- paste0(
  "https://gis.water.ca.gov/arcgis/rest/services/Boundaries/",
  "i03_Groundwater_Sustainability_Plan_Areas/MapServer/0/query",
  "?where=1%3D1&outFields=*&returnGeometry=true&outSR=4326&f=geojson"
)

# 2023 archived hand-download, kept only as backfill for plans i03 no longer
# lists. This is the file the papers historically copied.
ARCHIVE_DIR <- "data/Multipurpose_Files/GSP_Submitted"

OUT_DIR  <- fk("gsp_boundaries_core_dir")
OUT_SHP  <- fk("gsp_boundaries_core")

pad4 <- function(x) formatC(as.integer(x), width = 4L, format = "d", flag = "0")

# --- 1. Pull the live i03 layer -------------------------------------------
cat("=== BUILD GSP BOUNDARIES (core spatial metadata) ===\n")
cat("Fetching DWR i03 GSP Areas layer ...\n")
i03 <- st_read(I03_QUERY, quiet = TRUE)
i03 <- st_make_valid(i03)
i03 <- st_transform(i03, 4326)
i03_dt <- data.table(GSP_ID = pad4(i03$GSP_ID),
                     Basin_Name          = as.character(i03$Basin_Name),
                     Basin_Subbasin_Name = as.character(i03$Basin_Subbasin_Name),
                     Status              = as.character(i03$Status),
                     GSA_IDs             = as.character(i03$GSA_IDs),
                     src                 = "i03",
                     geometry            = st_geometry(i03))
i03_sf <- st_as_sf(i03_dt)
cat(sprintf("  i03: %d feature(s), GSP_ID %s-%s\n",
            nrow(i03_sf), min(i03_sf$GSP_ID), max(i03_sf$GSP_ID)))

# --- 2. Read the 2023 archive and harmonize its schema ---------------------
# sf reads the archive's DBF names with dots/truncation (e.g. `GSP.ID`,
# `Basin_Subb`); map the ones we keep onto the i03 schema so the merge is clean.
cat("Reading 2023 archive for backfill ...\n")
arc <- st_read(ARCHIVE_DIR, quiet = TRUE)
arc <- st_make_valid(arc)
arc <- st_transform(arc, 4326)
arc_dt <- data.table(GSP_ID = pad4(arc$GSP.ID),
                     Basin_Name          = as.character(arc$Basin_Name),
                     Basin_Subbasin_Name = as.character(arc$Basin_Su_1),
                     Status              = as.character(arc$Status),
                     GSA_IDs             = as.character(arc$GSA_IDs),
                     src                 = "archive_2023",
                     geometry            = st_geometry(arc))
arc_sf <- st_as_sf(arc_dt)

# --- 3. Backfill: keep every i03 plan, add archive-only plans --------------
backfill_ids <- setdiff(arc_sf$GSP_ID, i03_sf$GSP_ID)
backfill <- arc_sf[arc_sf$GSP_ID %in% backfill_ids, ]
cat(sprintf("  archive: %d feature(s); %d plan(s) not in i03 -> backfilled: %s\n",
            nrow(arc_sf), length(backfill_ids),
            paste(sort(backfill_ids), collapse = ", ")))

merged <- rbind(i03_sf, backfill)
merged <- merged[order(merged$GSP_ID), ]

# Guard: GSP_ID must be unique (one footprint per plan) after the merge.
dups <- merged$GSP_ID[duplicated(merged$GSP_ID)]
if (length(dups)) {
  stop("Duplicate GSP_ID after merge: ", paste(unique(dups), collapse = ", "))
}

# --- 4. Write the canonical shapefile --------------------------------------
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
st_write(merged, OUT_SHP, delete_dsn = TRUE, quiet = TRUE)
cat(sprintf("\nWrote %d feature(s) -> %s\n", nrow(merged), OUT_SHP))
cat(sprintf("  from i03: %d   backfilled from archive: %d\n",
            sum(merged$src == "i03"), sum(merged$src == "archive_2023")))
cat("Done.\n")
