# `inputs/` — paper-owned, non-core inputs

These artifacts are **not** produced by `core_code` and are **not** in
`data/core_data`. They are curated inputs owned by this paper. Code under
`Code/` reads them only through the `nip_input()` helper in `Code/_paths.R`;
nothing here is read from a sibling paper directory at run time.

Each was staged once from the source below. The staging reads are the *only*
place `data/Multipurpose_Files` / sibling-paper data is touched — the paper's
own scripts never do.

| File | What it is | Provenance (one-time staging) |
|---|---|---|
| `gsp_covariates.csv` | Per-plan political/economic covariates keyed on 4-digit `gsp_id` (the canonical/original plan id, ≤0156). `modeling/*` consumes `Republican_Vote_Share`, `Agr_Share_Of_GDP`, and `exante_collab` (joined by `canonical_gsp_id`). The file also carries `mult_gsas`, `priority_category`, and `gwsum` from its original staging, but those are **no longer consumed**: `mult_gsa` now derives from `sgma_gsa_full.csv` (GSA-keyed) and `priority` from `sgma_basin_full.csv` (basin-keyed). | Deduplicated per `gsp_id` from the covariate columns of the legacy `data/Multipurpose_Files/gsp_docs_w_meta`. 119 rows. |
| `entity_type_overrides.csv` | The deterministic entity→type gazetteer (exact + regex rows) — the authoritative label source, applied over both the cache and the LLM. See [`Code/01_entity_classification/ENTITY_TAGGING.md`](../Code/01_entity_classification/ENTITY_TAGGING.md). | Baked from `core_code/dicts/*` by `Code/01_entity_classification/build_overrides_from_dicts.R`, plus hand-curated Consultant/Research/NGO rows. |

**Retired inputs** (removed 2026-09-10):

- `gsp_basin_ids.csv` — a `gsp_id` → `basin_id` map. It was read but never used by any active or exploratory script, so it was deleted. Basin identity now comes from the crosswalk `basin` field / `sgma_basin_full.csv`.
- `GSP_Submitted/` — GSP boundary shapefile. Now core spatial metadata: `data/core_data/spatial/gsp_boundaries.shp` (filekey `gsp_boundaries_core`), read via `core_gsp_boundaries()`. The old local copy was a stale 2023 vintage (max GSP.ID 0156) missing the newer plans, which crashed `modeling/*`. Rebuild the core file with `Rscript core_code/metadata_generators/build_gsp_boundaries.R`.

## Re-staging

If you need to rebuild `gsp_covariates.csv`, it comes from the legacy
`gsp_docs_w_meta` object:

```r
library(data.table)
meta <- as.data.table(readRDS("data/Multipurpose_Files/gsp_docs_w_meta"))
meta[, gsp_id := formatC(as.integer(gsp_id), width = 4, flag = "0")]
fwrite(unique(meta[, .(gsp_id, Republican_Vote_Share, Agr_Share_Of_GDP,
                       exante_collab, mult_gsas, priority_category, gwsum)], by = "gsp_id"),
       "Network_Innovation_Paper/inputs/gsp_covariates.csv")
```

The page-level section flags are **no longer a paper input** — they are a shared,
cross-paper dataset and now live in core as
`data/core_data/metadata/gsp_page_sections.csv` (read via `core_page_sections()`).
See `data/core_data/core_data_README` for its staging snippet.
