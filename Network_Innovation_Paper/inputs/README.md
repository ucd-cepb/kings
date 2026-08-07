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
| `gsp_covariates.csv` | Per-plan political/economic covariates (`Republican_Vote_Share`, `Agr_Share_Of_GDP`, `exante_collab`, `mult_gsas`, `priority_category`, `gwsum`), keyed on 4-digit `gsp_id`. Consumed by `modeling/*`. | Deduplicated per `gsp_id` from the covariate columns of the legacy `data/Multipurpose_Files/gsp_docs_w_meta`. 119 rows. |
| `gsp_page_sections.RDS` | Page-level section-classification flags (`admin`, `basin_plan`, `sust_criteria`, `monitoring_networks`, `projects_mgmt_actions`, `is_comment`, `is_reference`) keyed on `gsp_id`+`page_num`. Consumed by `text_preprocessing/preprocess_portal_texts.R`. | The page-section columns of `data/Multipurpose_Files/gsp_docs_w_meta`. These flags are not in core. 165,844 rows. |
| `gsp_basin_ids.csv` | `gsp_id` → `basin_id` map. Consumed by `modeling/*` and `text_reuse/*`. | Copied verbatim from `EJ_DAC_Paper/Data/gsp_basin_ids.csv` (snapshot; the paper no longer reads the sibling path). |
| `node_dictionary_seed.csv` | Frozen prior hand-coded entity→type labels. Used **only** as few-shot examples by `Code/classify_entities.R` so the classifier prompt is stable across rebuilds. | Snapshot of the pre-refactor `data_products/node_dictionary.csv`. |
| `GSP_Submitted/` | GSP boundary shapefile (`SubmittedGSP_Master.*`) for spatial adjacency. Consumed by `modeling/*` and `text_reuse/map_similarity*`. **git-ignored** (5.5 MB binary). | Copied from `data/Multipurpose_Files/GSP_Submitted/`. |

## Re-staging

If you need to rebuild the derived inputs, the covariate and page-section
tables come from the legacy `gsp_docs_w_meta` object:

```r
library(data.table)
meta <- as.data.table(readRDS("data/Multipurpose_Files/gsp_docs_w_meta"))
meta[, gsp_id := formatC(as.integer(gsp_id), width = 4, flag = "0")]
fwrite(unique(meta[, .(gsp_id, Republican_Vote_Share, Agr_Share_Of_GDP,
                       exante_collab, mult_gsas, priority_category, gwsum)], by = "gsp_id"),
       "Network_Innovation_Paper/inputs/gsp_covariates.csv")
saveRDS(meta[, .(gsp_id, page_num, admin, basin_plan, sust_criteria,
                 monitoring_networks, projects_mgmt_actions, is_comment, is_reference)],
        "Network_Innovation_Paper/inputs/gsp_page_sections.RDS")
```
