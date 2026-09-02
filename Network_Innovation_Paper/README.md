# Network Innovation Paper

Governance-network analysis of California Groundwater Sustainability Plans (GSPs),
ending in valued/binary ERGM models of plan-to-plan similarity.

This paper proceeds from the **canonical pipeline outputs** in `data/core_data/`
(produced by `core_code`) plus a small set of **paper-owned inputs** in
`inputs/`. It **does not** re-run any core document→graph work, and it reads
nothing from sibling paper directories, `data/Multipurpose_Files`, or foreign
repos.

## Root data:

1. `data/core_data/` — canonical pipeline outputs (disambiguated textnet objects,
   igraph objects, the plan-family manifest, `sgma_gsa_full.csv`).
2. `core_code/dicts/` — canonical NER dictionaries (used by the pipeline; these
   are *not* semantic entity-type labels).
3. `inputs/` — curated, non-core inputs owned by this paper (see `inputs/README.md`).

Every path is resolved through `Code/_paths.R` — no script hardcodes a location.
The core-facing helpers (`core_disambig()`, `core_igraph_weighted()`,
`core_manifest()`, …) are the *only* contract with core; `nip_input()` and
`nip_product()` resolve paper-local files.

## Flow

```
data/core_data ─┐
                ├─►  Code/00_ingest_core.R  ─►  data_products/{id_crosswalk, node_dictionary, all_gsa_edges}
inputs/ ────────┘                                        │
                                                         ▼
   Code/{03A_reference_extraction, 03B_text_reuse,              ┌─► data_products/{gsp_reference_pairs,
         03C_knowledge_tree}  ──────────────────────────────────┤     triple_similarity, project_jaccard, …}
                                                                ▼
                                     Code/04_modeling/make_networks.R (+ valued / binary0.9)  ─► ERGMs
```

### `Code/00_ingest_core.R` — the only bridge to core

Starts from the disambiguated core objects (it reads **no** raw docs) and writes
three bridge files into `data_products/`:

- **`id_crosswalk.csv`** — `gspDocId` ↔ legacy `gsp_id`/`version`, taken directly
  from the plan-family manifest, plus `plan_section`/`submitted_date` and a
  derived `doc_rank` (1 = a plan's earliest submission). `doc_rank` — not the
  manifest `version` field — is how the paper selects one document per plan
  (`select_plan_docs()` / `NIP_DOC_SELECT` in `_corpus.R`). A plan's multiple
  documents are chronological *resubmissions* of the whole plan (`plan_section` is
  `original` vs. `resubmitted`), **not** volumes/parts of one plan — so selecting
  by `doc_rank` chooses one complete submission (earliest by default), never one
  half of a split plan. 79 plans were submitted once; 53 have an original and a
  resubmission. A submission is never spread across multiple documents: every
  `gsp_id` in `id_crosswalk.csv` has exactly one or two rows, and every two-row
  plan is one `original` + one `resubmitted` (distinct `plan_section` *and*
  `submitted_date`) — no plan carries two documents of the same section, so
  choosing one `doc_rank` selects a complete submission with all its content,
  never one chapter of a split plan.

- **`node_dictionary.csv`** — every unique entity name → one of the six-leaf
  controlled vocabulary (`GSA`, `Consultant`, `Research`, `NGO`,
  `Institutional_other`, `Non_institutional`). Core carries only spaCy NER tags,
  so these semantic labels are regenerated with an LLM classifier
  (`Code/01_entity_classification/classify_entities.R`, few-shot-prompted from curated in-code exemplars and
  pinned by the core-dicts gazetteer). Cached, so re-runs only classify new names.
  Requires an Anthropic API key (env `ANTHROPIC_API_KEY`, or the file the
  classifier points at). See [`Code/01_entity_classification/ENTITY_TAGGING.md`](Code/01_entity_classification/ENTITY_TAGGING.md)
  for the vocabulary and the gazetteer → cache → LLM resolution.


- **`all_gsa_edges.csv`** — per-plan agency × connected-entity mention-weight
  matrix, built from `core_data/igraph_objects/uniplex_weighted_graphs` by
  folding edges onto their `Local_GSA` endpoint. Consumed by `modeling/*`.

Run from the repo root:

```sh
Rscript Network_Innovation_Paper/Code/00_ingest_core.R      # CLOBBER=TRUE to rebuild
```

## `Code/` layout

Directory names carry their **stage number as a prefix**, so the on-disk order
*is* the run order. A shared letter (`03A`/`03B`/`03C`) marks chains that run **in
parallel** — they are mutually independent and all feed `04_modeling/`. The
top-level helper/bridge scripts (`_paths.R`, `_corpus.R`, `00_ingest_core.R`) are
unprefixed files, not stages of the page pipeline.

| Dir / file | Stage | Purpose |
|---|---|---|
| `_paths.R`, `_corpus.R` | — | Helpers: the path resolver and the core clean-text corpus + id-crosswalk readers. Sourced everywhere; no run order. |
| `00_ingest_core.R` | 00 | Core → paper bridge (above). Runs `01_entity_classification/classify_entities.R` internally. |
| `01_entity_classification/` | 01 | Entity-tagging subsystem (`classify_entities.R` + `build_overrides_from_dicts.R` + `eval_classifier.R`) — **invoked by** `00_ingest_core.R` (the stage-00 bridge), not run as a standalone downstream stage. See [`Code/01_entity_classification/ENTITY_TAGGING.md`](Code/01_entity_classification/ENTITY_TAGGING.md) for the whole tagging subsystem. |
| `02_text_preprocessing/` | 02 | Stage-2 build of `page_metadata.RDS` — **not** obviated by core. Core supplies pre-split clean-text parquet, but this still applies the paper's own page filter (`cleanText`: short-page + all-caps + stricter numeric/whitespace; core's `step2` already handles total-punctuation), recovers legacy `gsp_id`/`version` via `id_crosswalk.csv`, and joins the core page-section flags (`core_page_sections()`). Prerequisite of 03B and 03C. |
| `03A_reference_extraction/` | 03A | Extract & match plan bibliographic references (independent of 02). |
| `03B_text_reuse/` | 03B | Page/section text-similarity + spatial adjacency (consumes 02). |
| `03C_knowledge_tree/` | 03C | Knowledge-triple extraction & similarity (consumes 02). |
| `04_modeling/` | 04 | Build networks & fit ERGMs (analysis endpoint). Consumes the 03 chains. |
| `exploratory/` | — | One-off / interactive analysis scripts, not in the pipeline (see `Code/exploratory/README.md`). |
| `unused/` | — | Deprecated / dead-end scripts, retired from the pipeline (see `Code/unused/README.md`). |

## Regenerating the page-similarity products from current core

The text-reuse feeders read the **core clean-text corpus** — one parquet per
plan document under `core_txt_clean()`, named by the numeric `gspDocId` stem,
with columns `page`/`text` (the legacy `<<PAGE_BREAK>>`-delimited
`v*_gsp_num_id_*.txt` layout is gone). `Code/_corpus.R` centralizes the parquet
read and the `gspDocId → gsp_id/version` recovery via `id_crosswalk.csv`.
Page-entity keys throughout this chain are `<gspDocId>_<page_num>`. To rebuild
from scratch, run in order (from the repo root):

```sh
Rscript Network_Innovation_Paper/Code/02_text_preprocessing/preprocess_portal_texts.R  # -> page_metadata.RDS
Rscript Network_Innovation_Paper/Code/03B_text_reuse/hash_and_compare_pages.R          # -> score_results/portal_page_scores_<date>.rds
# map_similarity.R / map_similarity_total.R / link_page_lda_results_to_meta.R read the newest score file
```

`latest_page_scores()` always picks the **newest** `portal_page_scores_*.rds`, so
run `hash_and_compare_pages.R` before the map/link scripts on any rebuild —
otherwise they consume a stale score file whose legacy `v*_gsp_num_id_*.txt`
keys won't resolve against the `gspDocId` crosswalk.

COMMENT: IF THERE ARE LEGACY RESULTS NOT BASED ON TEH CURRENT CORE DATA, DO A ONE TIME DELETE OF THOSE TO MAKE SURE THEY GET REPLACED. UPDATE THIS TEXT ABOVE ACCORDINGLY TO REFLECT THE NEW PROCESS AND DON'T NEED TO REFERENCE THE OLD ONE.

## Notes

- `filekey.csv` (repo root) is **not** read by this paper; paths come from
  `_paths.R`.
