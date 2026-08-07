# Network Innovation Paper

Governance-network analysis of California Groundwater Sustainability Plans (GSPs),
ending in valued/binary ERGM models of plan-to-plan similarity.

This paper proceeds from the **canonical pipeline outputs** in `data/core_data/`
(produced by `core_code`) plus a small set of **paper-owned inputs** in
`inputs/`. It **does not** re-run any core document→graph work, and it reads
nothing from sibling paper directories, `data/Multipurpose_Files`, or foreign
repos.

## Contract: what the paper depends on

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
   Code/{reference_extraction, text_reuse, knowledge_tree,      ┌─► data_products/{gsp_reference_pairs,
         tag_consultants, tag_preparers}  ──────────────────────┤     triple_similarity, project_jaccard, …}
                                                                ▼
                                          Code/modeling/make_networks.R (+ valued / binary0.9)  ─► ERGMs
```

### `Code/00_ingest_core.R` — the only bridge to core

Starts from the disambiguated core objects (it reads **no** raw docs) and writes
three bridge files into `data_products/`:

- **`id_crosswalk.csv`** — `gspDocId` ↔ legacy `gsp_id`/`version`, taken directly
  from the plan-family manifest, plus `plan_section`/`submitted_date` and a
  derived `doc_rank` (1 = a plan's earliest submission). `doc_rank` — not the
  manifest `version` field — is how the paper selects one document per plan
  (`select_plan_docs()` / `NIP_DOC_SELECT` in `_corpus.R`).
- **`node_dictionary.csv`** — every unique entity name → one of 22 semantic types
  (`Local_GSA`, `Company`, `NGO`, `Group`, `District`, …). Core carries only
  spaCy NER tags, so these semantic labels are regenerated with an LLM classifier
  (`Code/classify_entities.R`, few-shot-seeded from the prior hand labels in
  `inputs/node_dictionary_seed.csv`). Cached, so re-runs only classify new names.
  Requires an Anthropic API key (env `ANTHROPIC_API_KEY`, or the file the
  classifier points at).
- **`all_gsa_edges.csv`** — per-plan agency × connected-entity mention-weight
  matrix, built from `core_data/igraph_objects/uniplex_weighted_graphs` by
  folding edges onto their `Local_GSA` endpoint. Consumed by `modeling/*`.

Run from the repo root:

```sh
Rscript Network_Innovation_Paper/Code/00_ingest_core.R      # CLOBBER=TRUE to rebuild
```

## `Code/` layout

| Dir | Purpose |
|---|---|
| `_paths.R` | Single source of truth for all paths. |
| `_corpus.R` | Core clean-text corpus reader + id-crosswalk helpers (used by the text-reuse feeders). |
| `00_ingest_core.R` | Core → paper bridge (above). |
| `classify_entities.R` | LLM entity-type classifier (used by the ingest). |
| `reference_extraction/` | Extract & match plan bibliographic references. |
| `text_preprocessing/` | Build `page_metadata.RDS` from the core clean-text corpus (first step of the page-similarity rebuild). |
| `text_reuse/` | Page/section text-similarity + spatial adjacency. |
| `knowledge_tree/` | Knowledge-triple extraction & similarity. |
| `tag_consultants/` | Consulting-firm tagging. |
| `tag_preparers/` | Preparer-entity tagging. |
| `modeling/` | Build networks & fit ERGMs (analysis endpoint). |

## Regenerating the page-similarity products from current core

The text-reuse feeders read the **core clean-text corpus** — one parquet per
plan document under `core_txt_clean()`, named by the numeric `gspDocId` stem,
with columns `page`/`text` (the legacy `<<PAGE_BREAK>>`-delimited
`v*_gsp_num_id_*.txt` layout is gone). `Code/_corpus.R` centralizes the parquet
read and the `gspDocId → gsp_id/version` recovery via `id_crosswalk.csv`.
Page-entity keys throughout this chain are `<gspDocId>_<page_num>`. To rebuild
from scratch, run in order (from the repo root):

```sh
Rscript Network_Innovation_Paper/Code/text_preprocessing/preprocess_portal_texts.R  # -> page_metadata.RDS
Rscript Network_Innovation_Paper/Code/text_reuse/hash_and_compare_pages.R           # -> score_results/portal_page_scores_<date>.rds
# map_similarity.R / map_similarity_total.R / link_page_lda_results_to_meta.R read the newest score file
```

`latest_page_scores()` always picks the **newest** `portal_page_scores_*.rds`, so
run `hash_and_compare_pages.R` before the map/link scripts on any rebuild —
otherwise they consume a stale score file whose legacy `v*_gsp_num_id_*.txt`
keys won't resolve against the `gspDocId` crosswalk.

## Notes

- `filekey.csv` (repo root) is **not** read by this paper; paths come from
  `_paths.R`.
