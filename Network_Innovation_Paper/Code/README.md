# `Code/` — script-level map

This is a **per-file guide** to the paper's code. For the higher-level picture —
the core→paper contract, the `00_ingest_core.R` bridge, the `_paths.R` path
system, and the end-to-end run order — see
[`../README.md`](../README.md). This file does not repeat that; it lists what
each script does and the order to run scripts *within* a subdirectory.

All scripts are run from the **repo root** (paths are written relative to it),
e.g. `Rscript Network_Innovation_Paper/Code/<path>.R`.

## Top-level files

| File | Role |
|---|---|
| `run_all.R` | Orchestrator — regenerates the page-similarity products from current core (ingest → preprocess → hash; optional maps). Resolves every path via `_paths.R` and runs each stage in its own Rscript process. `RUN_INGEST=1` / `RUN_MAPS=1` opt into the gated stages. |
| `_paths.R` | Path resolver — every location (`core_*()`, `nip_input()`, `nip_product()`, `core_txt_clean()`, `stem_from_filename()`). Sourced by everything; hardcode no paths. |
| `_corpus.R` | Core clean-text corpus reader + id helpers: `read_core_corpus()` (one parquet per `gspDocId`, cols `page`/`text`), `load_id_crosswalk()` (`gspDocId → gsp_id`/`version`/`doc_rank`), `select_plan_docs()` (one document per plan; `NIP_DOC_SELECT`=`original`|`latest`), `latest_page_scores()`. Sourced by the text-reuse feeders. |
| `00_ingest_core.R` | The only bridge to core; writes `data_products/{id_crosswalk,node_dictionary,all_gsa_edges}`. Detailed in the primary README. Invokes the entity classifier in `01_entity_classification/`. |
| `_entity_groups.R` | Single source of truth for entity-type groupings over the six-leaf vocabulary. Shared: sourced by `01_entity_classification/classify_entities.R` **and** all three `04_modeling/*` scripts, so it stays at root. |

## `01_entity_classification/`

The entity-type tagging subsystem, invoked *by* `00_ingest_core.R` (not a separate
downstream stage). Labels entity names into the six-leaf controlled vocabulary
(GSA / Consultant / Research / NGO / Institutional_other / Non_institutional). Full
subsystem doc: [`ENTITY_TAGGING.md`](01_entity_classification/ENTITY_TAGGING.md).

- `classify_entities.R` — LLM entity-type classifier (Claude API), sourced by
  `00_ingest_core.R`. Deterministic gazetteer → cache → LLM precedence; needs an
  Anthropic key. The grouping vocabulary lives in the root `_entity_groups.R`.
- `build_overrides_from_dicts.R` — bakes the `core_code/dicts` gazetteers into
  `inputs/entity_type_overrides.csv`, the authoritative label source that wins over
  both cache and LLM. Run standalone: `Rscript Network_Innovation_Paper/Code/01_entity_classification/build_overrides_from_dicts.R`.
- `eval_classifier.R` — no-gold hand spot-check: samples up to `NIP_EVAL_PER_CAT`
  names per predicted leaf for a human to eyeball. Run standalone: `NIP_EVAL_PER_CAT=40 Rscript Network_Innovation_Paper/Code/01_entity_classification/eval_classifier.R`.

## `02_text_preprocessing/`

First step of the page-similarity rebuild.

- `preprocess_portal_texts.R` — reads the core clean-text corpus via `_corpus.R`,
  applies the paper's page filter (`cleanText`: short-page + all-caps + stricter
  numeric/whitespace, on top of core's `step2` punctuation cleaning), joins the id
  crosswalk + the core page-section flags (`core_page_sections()`), and writes
  `data_products/page_metadata.RDS` (the input to the whole `03B_text_reuse/` chain).

## `03B_text_reuse/`

Page- and section-level similarity between plans, plus the spatial maps. Consumes
`page_metadata.RDS`. Score outputs land in `data_products/score_results/`.

Rebuild order:

1. `hash_and_compare_pages.R` — minhash + LSH over all pages → `score_results/portal_page_scores_<date>.rds`. Memory-heavy.
2. `map_similarity.R`, `map_similarity_total.R` — build the plan-to-plan networks from the newest page-score file and draw the spatial similarity maps. Vertices are one document per plan via `select_plan_docs()` (default `original`; set `NIP_DOC_SELECT=latest` to use resubmitted docs).
3. `link_page_lda_results_to_meta.R` — join page scores back onto section metadata.

Related, run independently of the page chain:

- `compare_project_sections.R` — restricts the same idea to the *projects & management actions* pages (one concatenated doc per plan; Jaccard 10-grams) → `project_section_jaccard_scores_<date>.rds`, a `04_modeling/*` input. (The older `hash_and_compare_projects.R` variant is retired — see `unused/`.)

Section-level and "total"-map exploration live in `exploratory/` (below).

## `03A_reference_extraction/`

Bibliographic reference extraction and matching. The file-level numeric prefixes are the run order:

1. `01_extract_GSP_references.R` — extract reference strings from the plan PDFs (`referenceExtract`; depends on `anystyle`/ruby — see the notes in the script header).
2. `02_aggregate_references.R` — classify & aggregate the extracted references (`referenceClassify`).
3. `03_query_titles_in_openalex.R` — query candidate titles against OpenAlex.
4. `04_search_OA_titlematch_index.R` — build/search the title-match index → `gsp_solr_OA_matches.rds`.
- `reference_set_similarity.R` — turn matched reference sets into plan-to-plan reference-overlap similarity (a modeling input).

## `03C_knowledge_tree/`

Subject–predicate–object ("knowledge triple") extraction and similarity.

- `extract_knowledge_triples.R` — assemble triples from page metadata.
- `spo_extraction.py` / `spo_extraction*.ipynb` — the SPO extraction experiments (Python).
- `semantic_kg_similarity.ipynb` — knowledge-graph similarity scoring.

> Note: these scripts predate the core-parquet refactor and still reference the
> older `page_metadata.csv` / `^v1`-style layout; adapt paths before re-running.

## `exploratory/`

One-off / interactive analysis scripts that read the current products but aren't
in the pipeline (no `run_all.R` stage, no downstream consumer). See
[`exploratory/README.md`](exploratory/README.md): `compare_sections.R`,
`analyze_dissimilarity.R`, `map_similarity_total.R`.

## `unused/`

Deprecated / dead-end scripts, retired from the pipeline (nothing runs, sources,
or reads their outputs). See [`unused/README.md`](unused/README.md). Currently:
the old heuristic `tag_consultants/` (superseded by the `classify_entities.R`
semantic types) and `tag_preparers/` notebook, plus
`hash_and_compare_projects.R` (superseded by `compare_project_sections.R`).

## `04_modeling/`

Analysis endpoint — assemble plan-to-plan networks from the upstream similarity
products plus paper-owned covariates (`nip_input('gsp_covariates.csv')`), then fit
ERGMs.

- `make_networks.R` — base network assembly.
- `make_valued_networks.R` — valued-ERGM variant.
- `make_binary0.9_networks.R` — binary variant thresholded at the 0.9 cut.
