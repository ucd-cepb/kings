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
| `run_all.R` | Orchestrator — regenerates the modeling inputs from current core (optional bridge + entity classification → preprocess → the three Stage 3 similarity products: 3A references, 3B project-Jaccard, 3C knowledge-tree). Every stage is an independent TRUE/FALSE toggle; 3A + 3C default OFF because they are intensive and need tools beyond R (anystyle/ruby + OpenAlex + Solr for 3A; a `jupyter` env for 3C). Builds only what a `04_modeling/*` script actually reads; skips the exploratory page-score branch (`03B_text_reuse/explore/` maps), which feeds nothing downstream. Resolves every path via `_paths.R`, runs each R stage in its own Rscript process, and the 3C similarity step via `jupyter nbconvert`. `RUN_INGEST=1` opts into the gated bridge + entity rebuild. |
| `_paths.R` | Path resolver — every location (`core_*()`, `nip_input()`, `nip_product()`, `core_txt_clean()`, `stem_from_filename()`). Sourced by everything; hardcode no paths. |
| `_corpus.R` | Core clean-text corpus reader + id helpers: `read_core_corpus()` (one parquet per `gspDocId`, cols `page`/`text`), `load_id_crosswalk()` (`gspDocId → gsp_id`/`version`/`doc_rank`), `select_plan_docs()` (one document per plan; `NIP_DOC_SELECT`=`original`|`latest`), `latest_page_scores()`. Sourced by the text-reuse feeders. |
| `00_ingest_core.R` | The only *ingest* bridge to core; writes `data_products/id_crosswalk.csv` straight from the manifest (no derivation, no LLM). Detailed in the primary README. The entity products (`node_dictionary`, `all_gsa_edges`) are in-project analysis and live in `01_entity_classification/`. |
| `_entity_groups.R` | Single source of truth for entity-type groupings over the six-leaf vocabulary. Shared: sourced by `01_entity_classification/classify_entities.R` **and** all three `04_modeling/*` scripts, so it stays at root. |

## `01_entity_classification/`

The paper's own entity-classification stage (Stage 1) — in-project analysis, **not**
ingest: core carries spaCy NER tags only, so the paper regenerates its six-leaf
semantic vocabulary here (GSA / Consultant / Research / NGO / Institutional_other /
Non_institutional) and derives the GSA edge product from it. Full subsystem doc:
[`ENTITY_TAGGING.md`](01_entity_classification/ENTITY_TAGGING.md). Run order within
the stage: `build_overrides_from_dicts.R` → `build_node_dictionary.R` → `build_gsa_edges.R`.

- `build_overrides_from_dicts.R` — bakes the `core_code/dicts` gazetteers into
  `inputs/entity_type_overrides.csv`, the authoritative label source that wins over
  both cache and LLM. Run FIRST. Run standalone: `Rscript Network_Innovation_Paper/Code/01_entity_classification/build_overrides_from_dicts.R`.
- `classify_entities.R` — LLM entity-type classifier (Claude API), sourced by
  `build_node_dictionary.R`. Deterministic gazetteer → cache → LLM precedence; needs
  an Anthropic key. The grouping vocabulary lives in the root `_entity_groups.R`.
- `build_node_dictionary.R` — collects the unique entity names from the core disambig
  objects and runs the (cached) classifier → `data_products/node_dictionary.csv`. The
  one step that hits the API. Run standalone: `CLOBBER=TRUE Rscript Network_Innovation_Paper/Code/01_entity_classification/build_node_dictionary.R`.
- `build_gsa_edges.R` — folds the core weighted graphs down to the GSA-typed entities
  (read from `node_dictionary.csv`) → `data_products/all_gsa_edges.csv`. Run AFTER the
  node dictionary. Run standalone: `CLOBBER=TRUE Rscript Network_Innovation_Paper/Code/01_entity_classification/build_gsa_edges.R`.
- `eval_classifier.R` — no-gold hand spot-check: samples up to `NIP_EVAL_PER_CAT`
  names per predicted leaf for a human to eyeball. Run standalone: `NIP_EVAL_PER_CAT=40 Rscript Network_Innovation_Paper/Code/01_entity_classification/eval_classifier.R`.

## `02_text_preprocessing/`

First step of the page-similarity rebuild.

- `additional_filter_texts.R` — reads the core clean-text corpus via `_corpus.R`,
  applies the paper's page filter (`cleanText`: short-page + all-caps + stricter
  numeric/whitespace, on top of core's `step2` punctuation cleaning), joins the id
  crosswalk + the core page-section flags (`core_page_sections()`), and writes
  `data_products/page_metadata.RDS` (the input to the whole `03B_text_reuse/` chain).

## `03B_text_reuse/`

Page- and section-level similarity between plans, plus the spatial maps. Consumes
`page_metadata.RDS`.

**Run by `run_all.R`** — the only 03B script whose output a `04_modeling/*` script
reads:

- `compare_project_sections.R` — Jaccard 10-gram similarity over the *projects &
  management actions* pages (one concatenated doc per plan) →
  `project_jaccard_results/project_section_jaccard_scores.rds`, the 03B
  modeling input. Reads `page_metadata.RDS` directly, independent of the page-score
  branch below. (The older `hash_and_compare_projects.R` variant is retired — see
  `unused/`.)

**Exploratory page-score branch** (in `explore/`; NOT run by `run_all.R`; feeds
nothing downstream) — run by hand only if you want the raw page scores or the maps:

1. `explore/hash_and_compare_pages.R` — minhash + LSH over all pages → `score_results/portal_page_scores_<date>.rds`. Memory-heavy.
2. `explore/map_similarity.R` — builds the plan-to-plan network from the newest page-score file and draws the spatial similarity map (persists no product). Vertices are one document per plan via `select_plan_docs()` (default `original`; set `NIP_DOC_SELECT=latest` to use resubmitted docs).
3. `explore/link_page_lda_results_to_meta.R` — join page scores back onto section metadata (in memory; persists no product).

The "total"-map variant `map_similarity_total.R` lives in `exploratory/` (below).

## `03A_reference_extraction/`

Bibliographic reference extraction and matching. **Stage 3A of `run_all.R`**
(`STAGE_3A_REFERENCES`, default OFF): the five scripts run in numeric order,
ending in the model input `gsp_reference_pairs.rds`. External deps: `anystyle`/ruby
(step 01), the OpenAlex API (step 03), a Solr title-match index (step 04). The
file-level numeric prefixes are the run order:

1. `01_extract_GSP_references.R` — extract reference strings from the plan PDFs (`referenceExtract`; depends on `anystyle`/ruby — see the notes in the script header).
2. `02_aggregate_references.R` — classify & aggregate the extracted references (`referenceClassify`).
3. `03_query_titles_in_openalex.R` — query candidate titles against OpenAlex.
4. `04_search_OA_titlematch_index.R` — build/search the title-match index → `gsp_solr_OA_matches.rds`.
- `reference_set_similarity.R` — turn matched reference sets into plan-to-plan reference-overlap similarity (a modeling input).

## `03C_knowledge_tree/`

Subject–predicate–object ("knowledge triple") extraction and similarity. **Stage 3C
of `run_all.R`** (`STAGE_3C_KNOWLEDGE`, default OFF): the two-step canonical chain
below produces the model input `triple_similarity.csv`. Needs a Python/Jupyter env
on PATH (`jupyter nbconvert`, plus sentence-transformers + networkx for the notebook).

- `extract_knowledge_triples.R` — assemble SPO triples from the CORE dependency
  parses (`parsed_plans/parsed_<stem>.parquet`) over the **sust-criteria** pages of
  the original document per plan → `knowledge_triples_sustcrit.csv` (`file` = 4-digit
  gsp_id). Migrated off the pre-refactor `data/Innovation_Paper/` + `page_metadata.csv`
  + `^v1` layout. Extracts triples with `textNet::textnet_extract()` (its edgelist
  `source`/`head_verb_lemma`/`target` = subject/predicate/object; `keep_incomplete_edges=FALSE`
  drops edges missing a subject or object), replacing the pre-refactor
  `extract_advanced_triples_from_df` / `clean_triples` pair that this repo never defined.
- `semantic_kg_similarity.ipynb` — embeds those triples and scores plan-to-plan
  similarity → `triple_similarity.csv` (the 3C model input). Run by `run_all.R` via
  `jupyter nbconvert`; its `../../data_products/` paths already target modern layout.
- `spo_extraction.py` / `spo_extraction*.ipynb` — alternative REBEL/Triplex SPO
  extraction experiments (Python); NOT on the `run_all.R` path.

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

- `make_binary0.9_networks.R` — the paper's model: binarizes each similarity
  network at its 0.9-quantile cut and fits Bayesian ERGMs (`bergm`), writing the
  manuscript figures (`figure1_dv_distributions.png`, `model[12]_plot.png`) and
  the regression tables (`mod0/1/2/3_html.html`).

**Exploratory variants** (in `explore/`; off the critical path) — earlier
valued-ERGM approaches, superseded by the binary/bergm model above; run by hand
only, they persist no manuscript output:

- `explore/make_networks.R` — valued-ERGM variant fit with contrastive
  divergence (`estimate = 'CD'`); prints console tables and saves intermediate
  `*_object.rds` placeholders.
- `explore/make_valued_networks.R` — valued-ERGM variant.
