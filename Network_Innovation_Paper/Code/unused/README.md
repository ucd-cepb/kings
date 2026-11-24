# `unused/` — deprecated / dead-end scripts

Code that is **no longer part of any pipeline**: nothing in `run_all.R` invokes
it, no other script `source()`s it, and no downstream script reads its outputs.
Kept here (rather than deleted) for provenance and in case a piece is worth
reviving. Nothing outside this directory should depend on anything in it.

## What's here and why it's retired

### `tag_consultants/` — superseded by the LLM entity classifier
Heuristic, hand-curated consulting/law-firm tagging (hard-coded firm lists,
keyword regex like `consulting|consultants|associates|engineering`). The job of
identifying consultants now flows through the semantic classifier:
`classify_entities.R` assigns the `Consultant` type in `node_dictionary.csv`,
which reaches the models through the bag layer (`gsa_bag_edges_alters.csv`,
grouped by `_entity_groups.R`). None of
the `consulting_*` CSVs these scripts write are read by anything.

- `consultant_extract.R`, `consultant_extract_simple.R`, `consultant_extract_raw.R`

### `tag_preparers/` — orphaned notebook
- `extract_participating_entities.ipynb` — a preparer/participating-entity
  notebook with no `.R` driver; not referenced by any script and no consumer of
  its output. The equivalent semantic types (`Consultant`/`Research`/`NGO`/…)
  now come from the classifier.

### `hash_and_compare_projects.R` — superseded by `compare_project_sections.R`
Writes `project_page_scores_<date>.rds`, which **no script reads**. The
projects/management-actions similarity that actually feeds `modeling/*` is
`project_section_jaccard_scores.rds`, produced by
`text_reuse/compare_project_sections.R` (still live). This file is the older
"same idea" variant that lost out.

### `entity_curation/make_alter_overrides.R` — one-off, applied (retired 2026-11-24)
Generated the first NGO/research rows of `inputs/entity_bag_overrides_{ngo,research}.csv`
from specs inside the script. Every family it lists is applied, and the files are now
curated by hand with dated notes. **Do not re-run it:** it rewrites every row's `note`
to one generic stamp, and it cites BAG_REVIEW_FINDINGS sections (§7d/§10d) that no
longer exist. Its `_paths.R` source path also no longer resolves from here.

### `modeling_explore/` — superseded valued-ERGM variants (retired 2026-11-24)
- `make_networks.R` (contrastive-divergence fit), `make_valued_networks.R` — earlier
  copies of `04_modeling/make_binary0.9_networks.R` that had drifted from it. They
  printed console tables and wrote `04_modeling/rds_placeholders/*_object.rds`, which
  nothing read (deleted). Kept in sync by hand until retirement; they will not track
  later bag or grouping changes.

## Not moved here (still live, even if `run_all.R` doesn't run them)

For reference — these are **not** dead ends and stay in place:

- `reference_extraction/*`, `knowledge_tree/*` — run by hand, but they produce
  `gsp_reference_pairs.rds` / `triple_similarity.csv`, which `modeling/*` reads.
- `text_reuse/compare_project_sections.R` — feeds the project Jaccard modeling input.
- `text_reuse/map_similarity.R`, `link_page_lda_results_to_meta.R` — wired as
  `run_all.R`'s optional stage 3.
