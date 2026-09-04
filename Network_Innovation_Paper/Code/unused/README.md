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
which the modeling scripts read via `_entity_groups.R` (`FOCAL_TYPES`). None of
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

## Not moved here (still live, even if `run_all.R` doesn't run them)

For reference — these are **not** dead ends and stay in place:

- `reference_extraction/*`, `knowledge_tree/*` — run by hand, but they produce
  `gsp_reference_pairs.rds` / `triple_similarity.csv`, which `modeling/*` reads.
- `text_reuse/compare_project_sections.R` — feeds the project Jaccard modeling input.
- `text_reuse/map_similarity.R`, `link_page_lda_results_to_meta.R` — wired as
  `run_all.R`'s optional stage 3.
