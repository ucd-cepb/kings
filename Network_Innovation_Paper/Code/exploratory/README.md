# `exploratory/` — one-off / interactive analysis scripts

These read the **current** corpus and score products but are **not part of the
pipeline**: `run_all.R` doesn't run them and nothing downstream (`modeling/*`,
the maps wired into `run_all`) consumes their output. They're kept as live
interactive tools for ad-hoc exploration — distinct from `unused/`, which holds
retired/superseded code.

- `compare_sections.R` — Jaccard dissimilarity between document sections defined
  by the metadata section flags → `score_results/section_jaccard_dissimilarity_<date>.rds`.
- `analyze_dissimilarity.R` — post-hoc exploration of a section-dissimilarity
  result set produced by `compare_sections.R` (its own outputs feed nothing
  further).
- `map_similarity_total.R` — the "total" variant of `text_reuse/map_similarity.R`:
  builds plan-to-plan networks from the newest page-score file and draws spatial
  similarity maps. Persists no product. (The `original`/`latest` `map_similarity.R`
  stays in `text_reuse/` because `run_all.R` wires it as optional stage 3.)

Run from the repo root, e.g. `Rscript Network_Innovation_Paper/Code/exploratory/compare_sections.R`.
