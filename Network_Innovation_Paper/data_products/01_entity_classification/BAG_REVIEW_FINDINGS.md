# GSA entity bags — design and open items

Entity names from the NER pipeline are fragmented across many spellings of the same
agency (`aliso_gsa`, `aliso_water_district_gsa`, `aliso_wd_gsa`, …). A **bag** folds
all spellings of one real entity onto a single canonical id so each agency is counted
once. This file states the rules the bag system follows and lists what still needs
doing.

---

## 1. Rules

**Authorship, not mention, defines network membership.** A document's authors are the
GSA ids DWR lists for it in `id_crosswalk.csv` (`gsa_ids`, comma-separated). Resolve ids
to names with the core roster `data/core_data/metadata/sgma_gsa_full.csv`
(`GSA_ID`→`GSA_Name`). Do **not** use the crosswalk's `gsa_names` column — it is
display-only and not row-aligned with `gsa_ids`. A resolved GSA row is kept only if its
`bag_id` is one of that document's `gsa_ids`; anything else is a mention and is dropped.
259 ids are affiliated with at least one plan.

**Key on `gsp_doc_id`, never on `gsp_id`, `plan_family_id`, or `version`.** A plan can
have several documents (2020 original, 2022/23 resubmission, later consolidation) whose
author teams differ, so the author set belongs to the document. See §4.

**Do not merge one roster id into another.** `inputs/bag_merges.csv` is empty and stays
empty. A name variant folds to exactly one roster id (its bag); ids are never rewritten
to each other. Merging would drop real authors, because a document that lists the member
id but not the parent would fail the authorship test.

**Multi-management-area agencies: keep each area's id distinct.** About 41 organizations
operate more than one management-area GSA that share a base name and differ only by a
" – <Management Area>" suffix (Turner Island {220,223}; Sacramento County {295,496,497};
Tehama {124,418–423}; Imperial ×13; Salinas ×6; Fox Canyon ×4; …). A variant that names
such an organization is a pointer into the family, not a single id. At build time it
resolves per document to whichever family member(s) that document lists as authors; if a
document lists more than one, keep all of them; if it lists none, the variant is a
mention and is dropped.

**Groups with no roster id stay as identity, which makes them non-authors.** Umbrellas,
coalitions, and multi-agency plan areas have no single `GSA_ID`, never appear in any
document's `gsa_ids`, and so can never be authors. Their coordination is represented
through their member GSAs, which avoids double-counting. Current cases: Kern Groundwater
Authority; South of Kern River; Salinas Valley umbrella (`svbgsa` + `salinas_valley_*`);
North Delta; McFarland; Pauma Valley; the Colusa + Glenn two-authority JPA.

---

## 2. How bags are built

- `build_entity_bag_map.R` → `entity_bag_map.csv` maps each variant to a bag. Tiers:
  `exact` (variant equals a roster name), `subset` (the roster name's distinctive tokens
  all appear in the variant), `revsubset` (the variant's distinctive tokens are all
  within one roster name), then `fuzzy` (reviewed, not auto-applied). Only unambiguous
  `subset`/`revsubset` matches auto-fold; ambiguous ones go to a review file. The
  stoplist drops only SGMA boilerplate, so "county"/"city"/"water"/"district" stay and
  keep sibling agencies apart.
- Curated overrides win over the matcher and are the authoritative record of reviewed
  decisions: `inputs/entity_bag_overrides_gsa.csv` (214 rows),
  `inputs/entity_bag_overrides_consultant.csv` (295), `inputs/entity_bag_overrides_ngo.csv`
  (108), `inputs/entity_bag_overrides_research.csv` (135). A variant with no match
  defaults to its own identity bag.

**Needed matcher guard (not yet added).** `revsubset` should not auto-fold when the
variant has a single distinctive token and that token is a direction or feature word
(south/north/east/west/upper/lower/central/river/valley/basin/county/mid) or is shared by
more than one roster base name. It should go to review instead. This is the root cause of
every item in §5.

---

## 3. State

- **GSA (ego) side: done.** Reviewed folds are in the 214-row override; `bag_merges.csv`
  is retired. `build_gsa_bag_edges.R` → `gsa_bag_edges.csv` (Step 2) is built: it folds
  authoring GSA rows, keeps only ids in each document's `gsa_ids`, expands multi-area
  pointers per document, pools each bag's co-mentions across the corpus, and keys on
  `gsp_doc_id` + `plan_family_id` (445 rows, 182 documents, 118 plans, 254 authoring
  bags). Two of 120 plans drop under strict authorship — plan 43 (doc 3712, empty
  `gsa_ids`) and plan 52 (doc 1683, whose sole author gsa_106 has no folding variant).
  Plan 43's cause is **fixed as of 2026-10-11** (§7 author override: `gsa_ids = 205`,
  Eastern Tule GSA); re-run `build_gsa_bag_edges.R` to pick it up — the 445/182/118/254
  counts above predate the override.
- **One open GSA correction:** §5 (bare-token false folds), not yet applied.
- **Alter side: folded and reviewed, not wired into the network build**, with open
  items: §6.
- **Integration:** §7.

---

## 4. Crosswalk data quality

**The `version` column is not a submission key.** It labels both the January-2020
original GSP and its 2022/23 resubmission `version = 1`; 53 `(gsp_id, version)` pairs each
cover more than one submission date. Order documents by `doc_rank` (or `submitted_date`),
and join authorship on `gsp_doc_id`. `gsp_id` can also be reassigned across resubmissions
(4 canonical plans span more than one: 7→{0007,0174}, 12→{0012,0160}, 50→{0050,0164–0166},
150→{0150,0167–0173}). Counts: 185 documents, 132 `gsp_id`s, 120 canonical plans; of the
canonical plans, 67 have one submission date, 49 have two, 4 have three.

Example — canonical plan 7:

| gsp_doc_id | gsp_id | submitted | doc_rank | version | authors |
|---|---|---|---|---|---|
| 944 | 0007 | 2020-01-17 | 1 | 1 | 1 (Aliso WD GSA) |
| 8770 | 0007 | 2022-07-20 | 2 | 1 | 1 (Aliso WD GSA) |
| 11932 | 0174 | 2026 | 3 | 2 | 23 (Delta-Mendota consolidated) |

### Three review dictionaries
1. **Variant → canonical GSA bag:** `01_entity_classification/GSA_BAGS_review.txt`.
2. **Document → canonical GSA authors** (the DWR scrape, at document grain):
   `00_ingest/DOC_AUTHORS_review.txt`. Authors are `gsa_ids` resolved through the core
   roster; 17 documents are flagged where the `gsa_names` column disagrees with the roster.
3. **Document → authoring GSA bags** (author side as `bag_id`s, the Step-2 join input):
   `01_entity_classification/DOC_AUTHOR_BAGS_review.txt` plus the long-form
   `doc_canonical_gsa_bags.csv` (450 doc×author rows). Two affiliated labels are shared
   across ids and kept as distinct bags: Siskiyou FC&WCD {253,456,457}, Tehama FC&WCD
   {124,418,419,420,422}.

The generators for #2 and #3 currently exist only in a session scratchpad and must be
committed to `Code/01_entity_classification/` (or folded into Step 2) to be regenerable.

---

## 5. Open GSA correction — bare-token and geographic false folds

These are auto-matcher folds (not override rows), so fixing them adds identity override
rows or the §2 matcher guard; the 214-row override is untouched. Targets verified against
the core roster and the crosswalk.

### 5a. Kern River GSA (gsa_20) is absorbing three unrelated things
`revsubset` pins a bare token to the affiliated agency with the fewest distinctive tokens,
and Kern River = {kern, river} wins because the Kern Groundwater Authority umbrella (often
written "Kern GSA") has no roster id.

| variant | reason | fix |
|---|---|---|
| `south_of_kern_river_gsa`, `each_south_of_kern_river_gsa`, `south_of_kern_river_groundwater_sustainability_agencies_boards` | the South of Kern River plan group = Arvin (gsa_481), Wheeler Ridge-Maricopa (gsa_484), Tejon-Castac (gsa_482), authoring documents 9056/8969; gsa_20 does not author those documents | identity (no-roster group, §1) |
| `kern_gsa` | bare "kern" is shared by gsa_39/306/508/517/518/523 and the Kern Groundwater Authority umbrella | identity |
| `river_gsa` | bare "river" is within 9 affiliated "river" agencies | identity |
| `kern_river` | the river itself (typed `Non_institutional`) | identity |

### 5b. Same pattern elsewhere
- gsa_314 East Kaweah: `east_gsa`, `gsa_east` — bare "east", within 8 agencies.
- gsa_219 Merced Subbasin: `merced_gsa`, `of_merced_gsa`,
  `merced_groundwater_sustainability_agencies` — bare "merced", shared with County of
  Merced (191/231), Turner Island-Merced (223), Merced Irrigation-Urban (311).
- gsa_7 Mid-Kaweah: `groundwater_sustainability_agency_mid` — bare "mid" fragment.

Bare tokens that are unique to one agency (aliso, yucaipa, mcmullin, olcese, napa, vista…)
are safe and stay folded.

### 5c. Worth a look
- gsa_69 County of Madera: `madera_county_east_gsa` and `triangle_t_water_madera_county_west_gsa`
  (the second also names Triangle T Water District, gsa_394). County of Madera has
  management-area ids; confirm the correct id per §1 rather than pinning both to gsa_69.
- gsa_266 McMullin Area: `mcmullin_area_gsa_east_gsa` — a run-on; low risk.

---

## 6. Alter side — consultants, NGOs, research

The alter side folds the co-mentioned entities that a plan's authoring GSAs tie to. Each
type folds in its own namespace so one type's network never merges into another's.
Producers: `build_alter_bag_map.R` → `alter_bag_map.csv` (+ cluster audit and per-type
review files); `build_entity_bag_lookup.R` → `entity_bag_lookup.csv` (the single join
table, 13,833 variants → 12,088 bags).

| type | namespace | authority | variants → bags |
|---|---|---|---|
| Consultant | `con_*` | `consultant_dictionary.csv`, then self-cluster | 1017 → 397 |
| NGO | `ngo_*` | self-cluster (no roster) | 823 → 574 |
| Research | `res_*` | self-cluster (no roster) | 460 → 288 |
| Institutional_other | `io_*` | identity only (deferred) | 10099 → 10099 |

**Folder rules the matcher applies.** `tokset` requires a contiguous block rotation, so a
harmless "X of Y" / "Y X" inversion folds but an interior swap does not (Cal Poly San Luis
Obispo and Cal Poly Pomona stay separate). Only a pure-digit token, a comment-matrix row
id, or a digit run trailing a ≥4-letter word is stripped, so load-bearing digits survive
(`2ndnature`, `4creeks`, the whole CH2M Hill family on one bag). Entity-naming words
(foundation/fund/institute/center/program/council/division/extension/laboratory/national)
are not generic `contains` extras — which keeps Clean Water Action, the UC system, and the
ITRC center/institute/program apart. `contains` requires a contiguous run and refuses a
bare generic parent. Roster tiers are stemmed on both sides (so "GEI Consultants" anchors
to roster "GEI Consultant, Inc."), and a `.DEBRIS` allowlist lets `contains` fold a mention
wrapped in GSP boilerplate (`sustainable` is excluded — it is part of real names). An
acronym tier fires only when exactly one expansion exists, with three guards (a rival bag
that spells the initialism vetoes it; more than one matching expansion vetoes it;
`.ACRO_BLOCK` covers `lsu`); a refused initialism goes to review as `acronym_ambiguous`.
`bag_label` is a function of `bag_id` (asserted); `method` is the tier joining the variant
directly to its bag representative, with a `joined` column (anchor/direct/transitive/
singleton).

Curated overrides seed the clustering and are read first (consultant 295, NGO 108,
research 135 rows). Policy: a hosted center/lab folds into its host campus; UC Cooperative
Extension / ANR is a separate statewide actor; bare "University of California" is the
system; named local chapters (county Farm Bureaus, named Audubon and Sierra Club chapters,
county Leagues of Women Voters) stay separate. `make_alter_overrides.R` regenerates the
NGO and research specs — it unions with what is on disk and refuses to shrink a file.
Deliberately left unfolded as genuinely ambiguous: `con_gsi` (two firms), `con_bgc`,
`res_csu`, `res_lsu`, `res_scripps`, `res_irrigation_technology_and_research_center`
(Fresno State CIT vs Cal Poly ITRC), `ngo_basin_pumpers_association`, RCAP vs RCAC,
Putah Creek Council vs Putah Creek Streamkeeper.

**Not wired in.** `build_gsa_bag_edges.R` still emits raw node names as alter columns and
`_entity_groups.R::entity_names()` still groups alters by raw name. Folding alters means
rewiring both to group by `bag_id` and `bag_type`.

Review menu counts: Consultant 322, NGO 378, Research 306.

### 6a. Must fix before using the alter side
- **`entity_bag_lookup.csv` does not carry the signed-off GSA overrides.** 92 of 186
  joinable rows disagree with `entity_bag_overrides_gsa.csv` (lookup holds the bare
  identity string, override holds the roster bag), and 496 GSA lookup rows have an
  un-namespaced `bag_id`. Fix: make `build_entity_bag_lookup.R` apply the GSA override
  itself, with the same precedence `build_gsa_bag_edges.R` uses. `gsa_bag_edges.csv` is
  not affected because it applies the override at read time.
- **`plan_family_id` in `gsa_bag_edges.csv` is a bare integer**, not 4-digit
  zero-padded, so it fails to join `modeling_plan_selection()`, `gsp_covariates.csv`, and
  the shapefile on all 120 plans. `gsp_doc_id` joins fine. Fix: zero-pad.

### 6b. Scope gap
`Institutional_unresolved` (2,848 variants, 17% of the institutional population) is in no
namespace and has no review file. It needs its own namespace, an explicit drop decision,
or a second classifier pass.

### 6c. Compound mentions
`alter_bag_compounds.csv` (231 rows) lists mentions that name two organizations
(`woodard_curran_and_davids_engineering`, `clean_water_action_california_waterfowl_association*`).
The 1:1 variant → bag schema cannot carry two bags, so the second organization is recorded
here rather than dropped. Nothing consumes it yet; wiring it into the edge builder would
recover co-authorship ties that are currently lost. The `roster_subset` second-firm guard
only sees roster firms, so a non-roster second organization (Davids Engineering, Holland &
Knight) is captured only here.

---

## 7. Integration

- **Wiring.** `run_all.R` registers none of the bag scripts. Stage 1 still runs the
  superseded `build_gsa_edges.R`, and `04_modeling/make_binary0.9_networks.R:34` still
  reads `all_gsa_edges.csv`. Moving the model to `gsa_bag_edges.csv` (keyed on
  `gsp_doc_id`) replaces the id-to-doc melt at lines 34–40 and retires `build_gsa_edges.R`.
  In the same commit, update `Code/01_entity_classification/ENTITY_TAGGING.md` — its Files
  table lists `build_gsa_edges.R` + `all_gsa_edges.csv` as the current Stage 1c, which
  becomes obsolete the instant the model switches. The bag scripts and `run_all.R`'s run
  check are existence-based, so a changed input behind an existing output is missed; add an
  mtime or input-hash check.

- **Non-actor filter (replaces `prevalence_max`).** The prevalence filter is gone. On the
  118-plan modeling frame the old 0.10 cut fell at 12 plans and deleted the focal actors
  the fold was built to surface (TNC, LCJA, CWC, GEI, LSCE, EKI, Self-Help). Ubiquity is
  not disqualifying — DWR is in 115/118 plans because it administers SGMA, and it stays.
  Not being an actor is disqualifying — "California" co-occurs with every plan by
  construction. So `NON_ACTOR_ENTITIES` in `_entity_groups.R` drops bare state/nation
  placenames by name, in the raw and `io_` spellings (`io_california`, `io_calif`,
  `io_united_states`, `io_united_states_of_america`, `io_us`). Scope is state/nation only —
  counties stay (shared county naming is real co-location, and counties are often
  governmental actors). `build_shared_entity_matrix()` keeps `prevalence_max` as an opt-in
  argument defaulted to `NA` (off) for reproducibility.

- **Binarized incidence.** `build_shared_entity_matrix()` reduces the plan × entity
  incidence to mentioned/not before `tcrossprod()`, so a cell counts shared entities. The
  edge weights are not co-occurrence counts: per `core_code/step5_build_igraphs.R`, a
  uniplex `weight` is the number of parsed source→target relations between two entities in
  a document, so the weighted form carried verbosity (how much DWR boilerplate a plan
  contains) and coalition size (a large joint GSP scoring high on breadth alone) into the
  coefficients. Binary is the direct operationalization — the relational event was observed
  or it was not — and is also the least DWR-concentrated reduction available (DWR share of
  the institutional covariate: raw 55.4%, binary 41.3%; every intensity-preserving
  alternative is higher). Dyad counts are unchanged; only values change. `binarize = FALSE`
  restores the weighted form.

- **Direction discarded.** The construct is an observed relational event between two
  entities, so which end was the grammatical subject does not bear on it; the symmetric
  fold (`build_gsa_edges.R:55-58`, `build_gsa_bag_edges.R:118-119`) is the intended shape.
  It is recoverable later from the directed core uniplex graphs if a paper wants it, but
  only as two additional direction-preserving edgecovs — not a transform of the symmetric
  plan × plan matrix.

- **GSA self-loops.** The `rbind` fold stores `from == to` edges at 2× (3,048 of 62,471
  GSA-touching triples). Immaterial: predictors are between-document values, so self-loops
  sit only on the plan × plan diagonal, which every caller zeroes. `build_gsa_bag_edges.R`
  already drops self-loops (`[alter != v]`), so Step 3 needs no change.

- **`mult_gsa` is an integer count.** `make_binary0.9_networks.R`, `explore/make_networks.R`,
  and `explore/make_valued_networks.R` now return `length(ids[nzchar(ids)])` and use
  `nodecov('mult_gsa')` in place of `nodefactor('mult_gsa')` (27 call sites). It is a
  partial control while the covariate is built from raw names (`build_gsa_edges.R` counts
  every GSA-typed vertex, not just authors) and an apt one after the bag rewire.
  **Blocker:** `gsp_doc_id` 3712 (canonical plan 43, both documents) has empty `gsa_ids` —
  the only two such rows in the crosswalk — so `mult_gsa = 0`, which is impossible (every
  plan has ≥1 GSA). The old binary coding hid it as `FALSE`; `nodecov()` takes the 0
  literally. This is the same gap §3 records as dropping plan 43 under strict authorship,
  reached from the modeling side.

  **It is a scrape gap, not an extraction gap, and the author is identifiable — recommended
  fix = backfill `gsa_ids = 205`** (Eastern Tule GSA; `gsa_names` is empty too, so the
  roster and the text are the only evidence). Doc 3712 holds 3,892 SVO triples over 2,650
  edges and 33 GSA-typed vertices, so its agencies are richly present in the parse; what is
  missing is DWR's record of *which* of them authored it. Four independent lines agree:
  1. `eastern_tule_groundwater_sustainability_agency` carries **528 triples**, >10× the next
     GSA vertex (Lower Tule 47, Pixley 35, Alpaugh 33) — the self-reference signature of an
     authoring agency in its own plan; the `eastern_tule_gsa` spelling adds 26 more.
  2. Eastern Tule GSA is roster **GSA_ID 205**, in this document's basin (5-022.13 TULE).
  3. **gsa_205 authors zero documents anywhere in the crosswalk** — a real Tule GSA that
     never appears as an author, which is exactly the shape of a dropped scrape value.
  4. The Tule subbasin has six plans and they close one-to-one: 48→25 Alpaugh, 56→42 Lower
     Tule, 57→490 Tri-County, 63→29 Delano-Earlimart, 65→43 Pixley, and **43→(none)**.
     Eastern Tule is the one agency left over, matched to the one plan left over.
  So plan 43 is a single-author Eastern Tule plan: `mult_gsa = 1`, and it stops being a
  strict-authorship dropout. ⚠️ Beware the id collision when writing this: **canonical plan
  43 is Eastern Tule's plan, while GSA_ID 43 is Pixley ID GSA**, which authors plan 65.
  Fix belongs upstream in `00_ingest` as a documented author override (§4 already flags 17
  further documents whose `gsa_names` disagrees with the roster, so the mechanism serves
  both).

  **✅ APPLIED 2026-10-11.** A fifth and more direct line of evidence turned up in
  `plan_family_manifest.csv`, which the crosswalk does not carry: DWR's own `localId` for
  both of plan 43's documents is **"Basin 5-022.13 - ETGSA GSP"** — Eastern Tule GSA named
  in the plan title. The same file shows `gsa_ids`/`gsa_names` blank upstream as well, so
  the gap is DWR's and not an ingest loss; a re-scrape would not close it.

  Wired as `inputs/doc_author_overrides.csv` + `apply_author_overrides()` in
  `Code/00_ingest_core.R`: **fill-only** (stops rather than overwrite a value DWR
  publishes), **roster-checked** (ids *and* names must agree with `core_gsa_full()` —
  this guard catches the 43/205 collision above; verified it fires), and it stamps a new
  `gsa_ids_source` column into `id_crosswalk.csv` (`scrape` 183 / `override` 2 /
  `missing` 0). Crosswalk rebuilt: exactly 2 rows changed. `mult_gsa` over the 118-plan
  frame now runs `min = 1` (was 0), the 1-bucket 79→80, mean 2.237→2.246; no other value
  moved. Documented in the primary README and `inputs/README.md`.

---

## 8. Upstream defects (fix outside the bag system)

Folding cannot repair these; tickets belong to `02_text_preprocessing`, the extractor, or
the classifier.
- **Glossary acronym expansion corrupts text inside words** (22+ names), e.g.
  `california_davids_engineeringpartment_of_water_resources` ("DE" → Davids Engineering).
  It also misclassifies DWR, Kaweah Delta WCD, and Northern Delta as Consultant.
- **DWR comment-matrix codes are read as organizations:** `clean_water_ngo016`–`ngo027`
  are comment ids, and the same artifact appends codes to real names (a 24-variant bag of
  `clean_water_action_california_waterfowl_association{1..30}`).
- **The type classifier is not invariant to citation digits or the `_x` placeholder**, so
  one organization lands in two namespaces (`land_iq` vs `land_iq_1`; 135 keys differ only
  by a trailing `_x` or digits). Fix: normalize before typing, or majority-vote the type
  over the normalized key.
- **`_x` is an extraction placeholder, not a name token** (80 variants). Strip at
  tokenization.
- **Compound mentions discard a co-author's tie:** `woodard_curran_and_davids_engineering`
  → `con_woodard_curran` drops Davids Engineering. A guard that emits a tie to both bags
  adds ties only (no precision cost) and is the one rule here that changes network
  structure.
