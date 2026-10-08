# GSA entity bags — design and open items

Entity names from the NER pipeline are fragmented across many spellings of the same
agency (`aliso_gsa`, `aliso_water_district_gsa`, `aliso_wd_gsa`, …). A **bag** folds
all spellings of one real entity onto a single canonical id so each agency is counted
once. This file states the rules the bag system follows and lists what still needs
doing. It is not a change log.

---

## 1. Rules

**Authorship, not mention, defines network membership.** A document's authors are the
GSA ids DWR lists for it in `id_crosswalk.csv` (`gsa_ids`, comma-separated). Resolve ids
to names with the core roster `data/core_data/metadata/sgma_gsa_full.csv`
(`GSA_ID`→`GSA_Name`). Do **not** use the crosswalk's `gsa_names` column — it is
display-only and not row-aligned with `gsa_ids`. A resolved GSA row is kept only if its
`bag_id` is one of that document's `gsa_ids`; anything else is a mention and is dropped.
259 ids are affiliated with at least one plan.

**Key on `gsp_doc_id`, never on `gsp_id`, `canonical_gsp_id`, or `version`.** A plan can
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
  `inputs/entity_bag_overrides_consultant.csv` (295 rows). A variant with no match
  defaults to its own identity bag.

**Needed matcher guard (not yet added).** `revsubset` should not auto-fold when the
variant has a single distinctive token and that token is a direction or feature word
(south/north/east/west/upper/lower/central/river/valley/basin/county/mid) or is shared by
more than one roster base name. It should go to review instead. This is the root cause of
every item in §7.

---

## 3. State

- **GSA (ego) side: done.** Reviewed folds are in the 214-row override; `bag_merges.csv`
  is retired. `build_gsa_bag_edges.R` → `gsa_bag_edges.csv` (Step 2) is built: it folds
  authoring GSA rows, keeps only ids in each document's `gsa_ids`, expands multi-area
  pointers per document, pools each bag's co-mentions across the corpus, and keys on
  `gsp_doc_id` + `canonical_gsp_id`.
- **One open GSA correction:** §6 (bare-token false folds), not yet applied.
- **Alter side: built, not wired into the network build**, with open bugs: §7.
- **Integration gaps:** §8.

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
   `doc_canonical_gsa_bags.csv`. Two affiliated labels are shared across ids and kept as
   distinct bags: Siskiyou FC&WCD {253,456,457}, Tehama FC&WCD {124,418,419,420,422}.

The generators for #2 and #3 currently exist only in a session scratchpad and must be
committed to `Code/01_entity_classification/` (or folded into Step 2) to be regenerable.

---

## 5. (reserved)

---

## 6. Open GSA correction — bare-token and geographic false folds

These are auto-matcher folds (not override rows), so fixing them adds identity override
rows or the §2 matcher guard; the 214-row override is untouched. Targets verified against
the core roster and the crosswalk.

### 6a. Kern River GSA (gsa_20) is absorbing three unrelated things
`revsubset` pins a bare token to the affiliated agency with the fewest distinctive tokens,
and Kern River = {kern, river} wins because the Kern Groundwater Authority umbrella (often
written "Kern GSA") has no roster id.

| variant | reason | fix |
|---|---|---|
| `south_of_kern_river_gsa`, `each_south_of_kern_river_gsa`, `south_of_kern_river_groundwater_sustainability_agencies_boards` | the South of Kern River plan group = Arvin (gsa_481), Wheeler Ridge-Maricopa (gsa_484), Tejon-Castac (gsa_482), authoring documents 9056/8969; gsa_20 does not author those documents | identity (no-roster group, §1) |
| `kern_gsa` | bare "kern" is shared by gsa_39/306/508/517/518/523 and the Kern Groundwater Authority umbrella | identity |
| `river_gsa` | bare "river" is within 9 affiliated "river" agencies | identity |
| `kern_river` | the river itself (typed `Non_institutional`) | identity |

### 6b. Same pattern elsewhere
- gsa_314 East Kaweah: `east_gsa`, `gsa_east` — bare "east", within 8 agencies.
- gsa_219 Merced Subbasin: `merced_gsa`, `of_merced_gsa`,
  `merced_groundwater_sustainability_agencies` — bare "merced", shared with County of
  Merced (191/231), Turner Island-Merced (223), Merced Irrigation-Urban (311).
- gsa_7 Mid-Kaweah: `groundwater_sustainability_agency_mid` — bare "mid" fragment.

Bare tokens that are unique to one agency (aliso, yucaipa, mcmullin, olcese, napa, vista…)
are safe and stay folded.

### 6c. Worth a look
- gsa_69 County of Madera: `madera_county_east_gsa` and `triangle_t_water_madera_county_west_gsa`
  (the second also names Triangle T Water District, gsa_394). County of Madera has
  management-area ids; confirm the correct id per §1 rather than pinning both to gsa_69.
- gsa_266 McMullin Area: `mcmullin_area_gsa_east_gsa` — a run-on; low risk.

---

## 7. Alter side — consultants, NGOs, research

The alter side folds the co-mentioned entities that a plan's authoring GSAs tie to. Each
type folds in its own namespace so one type's network never merges into another's.
Producers: `build_alter_bag_map.R` → `alter_bag_map.csv` (+ cluster audit and per-type
review files); `build_entity_bag_lookup.R` → `entity_bag_lookup.csv` (the single join
table).

| type | namespace | authority | variants → bags |
|---|---|---|---|
| Consultant | `con_*` | `consultant_dictionary.csv`, then self-cluster | 1017 → 397 |
| NGO | `ngo_*` | self-cluster (no roster) | 823 → 574 |
| Research | `res_*` | self-cluster (no roster) | 460 → 288 |
| Institutional_other | `io_*` | identity only (deferred) | 10099 → 10099 |

**Not wired in.** `build_gsa_bag_edges.R` still emits raw node names as alter columns and
`_entity_groups.R::entity_names()` still groups alters by raw name. Folding alters means
rewiring both to group by `bag_id` and `bag_type`. Left until the alter folds are reviewed.

### 7a. Must fix before using the alter side
- **`entity_bag_lookup.csv` does not carry the signed-off GSA overrides.** 92 of 186
  joinable rows disagree with `entity_bag_overrides_gsa.csv` (lookup holds the bare
  identity string, override holds the roster bag), and 496 GSA lookup rows have an
  un-namespaced `bag_id`. Fix: make `build_entity_bag_lookup.R` apply the GSA override
  itself, with the same precedence `build_gsa_bag_edges.R` uses. `gsa_bag_edges.csv` is
  not affected because it applies the override at read time.
- **`canonical_gsp_id` in `gsa_bag_edges.csv` is a bare integer**, not 4-digit
  zero-padded, so it fails to join `modeling_plan_selection()`, `gsp_covariates.csv`, and
  the shapefile on all 120 plans. `gsp_doc_id` joins fine. Fix: zero-pad.

### 7b. Folder precision bugs (`build_alter_bag_map.R`)

**§7b, §7c and §7d are now FIXED for Consultant/NGO/Research — see §10.** They are
kept here as the record of what was wrong and why. §7a, §7e, §8 and §9 are still open.

1. The token-set tier ignores order, so `california_polytechnic_state_university` (Cal
   Poly SLO) and `california_state_polytechnic_university` (Pomona) share one bag. Gate
   only true reorderings — some "X of Y" ↔ "Y X" pairs are benign.
2. Digit stripping removes digits that are part of the name: `2ndnature`→`ndnature`,
   `4creeks`→`creeks`, `napa_vision_2050`→`napa_vision`; CH2M Hill splits across 5 bags.
   Fix: drop only all-digit tokens; strip a trailing digit run only when ≥4 letters remain.
3. A compound mention captured the base org: Clean Water Action (35 documents) sits in
   `ngo_action_clean_water_fund` while `ngo_clean_water_fund` exists separately.
4. A unit-form word acted as a generic extra and absorbed the UC system into a UC unit
   (`res_university_of_california_extension`). Same shape as the geographic-word problem.
5. `bag_label` is not a function of `bag_id` for 2 consultant bags, which puts
   `con_provost_pritchard_consulting_group` on two rows and emits a self-merge in the
   review menu.
6. `method` is wrong on 79 of 607 folded rows (it reports the strongest tier on any
   incident edge, not the fold's own tier). Do not filter on it as a safety signal.
7. The namespace assertion in `build_entity_bag_lookup.R` only catches a `bag_id` shared
   across two types; it never checks that a `bag_id` sits in its own type's namespace.
8. `bag_type` is not derivable from the `bag_id` prefix for the GSA namespace (496 rows
   disagree, and 6 real GSA identity bags are named `gsa_<word>`). Always carry
   `bag_type`; never re-derive it from the prefix.

### 7c. Review menus miss the bags that matter
The review tiers are symmetric local tests, so the largest bags never appear: 0 of 469
consultant review rows touch the 105-variant LSCE bag, and 431 of 469 pair bags with ≤4
variants each. Main cause: `.bag_review` does not normalize `bag_label`, so a roster-style
label like "CH2M Hill" is one token and unreachable by the `contains` tier. Fix: normalize
the label inside `.bag_review`. Separately, never use a bare generic token
(`ngo_farm_bureau`, `ngo_audubon`, `ngo_sierra_club`) as a fold parent — it would merge
independent county organizations.

Current review counts: Consultant 469 (344 contains / 125 fuzzy), NGO 600 (345/255),
Research 406 (205/201).

### 7d. Recall gaps (optional, measured)
- Roster tiers are not plural-stemmed: the roster says "GEI Consultant, Inc." but every
  mention says "GEI Consultants", so GEI gets no anchor and splits into 25 bags. Stemming
  both sides of the roster match rescues 27 variants at a precision cost of 1.
- An acronym tier that fires only when exactly one expansion exists gains ~11 consultant
  firms, 22 NGO pairs (including `ngo_wspa`, 385 mentions), and 27 research pairs.
- Normalizing `counsel` = `council` merges the misspelled "Leadership Counsel for Justice
  and Accountability" correctly, with 2 collision groups, both correct.

### 7e. Scope gap
`Institutional_unresolved` (2,848 variants, 17% of the institutional population) is in no
namespace and has no review file. It needs its own namespace, an explicit drop decision,
or a second classifier pass.

---

## 8. Integration

- `run_all.R` registers none of the bag scripts. Stage 1 still runs the superseded
  `build_gsa_edges.R`, and `04_modeling/make_binary0.9_networks.R:34` still reads
  `all_gsa_edges.csv`. Moving the model to `gsa_bag_edges.csv` (keyed on `gsp_doc_id`)
  replaces the id-to-doc melt at lines 34–40.
- Each bag script skips when its output exists, and `run_all.R`'s run check is also
  existence-based, so a changed input behind an existing output is missed. Add an mtime or
  input-hash check.
- **`prevalence_max` RESOLVED 2026-10-08 (user decision): the prevalence filter is
  GONE. Non-actor entities are excluded by identity instead.**

  The modeling frame is **118 plans** (post `modeling_plan_selection()` and the
  `0053`/`0089` drop), not the 182 documents the integration audit used, so the old
  0.10 cut fell at 12 plans. On bags it was deleting seven focal entities in
  [0.10, 0.40): TNC 39, LCJA 24, CWC 23, GEI 14, LSCE 14, EKI 13, Self-Help 12 — i.e.
  exactly the actors the fold was built to surface. 0.40 was tried and rejected: it is
  a **no-op for every focal group** (nothing reaches 48 plans), so its only live effect
  was on the institutional network, where prevalence conflates two different objections.

  The replacement distinguishes them. **Ubiquity is not disqualifying**: DWR is in
  115/118 plans *because it administers SGMA*, which is substantive, and it stays in.
  **Not being an actor is disqualifying**: "California" is not a participant in any
  plan — every GSP is in California, so co-mention is true by construction. So
  `NON_ACTOR_ENTITIES` in `_entity_groups.R` drops bare state/nation placenames by
  name, in both the raw and `io_` spellings (5 institutional bags: `io_california`,
  `io_calif`, `io_united_states`, `io_united_states_of_america`, `io_us`).
  `build_shared_entity_matrix()` keeps `prevalence_max` as an opt-in argument,
  defaulted to `NA` (off), only so the old behaviour stays reproducible.

  Scope decision: **state/nation only**. County names stay — two plans both naming Kern
  County share real co-location, and counties are often governmental actors here. The
  other bare-geography spellings need no entry (`america`, `usa`, `north_america`, `ca`,
  `u_s`, `cal`, `american`, `states` are typed `Non_institutional`; `state` is
  `Institutional_unresolved`), so no grouping admits them.

  Bag-folded result, nonzero dyads of 6,903 possible (old 0.10 → now):
  institutional 1,361 → **6,620** (95.9% dense), Consultant 236 → **484** (7.0%),
  NGO 110 → **1,126** (16.3%), crn 367 → **1,526** (22.1%), Research **35** (never
  bound at any threshold). These enter as **valued** `edgecov` — the 0.9 quantile
  thresholds only the dependent networks — so magnitude passes straight into the
  coefficients.

- **Incidence matrix now BINARIZED before the crossproduct, 2026-10-08 (user
  decision).** `build_shared_entity_matrix()` reduces the plan × entity incidence to
  mentioned/not before `tcrossprod()`, so a cell counts shared entities — which is
  what the function's own docstring always claimed, and what the valued `edgecov`
  should carry. Dyad **counts are unchanged** (a pair shares ≥1 entity or it does
  not): institutional 6,620, Consultant 484, Research 35, NGO 1,126, crn 1,526. Only
  the values change — institutional mass 1,868,447 → **16,313**, max dyad 121,308 →
  **39**; crn 32,529 → 2,041. `binarize = FALSE` restores the weighted form.

  This removes two artifacts that the weighted form was feeding into the coefficients:

  1. **Verbosity.** DWR's per-plan weight runs 1–74 across its 115 plans, so it
     carried **50.3%** of the institutional covariate: the covariate was substantially
     "how much DWR boilerplate does this plan contain." Binarized, DWR's dyad-mass
     contribution falls 940,506 → 6,555 and its share to **40.2%** — still the largest,
     irreducibly so, because an entity in 115/118 plans *is* in 95% of dyads. That
     residual is the substantive ubiquity the user chose to keep; the verbosity term on
     top of it is gone.
  2. **Coalition size** — and this, not self-mention, is what `gsa_146` was.

     **These weights are not co-occurrence counts.** `step5_build_igraphs.R:5-17`: the
     core multiplex graph holds **one edge per SVO triple** from the dependency parse;
     the uniplex graph collapses parallel edges so `weight` = the **number of parsed
     source→target relations** between two entities in that document. Summing an
     entity's column over a plan's agency rows gives total parsed relations between it
     and any of the plan's agencies — coherent, but scaling with coalition size.
     `gsa_146` (San Joaquin County GSA) held 6.1% off **two** plans through no artifact
     at all: doc 2509 is a 31-agency joint GSP with 709 triples on that vertex (408 as
     source, 337 as target) and 36–40 genuine asserted relations to each co-agency. An
     earlier draft of this note called it a roster/signature-table artifact and before
     that a self-mention artifact. **Both were wrong** — its self-mention share is only
     12% (the real self-loop cases are `gsa_124` 83%, `gsa_20` 57%) and its counterparty
     profile is clean inter-agency relational content. Binarized it is **0.01%**, which
     is the cost of binarizing, not a bug being fixed.

  **Why binary, and what it is not.** Binary is the direct operationalization of the
  construct — the relational event either was observed or was not — so intensity is out of
  scope by design, not traded away reluctantly. **Robustness answer if a reviewer presses
  on discarding counts:** every intensity-preserving alternative concentrates **more** on
  DWR, not less — DWR share of the institutional covariate: raw **55.4%**,
  binary **41.3%**, sqrt 64.3%, log1p 65.6%, row-normalized 70.5%, log1p+row-norm 73.8%.
  Dyad mass goes as `(Σw)² − Σw²`, which rewards **breadth**: compressing magnitudes strips
  the coalition spikes and leaves DWR's presence in 115/118 plans standing taller. Binary
  is also the least DWR-concentrated reduction available.

  **Scope of a 1, and agency breadth — RESOLVED as signal (2026-10-08, user).** Documents
  touching more GSA vertices register more distinct entities (**cor = 0.794**, spearman
  0.79, with the number of agency rows in `all_gsa_edges.csv`). **Retained deliberately —
  it reflects reality**: a large coalition genuinely does know more alters than a
  two-agency GSP, so the breadth difference is the phenomenon rather than noise over it.
  **Do not normalize it away per plan.** The control belongs in the models, via the
  existing `mult_gsa` term, now an integer count (see below).

  ⚠️ **CORRECTION (2026-10-08) to what this bullet said earlier.** It claimed a 1 means
  "a parsed relation from at least one of the plan's *authoring agencies*" and cited
  **cor 0.771 with author count**. Both were wrong, and in the same way: the quantity
  measured was **GSA rows in the edge file**, i.e. every GSA-typed vertex in the document,
  authors or not — `build_gsa_edges.R` does not restrict to the author set. Re-measured on
  the 118-plan frame (`scratchpad/multgsa.R`): cor with agency rows **0.794**, cor with the
  true **authoring** count from crosswalk `gsa_ids` only **0.243** (spearman 0.30); the two
  agency counts correlate 0.461. The tell was doc **3712** — cited before as "41 agencies →
  99 entities", it actually has **32 GSA rows, 99 entities, and 0 recorded authors**.
  So for the current raw-name pipeline a 1 means "a parsed relation from ≥1 GSA-typed
  vertex in the document". The narrower authoring reading becomes true only at **Step 3**:
  `build_gsa_bag_edges.R` restricts via `intersect(.family_of(X, fam), author_set)` and
  drops mention-only egos. Consequence: `nodecov('mult_gsa')` is a **partial** control
  while the covariate is built from raw names, and an apt one after the bag rewire.

  **`mult_gsa` is now an integer count (2026-10-08, user), in all three modeling scripts.**
  `make_binary0.9_networks.R` plus `explore/make_networks.R` and
  `explore/make_valued_networks.R`: the crosswalk fold returns
  `length(ids[nzchar(ids)])` as `integer(1)` instead of `> 1L`, and all 9 model
  specifications per file move from `nodefactor('mult_gsa')` to `nodecov('mult_gsa')`
  (27 call sites). Distribution over the frame: 0:1, 1:79, 2:16, 3:5, 4:4, 5:3, 6:2, 7:2,
  9:1, 10:2, 13:1, 16:1, 23:1 — mean 2.24, max 23. The old binary split was 80/38.
  No collinearity with the other collaboration term: cor(mult_gsa, joint_agency) = **0.113**
  (the old binary was 0.023), and `joint_agency = TRUE` plans still have median 1 author,
  so `exante_collab` is measuring something genuinely different. All three files parse.
  ⚠️ **BLOCKER for the final run: 1 plan in the frame scores 0 authors** — `gsp_doc_id`
  **3712** (canonical 43; *both* of its documents, and the only 2 empty `gsa_ids` rows in
  the whole crosswalk). Zero is not a real value — every plan has ≥1 GSA. The old binary
  coding hid this as `FALSE`, indistinguishable from a single-author plan; `nodecov()` will
  take the 0 literally. Needs a decision: backfill from `gsa_names`/the core roster, or
  drop the plan.

  The one thing that did have to go is **magnitude double-counting**, and binarizing
  eliminates it provably rather than approximately: `sum(value) > 0` is identical to
  `any(value > 0)`, so the binary cell is **invariant to how many agency rows a plan
  contributes** — an entity reached by 10 of a plan's agencies scores the same 1 as one
  reached by a single agency. Nothing is counted once per author; breadth is counted once
  per entity. (That invariance is also what makes step 4 safe *after* the dcast's
  `fun.aggregate = sum` — the sum's magnitude never survives it.)

- **The `build_gsa_edges.R:55-58` `rbind` fold — both orientations collapse into one
  `(gsa, connected_to)` cell. Examined 2026-10-08; BOTH consequences now closed.**
  1. **Direction is discarded — accepted, by decision (2026-10-08, user).** Not a defect.
     The construct is an **observed relational event** between two entities: a parsed
     source→target relation is more informative than seeing both entities somewhere in the
     same text, and *that* is the whole claim. Which end was the grammatical subject does
     not bear on it, so the symmetric fold is the right shape. The core uniplex graphs are
     directed and retain `from`/`to`, so this is recoverable if a later paper wants it —
     **do not "fix" the fold on direction grounds**, and the same symmetrization in
     `build_gsa_bag_edges.R:118-119` is likewise fine. Measurements of what is set aside
     are kept below for the reviewer who asks.
  2. **GSA self-loops are stored at 2×.** An edge with `from == to == X` matches both
     `rbind` branches and both land in cell `(X,X)` — verified 36→72, 9→18, 11→22.
     Corpus-wide, GSA self-loops are **3,048 of 62,471** GSA-touching triples (**4.9%**),
     all doubled. **Closed as immaterial (2026-10-08, user):** the predictors are
     between-document values, so self-loops are confined to the plan × plan diagonal,
     which every caller zeroes — the doubling never reaches a modeled dyad. Still worth
     fixing if the edge file is ever used for a within-document quantity.

  **What (1) sets aside, measured 2026-10-08** (`scratchpad/direction.R`, 118 docs, loops
  excluded) — recorded as evidence the decision was informed, not as an open item:
  the orientations are not interchangeable. **62.6%** of GSA-touching triple weight has the
  GSA as source, 37.4% as target, and the per-alter split tracks actor role rather than
  scattering around .5 — outbound: DWR **.74** (115 docs), USGS .64, SWRCB .63,
  Reclamation .61, CA Water Commission .60, CDFW .57; inbound: TNC **.37**,
  Community Water Center **.22**. Focal types are net inbound too: Consultant 42.5%
  outbound (527 triples / 86 docs), NGO 45.1% (630 / 79), Research 55.1% (69 / 30).
  Of 26,593 `(doc, ego, alter)` cells, **83.6% carry one orientation only** (57.1%
  outbound-only, 26.4% inbound-only) — for those the fold merges nothing, it just drops a
  known label — but they hold only 46.6% of the weight; the reciprocated **16.4%** hold
  53.4%, and half of those are near-balanced (|fw−bw|/(fw+bw) < .2), i.e. genuinely
  two-way relations. Consequence for the shared-entity covariate: a 1 means "a relation in
  *either* direction", and of institutional cells **43.7% exist only outbound, 28.9% only
  inbound**, so the matrix is the union of two fairly different networks. Were direction
  ever wanted it could not come from a transform of the plan × plan matrix (symmetric by
  construction) — it would be **two additional edgecovs** (both plans' agencies act on *e*;
  both are acted on by *e*) built from a direction-preserving fold, i.e. a modeling
  extension rather than a repair. Note also a **trust caveat** that would have to be
  settled first: the usable signal depends on how `textNet::textnet_extract` treats
  passive constructions ("the plan was submitted to DWR"); not verified here (the installed
  package is lazy-loaded), and it bears directly on the regulator out-shares above.

  The Step-3 replacement `build_gsa_bag_edges.R:118-119` **already avoids (2)** — it drops
  self-loops with `[alter != v]` — and symmetrizes like (1), which is now the intended
  behaviour, so **Step 3 needs no change on either count.** NB an exact reconciliation of
  `all_gsa_edges.csv` against the current core graphs is **not available**: the file was
  built 2026-09-03 against an older `node_dictionary.csv`, so its GSA type set differs
  (predicted 588 vs actual 638 for the cell above). The fold's behaviour was verified by
  running the fold logic on current graphs, which is staleness-independent.

  The binarized top contributors are also substantively the right ones — SWRCB 6.6%,
  CDFW 4.8%, TNC 4.5%, CA Water Commission 3.0%, Reclamation 2.5%, USGS 2.2%, LCJA 1.7%,
  CWC 1.6% — where weighted they were ~0.3% each and the top ten were eight GSA bags
  inflated by fan-out. Top-10 concentration 76.1% → **68.6%**.

  Nothing downstream has been rebuilt, and the modeling scripts still read raw names
  from `all_gsa_edges.csv`, so both changes currently apply to the raw-name pipeline.

## 9. Upstream defects (fix outside the bag system)

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

## 10. Alter-side fixes applied (2026-10-08)

Everything in §7b, §7c and §7d that applies to **Consultant, NGO and Research** is now fixed in
`build_alter_bag_map.R`. Institutional_other was explicitly left deferred and is
unchanged (verified still a strict 1:1 identity passthrough over all 10,099 rows).
The GSA- and integration-side items (§7a blockers, §8 integration) are **NOT**
addressed here and remain open.

### 10a. Result

| type | before | after | shrink before -> after |
|---|---|---|---|
| Consultant | 1017 -> 633 | 1017 -> **397** | 37.8% -> **61.0%** |
| NGO | 823 -> 692 | 823 -> **574** | 15.9% -> **30.3%** |
| Research | 460 -> 391 | 460 -> **288** | 15.0% -> **37.4%** |
| Institutional_other | 10099 -> 10099 | 10099 -> 10099 | 0% (deferred, unchanged) |

`entity_bag_lookup.csv`: 13,833 variants -> **12,088 bags** (was 12,545).
Review menus shrank as the real merges were absorbed: Consultant 469 -> 322,
NGO 600 -> 378, Research 406 -> 306 — and what remains is far higher-signal.

### 10b. Precision fixes

1. **`tokset` now requires a contiguous block ROTATION, not just set equality.**
   A harmless "X of Y" / "Y X" inversion still folds
   (`land_trust_of_napa_county` ~ `napa_county_land_trust`); an interior swap no
   longer does. Cal Poly **San Luis Obispo** and Cal Poly **Pomona** are separate
   bags again. Rejected reorderings are surfaced as a new `reorder` review tier
   rather than dropped.
2. **Load-bearing digits are preserved.** Only a PURE-digit token (page/footnote/
   year), a comment-matrix row id (`a12`, `ct006`), or a digit run trailing a word
   of >=4 letters (`university1`) is stripped. A leading digit is never stripped
   from a token with letters. `con_2ndnature`, `con_4creeks`, `ngo_4_h`,
   `con_6si_water_solutions`, `con_ch2` all keep their identity, and the whole CH2M
   Hill family is on one bag.
3. **Entity-naming words are no longer generic `contains` extras** —
   `foundation`, `fund`, `institute`, `center`, `program`, `council`, `division`,
   `extension`, `laboratory`, `national`, `international`. Unlike `inc`/`llc` these
   name separate legal entities. This is what restored **Clean Water Action** as its
   own bag (35 documents) instead of sitting inside the CWA+Clean Water Fund
   compound, kept the UC system out of UC Extension, and stopped the Irrigation
   Training & Research **Center/Institute/Program** from collapsing into one.
4. **`contains` now also requires the subset to be a contiguous RUN** in the
   superset (the roster tier already did), and refuses a bare generic/weak parent.
5. **`bag_label` is now a function of `bag_id`**, asserted. The cluster audit has
   one row per bag (244 rows / 244 bags) and the review menu can no longer ask a
   human to merge a bag with itself.
6. **`method` is honest, and a new `joined` column says how.** `method` is the tier
   of the edge joining the variant DIRECTLY to its bag's representative; a variant
   pulled in only through a chain reports `transitive`. `joined` is
   anchor / direct / transitive / singleton.
7. **The anchor-clash branch no longer invents a bag.** Unanchored members of a
   clashing component stay on identity and the clash is written to the review menu.
   (Still 0 occurrences, but it is no longer silent if it happens.)
8. **Override files are validated**: a `bag_id` missing its namespace prefix now
   fails the build instead of landing in no namespace.
9. **Locale-independent sorting** via a radix helper that falls back for the two
   accented bag names rather than failing the build.

### 10c. Recall fixes

1. **Roster tiers are stemmed on both sides.** The roster says "GEI **Consultant**,
   Inc." and every corpus mention says "GEI **Consultants**", so GEI had no anchor
   and shattered across 25 bags; it is now 1. `roster_exact` rose 52 -> 155 and the
   digit fix removed the one false match this would otherwise have cost
   (`4creeks_engineering` vs roster "Creek Engineering").
2. **A document-debris allowlist** (`.DEBRIS`) lets `contains` fold a mention
   wrapped in GSP boilerplate — section/appendix/figure/comment/threshold/
   objective/guidance/plan/groundwater/sustainability and the like. This is what
   takes **The Nature Conservancy from 55 bags to 1** (73 members).
   `sustainable` is deliberately excluded: it is part of real names and allowing it
   folded CAUSE's "...for a Sustainable Economy" into "...for Economy".
3. **`.canon_pick` is debris- and abbreviation-aware.** It ranks fewest debris
   tokens, then most real tokens, then fewest abbreviations, then shortest. Without
   the debris step the TNC bag was named
   `ngo_umbrella_groundwater_sustainability_plan_nature_conservancy_checklist`;
   without the abbreviation step the Audubon bag was named `natl_audubon_socy`
   rather than `national_audubon_society`.
4. **An acronym/initialism tier**, invisible to every existing tier (0 acronym pairs
   appeared among the 406 Research review candidates). Initials are taken over the
   name's own tokens with only articles/prepositions dropped, so `rcac` = Rural
   Community Assistance **Corporation** works. Folded: `lsce`, `lwa`, `pmc`, `eci`,
   `wspa` (385 mentions), `cnps`, `crla`, `crpe`, `svwc`, `asfmra`, `ppic`, `twri`,
   `isws`, `csub`, `ucla`, `ucm`, `ucr`, `ucsb`, `ucsc`.
   **Three guards**, and they refuse exactly the cases §7d flagged as ambiguous:
   - a rival bag that SPELLS the initialism vetoes it (`con_bgc_engineering_inc`
     vetoes `bgc`; `con_gsi_water_solutions_inc` vetoes `gsi`) — the initials test
     alone cannot see these, since "bgc_engineering_inc" has initials "bei";
   - more than one expansion matching vetoes it;
   - `.ACRO_BLOCK` covers the one case no structural guard can see — `lsu`, whose
     conventional expansion (Louisiana State) has no bag here, so the initials
     would have been claimed by La Sierra University.
   A refused initialism is a real organization with no home, so it is written to the
   review menu as `acronym_ambiguous` rather than dropped.
   A single-token firm name with no initials match at all (`aecom`, `stantec`,
   `psomas`) is skipped silently — not ambiguous, simply not an acronym.
5. **`counsel` = `council`**, plus `-ies -> -y` stemming and a small unambiguous
   abbreviation map (`natl`, `socy`, `assn`, `univ`, `dept`, `lab`). The
   counsel/council normalization was measured risk-free in §7d and is applied only
   to matching, never to a label.
6. **Extraction artifacts normalized before tokenization**: the `_x` placeholder and
   DWR comment-response-matrix ids (`ngo016`, `mcr_2`, trailing `a12`). This is a
   containment measure, **not** a fix — see 9e.

### 10d. Curated overrides (the folds that no deterministic rule can reach)

Promoted from the audit into the per-type override files, which are read first and
SEED the clustering:

| file | rows | parent bags |
|---|---|---|
| `inputs/entity_bag_overrides_consultant.csv` | 13 -> **295** | 75 |
| `inputs/entity_bag_overrides_ngo.csv` | new, **108** | 21 |
| `inputs/entity_bag_overrides_research.csv` | new, **135** | 19 |

These carry the acronym expansions the uniqueness guard refuses (`mwh`, `uc`,
`ucd`, `ucanr`, `itrc`, `ipcc`, `wrcc`, `csumb`, `rcac`), the OCR and glossary-
corruption spellings, the rename chains (AMEC -> AMEC Foster Wheeler -> Wood;
Todd Engineers -> Todd Groundwater; Cleath -> Cleath-Harris), and the
`res_jet_propulsion_laboratory` synthetic bag name (no variant spells JPL cleanly).

The NGO and Research specs are regenerable: `make_alter_overrides.R` (a one-off
curation tool, NOT in `run_all.R`) holds the family patterns and the policy comments.
It unions with whatever is already on disk and refuses to shrink a file, so a rerun
after the folds are applied cannot silently undo the curation. The consultant list
came from the audit rather than a spec and lives only in the committed override file.

**Policy decisions encoded in them** — the bag is the TIE-BEARING actor:
- a hosted centre / lab / programme folds into its **host campus** (Center for
  Watershed Sciences -> UC Davis; Essig Museum -> UC Berkeley; ITRC -> its own bag);
- **UC Cooperative Extension / ANR is a separate statewide actor**, so a
  "UC Davis Cooperative Extension" mention goes to UCCE, not to the campus;
- bare "University of California" is the **system**, never a campus;
- **named local chapters stay separate** — county Farm Bureaus, named Audubon and
  Sierra Club chapters, county Leagues of Women Voters. Only the unqualified parent
  mentions and misspellings fold. Just the five `<county>_farm_bureau` /
  `<county>_county_farm_bureau` spelling pairs were merged.

Left deliberately unfolded as genuinely ambiguous: bare `con_gsi` (two real GSI
firms), `con_bgc`, `res_csu`, `res_lsu`, `res_scripps` (SIO vs SOPAC),
`res_irrigation_technology_and_research_center` (Fresno State CIT vs Cal Poly ITRC),
`ngo_basin_pumpers_association` (Fillmore vs Santa Paula), RCAP vs RCAC,
Putah Creek Council vs Putah Creek Streamkeeper.

### 10e. Still open

- **The §7a blockers are untouched** (GSA override staleness in the lookup; bare-int
  `canonical_gsp_id`), as are §8 (the fold shrinking the consultant subnetwork via
  `prevalence_max`, the ego/alter self-loop policy) and §8 (`run_all.R`).
- **Institutional_other** remains deferred by decision.
- **The upstream defects in §9 are contained, not fixed.** The folder now strips
  comment-matrix ids and the `_x` placeholder so they stop fragmenting bags, but
  DWR commenter codes and glossary-corrupted strings should never have been
  extracted as entity names, and the type classifier still splits organizations
  across namespaces by a trailing `_x` or citation digit. Those belong to
  `02_text_preprocessing` and the classifier.
- **New product `alter_bag_compounds.csv` (231 rows)** lists mentions that name two
  organizations (`woodard_curran_and_davids_engineering`,
  `clean_water_action_california_waterfowl_association*`). The 1:1 variant -> bag
  schema cannot carry two bags per variant, so the second organization is recorded
  here instead of being silently deleted. **Nothing consumes it yet** — wiring it
  into the edge builder would recover co-authorship ties that are currently lost.
- The `roster_subset` second-firm guard refuses a fold when the surrounding debris
  names another ROSTER firm; it cannot see a second organization that is absent
  from the roster (Davids Engineering, Holland & Knight). Those are in the compounds
  file.
