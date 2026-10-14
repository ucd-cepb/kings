# GSA entity bags — workflow

Entity names from the NER pipeline are fragmented across many spellings of the same
agency (`aliso_gsa`, `aliso_water_district_gsa`, `aliso_wd_gsa`, …). A **bag** folds
all spellings of one real entity onto a single canonical id so each agency is counted
once. This file states the rules the bag system follows, how the bags are built, and
what is still open.

---

## 1. Rules

**Authorship, not mention, defines network membership.** A document's authors are the
GSA ids DWR lists for it in `id_crosswalk.csv` (`gsa_ids`, comma-separated). Resolve ids
to names with the core roster `data/core_data/metadata/sgma_gsa_full.csv`
(`GSA_ID`→`GSA_Name`). Do **not** use the crosswalk's `gsa_names` column — it is
display-only and not row-aligned with `gsa_ids`. A resolved GSA row is kept only if its
`bag_id` is one of that document's `gsa_ids`; anything else is a mention and is dropped.
259 ids are affiliated with at least one plan.

> *Example.* Doc 944 (Aliso WD GSP, 2020) lists `gsa_ids = 23`. Its text also names
> `city_of_mendota_gsa` (gsa_67), `madera_irrigation_district_gsa` (gsa_19) and about 20
> other GSAs, all of which resolve to roster bags. Only gsa_23 is an author, so
> `gsa_bag_edges.csv` has one row for doc 944 (gsa_23); the other GSAs are mentions.

**Key on `gsp_doc_id`, never on `gsp_id`, `plan_family_id`, or `version`.** A plan can
have several documents (2020 original, 2022/23 resubmission, later consolidation) whose
author teams differ, so the author set belongs to the document. `version` is not a
submission key either: it labels both the 2020 original and its 2022/23 resubmission
`version = 1` (53 `(gsp_id, version)` pairs cover more than one submission date), and
`gsp_id` can be reassigned across resubmissions. Order documents by `doc_rank` (or
`submitted_date`). Counts: 185 documents, 132 `gsp_id`s, 120 canonical plans.

> *Example.* Canonical plan 7:
>
> | gsp_doc_id | gsp_id | submitted | doc_rank | version | authors |
> |---|---|---|---|---|---|
> | 944 | 0007 | 2020-01-17 | 1 | 1 | 1 (Aliso WD GSA) |
> | 8770 | 0007 | 2022-07-20 | 2 | 1 | 1 (Aliso WD GSA) |
> | 11932 | 0174 | 2026-05-28 | 3 | 2 | 23 (Delta-Mendota consolidated) |
>
> Keyed on the plan, the 2020 Aliso document would inherit all 23 consolidation authors.

**Do not merge one roster id into another.** `inputs/bag_merges.csv` is empty and stays
empty. A name variant folds to the roster id(s) it names; ids are never rewritten
to each other. Merging would drop real authors, because a document that lists the member
id but not the parent would fail the authorship test.

> *Example.* Turner Island WD has two ids: gsa_220 (Delta-Mendota) and gsa_223 (Merced).
> Docs 4342/8708 (plan 13) list 220 only; docs 386/8897 (plan 9) list 223 only. Merging
> 220 into 223 would remove Turner Island as an author of plan 13.

**Multi-management-area agencies: keep each area's id distinct.** About 41 organizations
operate more than one management-area GSA that share a base name and differ only by a
" – <Management Area>" suffix (Turner Island {220,223}; Sacramento County {295,496,497};
Tehama {124,418–423}; Imperial ×13; Salinas ×6; Fox Canyon ×4; …). A variant that names
such an organization is a pointer into the family, not a single id. At build time it
resolves per document to whichever family member(s) that document lists as authors; if a
document lists more than one, keep all of them; if it lists none, the variant is a
mention and is dropped.

> *Example.* `turner_island_water_district_gsa` sits in bag gsa_223 in the map, but in
> `gsa_bag_edges.csv` it becomes gsa_223 in docs 386/8897 (plan 9) and gsa_220 in docs
> 4342, 8708, 458, 8825 and 11932 — whichever family member each document lists.

**A name that refers to several GSAs counts in each of their bags.** It gets one
override row per GSA it names, so it sits in every one of those bags. Strict authorship
still applies per document: only the named GSAs that author that document are kept.

> *Example.* `mcmullin_and_james_groundwater_sustainability_agencies` is in gsa_12 James
> and gsa_266 McMullin Area. `cosumnes_groundwater_sustainability_agencies` is in all seven
> Cosumnes subbasin GSAs, the authors of doc 5232. In `GSA_BAGS_review.txt` such names are
> tagged `<override; also gsa_…>`.

**Groups with no roster id stay as identity, which makes them non-authors.** Umbrellas,
coalitions, and multi-agency plan areas have no single `GSA_ID`, never appear in any
document's `gsa_ids`, and so can never be authors. Their coordination is represented
through their member GSAs, which avoids double-counting. Current cases: Kern Groundwater
Authority; South of Kern River; Salinas Valley umbrella (`svbgsa` + `salinas_valley_*`);
North Delta; McFarland; Pauma Valley; the Colusa + Glenn two-authority JPA.

> *Example.* `kern_groundwater_authority` and `kern_groundwater_authority_board` stay on
> their own identity bags. The KGA's members (e.g. Semitropic, Shafter-Wasco) appear as
> authors under their own roster ids instead.

---

## 2. How bags are built

`build_entity_bag_map.R` → `entity_bag_map.csv` maps each variant to a bag;
`entity_bag_unmatched.csv` holds everything sent to review, with the would-be target as
`sugg_bag_id`. Tokens are compared after dropping SGMA boilerplate only, so
"county"/"city"/"water"/"district" stay and keep sibling agencies apart. Tiers, in order:

- **Override** — a row in `inputs/entity_bag_overrides_gsa.csv` (244 rows, 224 variants) wins over the
  matcher. These are the authoritative reviewed decisions, and the only route for
  abbreviations and for names that lack a GSA marker (below).
  > *Example.* `pixid` → gsa_43 (Pixley ID GSA); `westland` → gsa_40 (Westlands WD GSA).
- **`exact`** — the variant equals a roster name.
  > *Example.* `kern_river_gsa` → gsa_20 Kern River GSA.
- **`subset`** — all the roster name's distinctive tokens appear in the variant.
  > *Example.* `james_id_gsa` and `vina_subbasin_gsa` → gsa_12 James GSA and gsa_409 Vina GSA.
- **`revsubset`** — all the variant's distinctive tokens fall within one roster name.
  > *Example.* `olcese_gsa` → gsa_196 Olcese Water District GSA.
- **`fuzzy`** — near-miss spellings; reviewed, never auto-applied.
  > *Example.* `el_rio_gsa` is suggested for gsa_358 El Rico GSA but does not fold.
- **Identity** — anything left over stays on its own bag.

Only an unambiguous `subset`/`revsubset` auto-folds. A match to more than one roster
name goes to review as `subset_tie` / `revsubset_tie`.

**Matcher guards.** Four guards send a would-be auto-fold to review under their own
`method`:

- **`revsubset_bare`** (50): a `revsubset` match with no place name. Either every
  distinctive token is a direction or feature word (north/south/east/west/upper/lower/
  central/mid/river/valley/basin/subbasin/county), or the name rests on a single token that
  is shorter than three characters or is shared by more than one roster base name ("kern",
  "merced", "sacramento"). Bare tokens unique to one agency (aliso, olcese, yucaipa) still
  fold.
  > *Example.* `east_gsa` could be any of eight "East" GSAs (Kings River East, East
  > Kaweah, Stockton East, …), so it is not folded. `yuba_gsa` matches both Yuba Water
  > Agency areas and City of Yuba City, so it is not folded either. `b_gsa` and `el_gsa`
  > rest on one- and two-letter tokens and are not folded. `upper_basin_gsa` had folded to
  > Salinas Valley Basin GSA - Upper Valley Aquifer on "upper" and "basin" alone; it is now
  > held.
- **`revsubset_generic`** (17): a `revsubset` match where every distinctive token is an
  entity-type word (water, district, storage, conservation, resource, irrigation,
  reclamation, county, city, authority, …). Such a name describes a kind of agency, not
  one agency.
  > *Example.* `storage_district_gsa` could be Semitropic, North Kern, Buena Vista or any
  > other water storage district, and `conservation_district_groundwater_sustainability_agency`
  > could be any RCD GSA, so neither is folded. `district_water_authority_gsa` (only
  > Westside) and `county_water_authority_…` (only Tri-County) were reviewed as unique and
  > fold by override.
- **`subset_geo`** (7): a `subset` match where the variant adds a direction word to the
  roster name. Added feature words do not trigger it, since they are usually descriptors.
  > *Example.* `south_of_kern_river_gsa` contains every token of "Kern River GSA" but
  > names the South of Kern River group (Arvin, Wheeler Ridge-Maricopa, Tejon-Castac), so
  > it is not folded. `madera_county_east_gsa` is held for the same reason. By contrast,
  > `pleasant_valley_subbasin_gsa` adds only a feature word and folds to Pleasant Valley GSA.
- **`no_gsa_marker`** (40): beyond the exact tier, a variant has to say it is a GSA to
  fold. The markers are a `gsa`/`gsas`/`gssa`/`kgsa` token, a "groundwater sustainab…"
  fragment (the extractor truncates and misspells "agency"), or a dedicated groundwater
  agency/authority (Glenn Groundwater Authority, Upper Ventura River Groundwater Agency and
  Fox Canyon GMA each are their GSA).
  > *Example.* `kern_river` could be the river or Kern River GSA, and `mid_kaweah` could
  > be the area or Mid-Kaweah GSA, so neither is folded. `mid_kaweah_grounwater_sustainability_agency`
  > carries the misspelled marker fragment and folds to gsa_7.

**Policy for names without a marker.** A district, city or county name with no marker
folds only through a reviewed override row, never automatically. A bare place or feature
name never folds, even by override.

> *Example.* `westlands_water_district` → gsa_40 is a reviewed override row.
> `county_of_tulare` has no override, because it could be the county government;
> `county_of_tulare_gsa` folds to gsa_470. `kern_river` and `san_diego_river_valley` have no
> override and stay unfolded.

**Document author overrides.** Where DWR publishes no authors for a document,
`inputs/doc_author_overrides.csv` + `apply_author_overrides()` in `Code/00_ingest_core.R`
fill `gsa_ids` before the crosswalk is written. The step only fills blanks (it stops rather
than overwrite a DWR value), requires ids and names to agree with the roster, and records
the source in `id_crosswalk.csv`'s `gsa_ids_source` column (`scrape` / `override`).

> *Example.* Both documents of plan 43 (doc 3712, 9090) are blank upstream; DWR's
> `localId` is "Basin 5-022.13 - ETGSA GSP", so the override sets `gsa_ids = 205` (Eastern
> Tule GSA). Note that canonical plan 43 ≠ GSA_ID 43 (Pixley ID GSA); the roster check
> catches that collision.

---

## 3. Outputs and review files

- **`gsa_bag_edges.csv`** (`build_gsa_bag_edges.R`): one row per (document, authoring
  bag), keyed on `gsp_doc_id` + `plan_family_id`, with each bag's co-mentioned alters
  as columns. It folds authoring GSA rows, keeps only ids in each document's `gsa_ids`,
  expands multi-area pointers per document, and drops self-loops. Currently 447 rows,
  184 documents, 119 plans, 255 authoring bags. Plan 52 (doc 1683) drops under strict
  authorship because its sole author, gsa_106, has no folding variant.
  - **GSA alters fold to bags too, without the authorship filter.** A GSA named in a plan
    it did not write stays an alter. 413 GSA alter names fold into 209 `bag:` columns:
    `bag:gsa_<id>`, or `bag:gsafam_<ids>` for a name pointing into a multi-area family,
    since no author set picks the member on the alter side. A name carrying a token
    unique to one member's area suffix keeps that member
    (`merced_county_delta_mendota_gsa` → County of Merced - Delta-Mendota). A multi-GSA
    name credits each bag; several names for one bag sum. Alters in the ego's own bag or
    family (its other names) are dropped. GSA names held unmatched, and all non-GSA
    alters, stay raw names. The `bag:` prefix avoids raw node names like `gsa_25`.
- **Review dictionaries:**
  1. Variant → GSA bag: `01_entity_classification/GSA_BAGS_review.txt`
     (`write_bag_review.R`, run after `build_entity_bag_map.R`; lists every bag,
     including single-name ones).
  2. Document → GSA authors (`gsa_ids` resolved through the roster):
     `00_ingest/DOC_AUTHORS_review.txt`; flags 17 documents where `gsa_names` disagrees
     with the roster.
  3. Document → authoring GSA bags: `01_entity_classification/DOC_AUTHOR_BAGS_review.txt`
     plus long-form `doc_canonical_gsa_bags.csv`. Two affiliated labels shared across ids
     stay distinct bags: Siskiyou FC&WCD {253,456,457}, Tehama FC&WCD {124,418,419,420,422}.

---

## 4. Alter side — consultants, NGOs, research

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

Unmatched GSA variants sit on a bare (un-prefixed) identity bag, so carry the `bag_type`
column rather than parsing the `bag_id` prefix.

**Folder rules.** `tokset` requires a contiguous block rotation, so a harmless "X of Y" /
"Y X" inversion folds but an interior swap does not (Cal Poly San Luis Obispo and Cal Poly
Pomona stay separate). Only a pure-digit token, a comment-matrix row id, or a digit run
trailing a ≥4-letter word is stripped, so load-bearing digits survive (`2ndnature`,
`4creeks`, the whole CH2M Hill family on one bag). Entity-naming words
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

**Overrides.** Curated overrides seed the clustering and are read first (consultant 295,
NGO 108, research 135 rows). Policy: a hosted center/lab folds into its host campus; UC
Cooperative Extension / ANR is a separate statewide actor; bare "University of California"
is the system; named local chapters (county Farm Bureaus, named Audubon and Sierra Club
chapters, county Leagues of Women Voters) stay separate. `make_alter_overrides.R`
regenerates the NGO and research specs — it unions with what is on disk and refuses to
shrink a file. Deliberately left unfolded as genuinely ambiguous: `con_gsi` (two firms),
`con_bgc`, `res_csu`, `res_lsu`, `res_scripps`,
`res_irrigation_technology_and_research_center` (Fresno State CIT vs Cal Poly ITRC),
`ngo_basin_pumpers_association`, RCAP vs RCAC, Putah Creek Council vs Putah Creek
Streamkeeper.

**Compound mentions.** `alter_bag_compounds.csv` (231 rows) lists mentions that name two
organizations (`woodard_curran_and_davids_engineering`). The 1:1 variant → bag schema
cannot carry two bags, so the second organization is recorded there.

Review menu counts: Consultant 322, NGO 378, Research 306.

---

## 5. Network construction (modeling)

- **Non-actor filter.** `NON_ACTOR_ENTITIES` in `Code/_entity_groups.R` drops bare
  state/nation placenames by name, in raw and `io_` spellings (`io_california`,
  `io_calif`, `io_united_states`, `io_united_states_of_america`, `io_us`). Ubiquity alone
  is not disqualifying — DWR is in 115/118 plans because it administers SGMA, and it
  stays. Counties stay (shared county naming is real co-location, and counties are often
  governmental actors). `build_shared_entity_matrix()` has an opt-in `prevalence_max`
  argument, off (`NA`) by default.
- **Binarized incidence.** `build_shared_entity_matrix()` reduces the plan × entity
  incidence to mentioned/not before `tcrossprod()`, so a cell counts shared entities. A
  uniplex `weight` is the number of parsed relations between two entities in a document
  (`core_code/step5_build_igraphs.R`), so weighting would carry verbosity and coalition
  size into the coefficients; binary is the direct operationalization and the least
  DWR-concentrated reduction (DWR share of the institutional covariate: weighted 55.4%,
  binary 41.3%). `binarize = FALSE` restores the weighted form.
- **Direction discarded.** The construct is an observed relational event between two
  entities, so the symmetric fold is the intended shape. Direction is recoverable from the
  directed core graphs only as additional direction-preserving edgecovs.
- **Self-loops.** Predictors are between-document values, so self-loops sit only on the
  plan × plan diagonal, which every caller zeroes; `build_gsa_bag_edges.R` drops them.
- **`mult_gsa`** is an integer count of a document's `gsa_ids`, used as
  `nodecov('mult_gsa')` in `make_binary0.9_networks.R`, `explore/make_networks.R`, and
  `explore/make_valued_networks.R`. Its minimum over the 118-plan frame is 1.

---

## 6. Open items

- **Wiring.** `run_all.R` registers none of the bag scripts. Stage 1 still runs
  `build_gsa_edges.R` → `all_gsa_edges.csv`, and `04_modeling/make_binary0.9_networks.R:36`
  still reads it. Moving the model to `gsa_bag_edges.csv` (keyed on `gsp_doc_id`) replaces
  the id-to-doc melt there and retires `build_gsa_edges.R`; update
  `Code/01_entity_classification/ENTITY_TAGGING.md` in the same commit. Until then
  `mult_gsa` is a partial control, because `build_gsa_edges.R` counts every GSA-typed
  vertex, not just authors. The run checks are existence-based, so a changed input behind
  an existing output is missed; add an mtime or input-hash check.
- **Alters not wired in.** `build_gsa_bag_edges.R` folds GSA alters to bags but still
  emits raw node names for every other alter type, and `_entity_groups.R::entity_names()`
  groups alters by raw name; both need to group by `bag_id` and `bag_type`. Nothing consumes `alter_bag_compounds.csv` yet; wiring it in
  would recover co-authorship ties that are currently lost.
- **`entity_bag_lookup.csv` disagrees with the GSA overrides** on 115 of 209 joinable
  rows. The lookup takes only `entity_type == "GSA"` rows from `entity_bag_map.csv`, so
  override variants typed otherwise are taken from `alter_bag_map.csv` instead (e.g. `pixid`, typed `Institutional_other`, lands on `io_pixid` rather than gsa_43), and the
  file predates the latest map rebuild. Fix: rebuild it, and apply the GSA override with
  the same precedence `build_gsa_bag_edges.R` uses. `gsa_bag_edges.csv` is unaffected.
- **`plan_family_id` in `gsa_bag_edges.csv` is a bare integer**, not 4-digit
  zero-padded, so it fails to join `modeling_plan_selection()`, `gsp_covariates.csv`, and
  the shapefile. `gsp_doc_id` joins fine.
- **`Institutional_unresolved`** (2,848 variants, 17% of the institutional population) is
  in no namespace and has no review file. It needs its own namespace, an explicit drop
  decision, or a second classifier pass.
- **Review-file generators** for `DOC_AUTHORS_review.txt` and `DOC_AUTHOR_BAGS_review.txt`
  are not in `Code/`, so those files cannot be regenerated from the repo.
- **EJZ markup of the bag review:** `EJZ_MARKUP_STATUS.md` tracks which flags the guards
  obviated and which remain: one-to-many "agencies" names, generic-suffix folds, `b_gsa`, and the
  `el_rico` regression.
- **Run-on variants held by `subset_geo`:** `mcmullin_area_gsa_east_gsa` and
  `north_kings_groundwater_sustainability_agency_western_boundary_groundwater`. Add
  override rows if they should fold.

### Upstream defects (fix outside the bag system)

Folding cannot repair these; they belong to `02_text_preprocessing`, the extractor, or the
classifier.
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
