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

**Do not merge one roster id into another.** (The unused `inputs/bag_merges.csv` hook was
removed 2026-11-24.) A name variant folds to the roster id(s) it names; ids are never rewritten
to each other. Merging would drop real authors, because a document that lists the member
id but not the parent would fail the authorship test.

> *Example.* Turner Island WD has two ids: gsa_220 (Delta-Mendota) and gsa_223 (Merced).
> Doc 11932 (the 2026 Delta-Mendota consolidated plan) lists 220 only; docs 386/8897
> (plan 9) list 223 only. Merging 220 into 223 would remove Turner Island as an author of
> doc 11932.

**Multi-management-area agencies: keep each area's id distinct.** About 41 organizations
operate more than one management-area GSA that share a base name and differ only by a
" – <Management Area>" suffix (Turner Island {220,223}; Sacramento County {295,496,497},
whose roster names mix "County of Sacramento GSA" and "Sacramento County GSA", so
`.build_families()` compares base names in normal form;
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
  bag), keyed on `gsp_doc_id` + 4-digit `plan_family_id`, with each bag's co-mentioned
  alters as columns. It folds authoring GSA rows, keeps only ids in each document's
  `gsa_ids`, expands multi-area pointers per document, and drops self-loops. Currently
  422 rows, 185 documents, 120 plans, 260 authoring bags, 8,272 alter columns
  (2026-11-24); every author slot has a row. GSA-ness is decided by the resolver (override ∪ GSA-typed map rows),
  not by the classifier's type (since 2026-11-16).
  - **GSA alters fold to bags too, without the authorship filter.** A GSA named in a plan
    it did not write stays an alter. Columns are `bag:gsa_<id>`, or `bag:gsafam_<ids>`
    for a name pointing into a multi-area family, since no author set picks the member
    on the alter side. A name carrying a token unique to one member's area suffix keeps
    that member (`merced_county_delta_mendota_gsa` → County of Merced - Delta-Mendota).
    A GSA name on a non-roster bag (bare identity, the KGA umbrella) folds to
    `bag:<bag_id>` with no family logic. A multi-GSA name credits each bag; several
    names for one bag sum. Alters in the ego's own bag or family (its other names) are
    dropped. The `bag:` prefix avoids raw node names like `gsa_25`.
  - **Every other in-network alter folds too** (since 2026-11-16), through
    `entity_bag_lookup.csv`: `bag:con_*`, `bag:ngo_*`, `bag:res_*`, `bag:io_*`. A
    compound mention in `alter_bag_compounds.csv` credits every bag it names. Only
    `Non_institutional` and `Institutional_unresolved` names stay raw, and no grouping
    admits them. Column census: 2,022 bag keys (286 GSA, 61 Consultant, 87 NGO, 28
    Research, 1,560 Institutional_other) and 6,373 raw names (5,888 Non_institutional,
    485 Institutional_unresolved).
- **`gsa_bag_edges_alters.csv`** (same script): one row per alter column — `name`,
  `entity_type` (the namespace that OWNS the bag; a raw name's `node_dictionary` type),
  `bag_type`, `bag_id`, `bag_label`, `n_variants` (names folded into the column,
  corpus-wide), `folded`. This is what `_entity_groups.R::entity_names()` groups on.
- **Review listings** (both regenerated by `run_all.R`; neither is read by the model):
  1. Variant → GSA bag: `01_entity_classification/GSA_BAGS_review.txt`
     (`write_bag_review.R`, step 1d, run after `build_entity_bag_map.R` whenever
     Stage 1 rebuilds; lists every bag, including single-name ones).
  2. What the model gets: `01_entity_classification/DOC_BAGS_review.txt`
     (`write_doc_review.R`, step 1h, run on EVERY `run_all.R` call after the Stage 1
     guard, so it tracks both the products and `_entity_groups.R`). Sections: coverage
     flags (crosswalk authors with no edge row, `[SCRAPE-NAME MISMATCH]` documents,
     `[AUTHOR OVERRIDE]` slots); plan family → document → authoring bags with alter
     counts; each authoring bag's in-network alter profile (per-network counts, the
     excluded count, top alters by weight). As of 2026-11-17: 5 of 452 author slots
     have no edge row (South/North Yuba, Butte 1/2, Sacramento South American), 17
     scrape-name mismatches, 2 override slots.
  It replaced three hand-committed snapshots that no script wrote, deleted 2026-11-17:
  `00_ingest/DOC_AUTHORS_review.txt`, `DOC_AUTHOR_BAGS_review.txt` and its long form
  `doc_canonical_gsa_bags.csv`. Two affiliated labels shared across ids stay distinct
  bags: Siskiyou FC&WCD {253,456,457}, Tehama FC&WCD {124,418,419,420,422}.

---

## 4. Alter side — consultants, NGOs, research

The alter side folds the co-mentioned entities that a plan's authoring GSAs tie to. Each
type folds in its own namespace so one type's network never merges into another's.
Producers: `build_alter_bag_map.R` → `alter_bag_map.csv` (+ cluster audit and per-type
review files); `build_entity_bag_lookup.R` → `entity_bag_lookup.csv` (the single join
table). The lookup applies the same precedence as the edge builder's resolver: every
variant in `inputs/entity_bag_overrides_gsa.csv` is a `gsa`-namespace row whatever type
the classifier gave it (24 override names are typed `Institutional_other`, e.g. `pixid` →
gsa_43 and the Eastern Tule board/committee names → gsa_205; 4 are `Non_institutional`,
e.g. `greaterkaweahgsaorg` → gsa_281), then the map's GSA-typed rows, then the alter
maps minus those variants. `entity_type` records what the classifier said about the
spelling; `bag_type` is the namespace that owns the bag, and is what downstream groups
on. Asserted: no variant in two processes, no bag in two namespaces, and the
`gsa`-namespace rows equal the resolver's set exactly (fixed 2026-11-16; before, the
lookup took only GSA-typed map rows, so those 28 names landed on `io_*` bags).

| type | namespace | authority | variants → bags |
|---|---|---|---|
| Consultant | `con_*` | `consultant_dictionary.csv`, then self-cluster | 1017 → 398 |
| NGO | `ngo_*` | self-cluster (no roster) | 823 → 575 |
| Research | `res_*` | self-cluster (no roster) | 460 → 291 |
| Institutional_other | `io_*` | curated overrides only, no self-cluster (2026-11-18) | 10099 → 9316 |

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

**Compound mentions.** `alter_bag_compounds.csv` (37 rows) lists mentions that name two
organizations (`woodard_curran_and_davids_engineering`). The 1:1 variant → bag schema
cannot carry two bags, so the second organization is recorded there, and
`build_gsa_bag_edges.R` (`CREDIT_COMPOUNDS`, on) credits every bag the row names: 37
names add 69 extra bag ties. Adds ties only. Fixed 2026-11-18: a candidate is credited
only when its span in the mention is disjoint from another organization's span. Before
that, 229 rows credited a bag whose label merely sat inside the mention's own
organization (`fresno_county_farm_bureau` also crediting `ngo_county_farm_bureau` and
`ngo_farm_bureau`; every UC/CSU campus crediting the bare system bag). Five rows that
remain credit a unit's parent (a CNPS chapter, UC ANR, the Aarhus hydrogeophysics group,
Cal Poly ITRC).

Review menu counts: Consultant 108, NGO 320, Research 157 (2026-11-24, after the open-items pass).

---

## 5. Network construction (modeling)

- **Input.** Since 2026-11-16 `make_binary0.9_networks.R` (and the two `explore/`
  scripts) read `gsa_bag_edges.csv`, filtered to the one selected document per plan
  (`modeling_plan_selection()`), with no gsp_id translation. Alter columns are grouped
  with `gsa_bag_edges_alters.csv` through `_entity_groups.R::entity_names()`, which
  groups a folded column by the type that owns its bag (`bag_type`) and a raw column by
  its `node_dictionary` type. `run_all.R` registers the whole chain (Stage 1c–1g) and
  checks input mtimes against products, so an edited override file re-triggers the stage.
- **Non-actor filter.** `NON_ACTOR_ENTITIES` in `Code/_entity_groups.R` drops bare
  state/nation placenames by name, in raw, `io_` and `bag:io_` spellings (`io_california`,
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

Closed 2026-11-16 (one commit): the model and `run_all.R` now run on the bag chain
(`build_gsa_edges.R` / `all_gsa_edges.csv` retired; Stage 1 registers 1c–1g; the run
check compares input and product mtimes); non-GSA alters fold to bag keys and
`entity_names()` groups by `bag_type`, with `alter_bag_compounds.csv` consumed;
`entity_bag_lookup.csv` is rebuilt with the GSA override taking precedence over the
classifier type; `plan_family_id` is written 4-digit. One consequence to be aware of:
GSA-ness on both the ego and alter side is now decided by the resolver (override ∪
GSA-typed map rows), so the 28 override names the classifier typed otherwise count as
their GSA; before they were gated out on type.

- **`Institutional_unresolved`** (2,848 variants, 17% of the institutional population):
  decided 2026-11-16, not open. These names are unresolvable by definition, so they have
  no cross-document meaning and get no namespace, no bag, and no review file. They stay as
  raw columns in `gsa_bag_edges.csv` for provenance only; no grouping in
  `_entity_groups.R` admits them, so they never enter a network.
- **Orphan author-side products:** resolved 2026-11-17. The three hand-committed
  snapshots were deleted and replaced by the generated `DOC_BAGS_review.txt` (§3).
- **EJZ markup of the bag review:** `EJZ_MARKUP_STATUS.md` tracks which flags the guards
  obviated and which remain (gsa_458 tags; Northern Delta-Mendota resolved 2026-11-18). The `el_rico`
  regression is decided 2026-11-16: it stays unmatched. The bare-name guard exists to
  minimize false-positive folds, and a false negative on one name is the accepted cost.
- **Run-on variants held by `subset_geo`:** decided 2026-11-16, not folded, same
  false-negative policy. They are also inert: `mcmullin_area_gsa_east_gsa` occurs once, as
  an isolated vertex in the Kings River East GSP (doc 2704), and
  `north_kings_groundwater_sustainability_agency_western_boundary_groundwater` occurs only
  in North Kings' own two GSPs with no GSA co-mention, so folding would be a dropped
  self-tie. Neither name reaches `gsa_bag_edges.csv` either way.
- **2026-11-18 bag review (nine-agent pass):** applied:
  - `.build_families()` compares base names in normal form, so gsa_295 joins the
    Sacramento County family {295,496,497} and doc 6252 regains it as an author.
  - `.find_compounds()` credits only disjoint spans (§4); 229 → 37 rows.
  - Authors recovered: `county_of_butte_gsa` → gsa_122 (family {122,251}, doc 6291),
    `yuba_water_agency_formerly_yuba_county_water_agency_gsa` → gsa_106 (family
    {106,413}, docs 1682/1683; plan 52 re-enters the network).
  - Plan 0013 (docs 4342, 8708) authors 23 → 8 via a `replaces_gsa_ids` row in
    `doc_author_overrides.csv` (see README, ingest rule 1).
  - Held as identity (false-positive risk): `valley_groundwater_authority` (was the
    Gate1 → gsa_35 override; also means Owens Valley GA in doc 5091), the two
    `marina_groundwater_sustainability_*` names (City of Marina vs Marina Coast WD),
    `san_luis_obispo_gsa` / `luis_obispo_gsa` (City vs County), bare `montgomery`,
    `fall_creek_engineering`, `leadership_council`, `university_of_california_extension`,
    `us_cooperative_extension`, `california_state_polytechnic_university`.
    `montgomery_engineers` → James M. Montgomery Consulting Engineers, not MWH.
  - Policy decided by the user 2026-11-18, all as rows in `inputs/entity_bag_overrides_gsa.csv`:
    - **Multi-GSA group names credit every member** (one row per GSA, the Cosumnes
      precedent): Eastern San Joaquin ×16 (was the County of San Joaquin family), Solano
      Collaborative ×6, East Contra Costa board ×7, Merced ×3, Kaweah ×3, Turlock ×2
      (previously held), Colusa ×2, Sierra Valley ×2, Corning `sub_basin` ×2, Central +
      South Kings ×2, Cosumnes `subbasin` ×7. `northern_and_central_delta_mendota_…_agencies`
      → the 8 plan-0013 GSAs, which **closes the Northern Delta-Mendota open item**. Not
      group names, still held: the Delta-Mendota "GSA group" names (members undocumented)
      and the singular `east_contra_costa_gsa` (ECCID 252 vs Byron-Bethany 463).
    - **District name → GSA when the district is the GSA's sole member** (roster
      `formation_type = SINGLE`; districts and water agencies only, never a city or
      county, whose bare names carry unrelated meanings): 14 names (Madera ID, Farmers WD,
      Root Creek WD, Tri-County WA, …) plus three Yuba Water Agency names. Pixley ID GSA
      is `MULTIPLE` with no members listed, so `pixley_irrigation_district` → gsa_43 is
      outside the rule; it is kept as the Gate1-approved exception.
    - **GSA boards and committees fold into their GSA**: 53 names typed
      Institutional_other (North Kings, East/Mid-Kaweah, Fox Canyon, Colusa GA, …). KGA
      boards go to the `kern_groundwater_authority` umbrella identity. Held: the Salinas
      advisory committees (open gsa_458 item) and low-confidence truncations.
    - **Institutional_other curated folding on**: 1,029 high-confidence rows (state,
      federal, tribal and local) in `inputs/entity_bag_overrides_institutional_other.csv`,
      246 bags; `self_fold` stays FALSE. In-network io columns 1,560 → 1,367.
  - Not a bug: `dwr`, `usgs`, `usbr` are absent from `node_dictionary.csv` because core
    disambiguation already rewrote them (`department_of_water_resources`, …). Their
    `entity_type_overrides.csv` rows are inert.
- **2026-11-24 bag review, second pass** (missed folds and review-queue calls):
  - GSA: 60 override rows from the unmatched-queue review. Tier B (49): names with a
    GSA marker or the full/OCR'd agency name (Arroyo Seco ×5, DEID ×4, Fox Canyon ×3,
    IWV ×3, Spanish-language names ×4, Siskiyou FC&WCD ×4, …). Tier E (11): roster
    agencies named with a GSA marker, reaching the family by pointer (Camrosa, County
    of Ventura, IID, CVWD, MSWD, SFPUC, Zone 7, Pajaro, Santa Cruz Mid-County).
    `valley_groundwater_authority_july` held, like its base name. Tier C abbreviations
    (55, e.g. `nkgsa`, `nfkgsa`) not applied.
  - Consultant (25 rows): EKI ← Erler & Kalinowski; WRA Inc → WRA Environmental, bare
    `wra` held; typos (Fugro, Intera, Balance, GSI, Akel, BSK, RL Schafer, CDM); same-firm
    pairs (AKM, MBK, SHN, DHI, TRC, Aqua Terra, PACE, Michael Baker, Aquatic Bioassay,
    Ahtna). Bare `environ` left alone (possible truncation).
  - NGO (43) / Research (44): the review-queue "merge" calls plus the SPLIT findings
    (UC ANR forms → UCCE, Cal Poly SLO, UNAVCO/PBO, MALDEF, Sac State, Chico State,
    Stanford, LBNL, Cornell Lab, New Mexico Tech, Friends of Stone Lakes, poultry and
    water-foundation short forms). Held as too generic: bare
    `agriculture_and_natural_resources`, `water_conservation_coalition`,
    `environmental_justice_coalition`. The review's "undecided" rows and
    hosted-unit folds (Sac State CCP etc.) were not applied.
  - Code: `build_alter_bag_map.R` step 3b no longer renames override-anchored bags
    (it rewrote `con_mcr_engineering` to `con_engineering`, because `.depunct` strips
    "mcr"). Only that one bag id changed.
  - Not done in this pass; closed in the open-items pass below.
- **2026-11-24 open-items pass** (closes the remaining §6 items):
  - **Tier C GSA abbreviations, 53 rows** (`nkgsa`, `nfkgsa`, `ck_gsa`, `gkga`, `mkgsas`,
    `ekga`, `rcwdgsa`, `sjrecgsa`, `swk_gsa`, `tcwa_gsa`, `fcgma`, `uvrgsa`, `ema_gsa`, …).
    Each variant was located in the weighted graphs, and every occurrence is in a plan of
    the GSA's own or a neighboring basin (e.g. `nkgsa` in 13 Kings/Madera plans,
    `fcgma` in the Santa Cruz Mid-County plan, `nswd_gsa` next to
    `madera_new_stone_water_district_chowchilla_bypass`). Held: bare `kre` and `yga`
    (short, no marker; both isolated vertices in their own GSA's plan, so inert).
    Effect: seven raw GSA columns (`bag:nkgsa`, `bag:swk_gsa`, `bag:sjrecgsa`, …) that
    stood as separate GSA alters now credit the roster GSA; `swk_gsa` (degree 17 in the
    Tulare Lake plan) brings its neighbors onto Southwest Kings' row.
  - **gsa_458 (EJZ: "monterey tags" if we supernetwork): closed, no change needed.**
    Generic SVBGSA names fold as alters to the whole-family key
    `gsafam_268_458_459_460_461_462`; only names carrying "monterey" go to gsa_458; egos
    resolve by authorship (six plans, six ids). gsa_458 is only the family's anchor in
    the review file. The Salinas committees held on 2026-11-18 now fold under the board
    policy (2 advisory committees + `_advisory` + `_bod` → family; the Monterey Subbasin
    planning committee → gsa_458), plus 8 run-ons carrying citation digits, a date,
    spacing or a possessive. Not folded: `non_…`, `groundwater_extraction_…`, the
    w159–w162 comment codes, `chapter_6_…`, `_project`, and the Marina and Mountain
    Counties compounds.
  - **Review "undecided" rows (42), checked in plan text.** Folded: `plant_society` →
    CNPS (line-broken, doc 9300), `institute_for_conservation` → ICARE (doc 8062),
    Ballona Ecosystem Education Group → Foundation (same org, doc 6073),
    `landwatch`/`land_watch` → LandWatch Monterey County (Salinas plans only),
    `california_resource_lab` → CA Soil Resource Lab, `university_of_california_agriculture`
    → UCCE/ANR. Not folded: `ucr_coray`/`ucr_scott` (well names), University of
    Nebraska (its State Museum, not UNL), Kisling/Fowler CIT names (run-ons of people:
    Steve Kisling, Teapot Dome WD), Monterey Coastkeeper vs Waterkeeper, Cal Poly
    station names, chapters, compounds and generic parents.
  - **Wrong fold split:** `center_for_irrigation_technology` (Fresno State CIT; Kings and
    Tule plans) had been token-folded into `irrigation_technology_center`, which in all
    five of its plans is Texas A&M's center. CIT now folds to `res_fresno_state`;
    the Texas A&M name is pinned to its own bag.
  - **Hosted units folded per the §4 policy:** Sac State Geology and the Consensus and
    Collaboration Program → Sac State; UCSC hydrogeology group → UCSC; CWU analysis
    center → CWU; Aarhus HydroGeophysics Group → Aarhus. Bare `hydrogeophysics_group`
    held (also Stanford's).
  - **Labels:** new `inputs/alter_bag_label_overrides.csv` (15 bags: GEI Consultants,
    Davids Engineering, Stillwater Sciences, HydroFocus, Dunn, Quad Knopf, Earth
    Consultants, MCR Engineering, five stemmed NGO/research labels, Texas A&M ITC),
    applied after matching and compounds so it only changes display. Pipe-joined roster
    labels (8 bags) are trimmed to their first form.
  - **`gsa_1`:** an unmatched GSA token shaped like a roster id now gets the bag id
    `gsa_1_unrostered`, so it can never read as roster id 1 (`build_entity_bag_map.R`).
  - `run_all.R` Stage 1 now also watches `entity_bag_overrides_institutional_other.csv`
    and `alter_bag_label_overrides.csv` for staleness.
  - Net: edge file 422 rows, 8,272 alter columns; unmatched 1,067 → 1,006; compounds 37;
    0 of 422 author slots without a row; all override rows map as written.

### Upstream defects (recorded, not to be fixed)

Decided 2026-11-16: the text is taken as found. Folding cannot repair these and no
preprocessing, extractor, or classifier fix is planned; the list stays as a record of
known noise in the names.
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
