# GSA Bag Review — consolidated agent findings

Generated 2026-10-06 from a 3-agent fan-out review of the folded GSA bags, the
missed-match singletons, and the two hand-authored input files. **Every target
GSA_ID below was verified against the core roster (`sgma_gsa_full.csv`) and every
"current bag" against `entity_bag_map.csv`.** Nothing here has been applied — this
is a review sheet for your manual pass.

Confidence key: **[C]** = roster-confirmed, safe to apply · **[L]** = worth-a-look, needs your judgment / source-doc check.

Counts: 5 false folds (all [C]) · 22 missed matches [C] + ~32 [L] · `bag_merges.csv` RETIRED (see §3) · 5 clusters with no roster id.

> **GATE 1 CLOSED — user sign-off 2026-10-07.** `entity_bag_overrides_gsa.csv` is frozen at **214 rows** (0 parent_bag_id mismatches, 0 duplicate variants). Pre-Step-2 data-hygiene gate also PASSED: the per-document mention source is the core per-doc weighted graphs (`core_rds_for_stem(core_igraph_weighted(), <gsp_doc_id>)`), keyed by **document stem** (canonical-safe), 185/185 docs present; `node_dictionary.csv` supplies type only; `id_crosswalk.csv` supplies per-doc `gsa_ids` + `canonical_gsp_id`. The stale `all_gsa_edges.csv` is NOT an input to Step 2. Step 2 (`build_gsa_bag_edges.R`) is now unblocked.

---

## 0. BINDING DESIGN DECISION — strict DWR authorship  (decided 2026-10-07)

**Authorship, not mention, defines membership in the matrix.** The authoritative
authoring set for each plan is the DWR-declared GSA **ids** in `id_crosswalk.csv`
(`gsa_ids`, comma-delimited; 259 distinct affiliated ids corpus-wide) — the core
underlying DWR data. Join on the **ids**, not `gsa_names`: that name column is NOT
parallel to `gsa_ids` (62-row mismatch) and is display-only; canonical names come
from core `sgma_gsa_full.csv` (GSA_ID→GSA_Name). Step 2 (`build_gsa_bag_edges.R`)
must **keep a resolved `gsa`-row only if its `bag_id` (after overrides + bag_merges)
is one of that plan's crosswalk `gsa_ids`.**
A `gsa`-row that resolves to a free-text / identity bag (no roster `gsa_###`), or to a
`gsa_###` that DWR did not list for that plan, is a **mention, not an author**, and is
dropped. The authoring-GSAs × entities matrix is then colwise-summed to one row per
GSP document and crossed to form the GSP × GSP matrix.

Consequences for the sections below:
- **§2 folds are load-bearing, not cosmetic.** Folding a variant onto its correct
  `gsa_###` is what lets it be *counted as an author* when DWR listed that id for the
  plan. Left at identity it silently **washes out of the authoring set**.
- **§5 no-roster-id clusters are non-authors by construction** (KGA, North Delta,
  McFarland, Pauma Valley, Salinas/SVBGSA umbrella) — none appear in any plan's
  `gsa_names`, so a strict crosswalk-`gsa_id` filter can never admit them. Identity is
  their correct terminal state; umbrella coordination is represented through the
  member GSAs' shared profiles, not the umbrella itself (avoids double-counting).
- **Worked example — Kern Groundwater Authority (umbrella JPA).** Appears as a
  `gsa`-row in 14 analyzed plans' text; authors **0** — it has no roster `GSA_ID`
  (0 rows in `sgma_gsa_full.csv`), so it is absent from every plan's `gsa_ids` and can
  never pass the authorship filter. Its three identity-override rows are kept
  specifically to block the false fold into **gsa_306 Kern Non-Districted Land
  Authority GSA — a declared author of plan 0036 (`gsa_ids`=277,39,188,306,326)** —
  which would otherwise misattribute KGA's umbrella-level mentions to a member's
  authoring profile.

---

## 1. FALSE FOLDS — active errors, two GSAs merged into one bag  (fix first)

Same bug class as your Sacramento-Central fix: a short shared token drove a `revsubset`/`subset` match. Each row is currently contaminating the wrong agency's tie profile.

| variant (currently folded) | current bag (WRONG) | belongs to | fix | conf |
|---|---|---|---|---|
| `western_management_gsa` | gsa_120 Western Canal Water District GSA | **gsa_5** Santa Ynez River Valley Basin Western Management Area GSA | override → gsa_5 | [C] |
| `central_management_gsa` | gsa_211 Central Kings GSA | **gsa_11** Santa Ynez River Valley Basin Central Management Area GSA | override → gsa_11 | [C] |
| `pleasant_valley_basin_outlying_areas_gsa` | gsa_190 Pleasant Valley GSA | **gsa_353** County of Ventura GSA – Pleasant Valley Basin Outlying Areas | override → gsa_353 | [C] |
| `county_of_santa_cruz_gsa` | gsa_24 Santa Cruz Mid-County GA GSA (JPA) | **gsa_283** County of Santa Cruz GSA (member, not the JPA) | override → gsa_283 | [C] |
| `siskiyou_county_flood_control_and_water_conservation_district` | gsa_256 County of Siskiyou GSA | **gsa_253** Siskiyou County Flood Control & WCD GSA | override → gsa_253 | [C] |

Notes: #1/#2 matched on the bare token `western`/`central` after "management" was dropped as non-distinctive. #4 is umbrella-vs-member (the county GSA is a *member* of the Mid-County JPA). #5 county-vs-its-own-district — distinct roster ids.

---

## 2. MISSED MATCHES — identity strings that should fold onto a real GSA  (currently washing out)

All currently `method=identity` (so they never join the affiliated set downstream). Targets roster-verified.

### 2a. Highest value — Eastern Tule cluster → **gsa_205 Eastern Tule GSA**  [C]
~13 identity strings for one affiliated GSA: `eastern_tule_gsa` (exact), `east_tule_gsa`, `eastern_tule_groundwater_sustainability_agency`(+`_gsa`), `eastern_tule_gsa_joint_powers_authority`, `eastern_tule_gsa_sustainability_agency`, `eastern_tule_gsa_a1`, `_d30`, `_3_19`, `_section_437`, `eastern_tule_groundwater_sustainability_agency_board`. → all gsa_205.

### 2b. Exact normalized matches to a real GSA  [C]
| variant | → target |
|---|---|
| `alameda_county_water_district_gsa` | gsa_95 Alameda County Water District GSA |
| `heritage_ranch_community_services_district`(+`_gsa`) | gsa_197 Heritage Ranch Community Services District |
| `estrella_el_pomar_creston_water_district_gsa` | gsa_396 Estrella-El Pomar-Creston Water District GSA |
| `city_of_pomona_gsa` | gsa_368 City of Pomona GSA |
| `city_of_marina_gsa` (+ `city_of_marina_groundwater_sustainability_agency`) | gsa_399 City of Marina GSA |
| `city_of_lakeport_gsa` | gsa_333 City of Lakeport GSA |
| `los_angeles_gsa` | gsa_293 Los Angeles GSA |
| `monterey_peninsula_water_management_district_gsa` | gsa_8 Monterey Peninsula Water Mgmt District GSA |
| `pajaro_valley_water_management_agency_gsa` | gsa_6 Pajaro Valley Water Mgmt Agency GSA |
| `san_diego_river_valley_gsa` (+ `san_diego_river_valley`) | gsa_371 San Diego River Valley GSA |
| `san_timoteo_subbasin_gsa` | gsa_344 San Timoteo Subbasin GSA |
| `vandalia_water_district` | gsa_530 Vandalia Water District |
| `tea_pot_dome_water_district` | gsa_528 Tea Pot Dome Water District |
| `porterville_irrigation_district_gsa` | gsa_541 Porterville Irrigation District GSA |
| `midkaweah_groundwater_sustainability_agency` | gsa_7 Mid-Kaweah GSA |
| `delanoearlimart_irrigation_district_groundwater_sustainability_agency` | gsa_29 Delano-Earlimart Irrigation District GSA |

### 2c. `wd` = "water district" (matcher didn't expand the abbreviation)  [C]
`gravelly_ford_wd_gsa`→gsa_34 · `new_stone_wd_gsa`→gsa_59 · `root_creek_wd_gsa`→gsa_33 · `madera_wd_gsa`→gsa_41 (Madera *Water District*, distinct from County-of-Madera gsa_68/69/70).

### 2d. Westside trailing doc-text  [C]
`westside_districts_water_authority_gsa_board_of_directors`, `..._groundwater_sustainability_plan` → gsa_506 (joins the existing override cluster).

---

## 3. MERGE-FAMILY issues — RESOLVED: `bag_merges.csv` RETIRED  (decided 2026-10-07)

**`bag_merges.csv` has been emptied (header only).** All 8 merge families were a global
roster-id → roster-id remap that is incompatible with the strict DWR authorship decision
(§0) for two independent reasons:

1. **Washout.** The authorship join is on the *raw* roster `gsa_id` per document. A merge
   rewrites a member id to its parent, so a `gsa`-row that correctly resolved to a member
   fails the `bag_id ∈ plan.gsa_ids` test whenever the plan listed the member but not the
   parent → the real author silently drops. Audited against the crosswalk, **all 8 families
   are harmful**: 6 collapse ids that author *different* documents (Tehama 124/418/419/420/422
   → plans 137/94/140/139/134; Enterprise-Anderson 287/464 → 82/83; Colusa 323/467 → 92/98;
   Fillmore-Piru 339/468 → 73/72; Tri-County 411/490 → 42/57; Sacramento 496/497 → 106/117),
   and 2 (RD-1004 Butte 138/416 → plan 98; San Joaquin 142/389 → plan 47) collapse two ids
   DWR co-lists as distinct authors of the *same* document (no washout, but under-counts that
   document's author team 2→1).
2. **Document-varying teams.** The author set is a property of the *document*, not the plan:
   3/120 plans (canonical 150, 50, 7) have documents that disagree on their `gsa_ids`. A global
   id→id table structurally cannot express "these two ids are one author in doc A but the team
   differs in doc B." Author sets must be read per `gsp_doc_id` from the raw crosswalk.

Correct division of labor: **overrides + the bag map fix name-variant → correct roster id**
(load-bearing — see §2); **roster-id → roster-id merging is not done at all.** The crosswalk
`gsa_ids` per `gsp_doc_id` defines each document's author set verbatim.

Downstream consequences:
- **Tehama 421/423 add — DROPPED.** They author no document (0 crosswalk rows); nothing to fold.
- **Candidate merges `gsa_110` / `gsa_146` / `gsa_517`/`gsa_518` — MOOT.** No merging.
### 3a. Multi-MA orgs — a GLOBAL single-id pin is the same anti-pattern (decided 2026-10-07)

The two former `_gsa` flags (Sacramento `→ gsa_496`, Turner Island `→ gsa_223`) were going to be
re-targeted to a single "better" id. **That instinct is wrong and is now retracted.** Turner Island
WD is ONE real organization operating TWO management-area GSAs — gsa_223 (Merced) and gsa_220
(Delta-Mendota) — that author *different* documents; its generic name string legitimately belongs to
**both** documents. A global string→single-id pin can only ever match one of them, so it mis-assigns
the org's mention wherever a *different* MA id of the same family authored the plan. This is the exact
document-dependent-identity defect that retired `bag_merges` (§3) — just hiding in the override file.

**Roster scan: ~41 multi-MA orgs** (one base name, >1 roster `GSA_ID` differing only by a
" – <Management Area>" suffix): Turner Island {220,223}; Sacramento County {295,496,497}; Tehama
{124,418,419,420,421,422,423}; plus Imperial (×13), Salinas (×6), Fox Canyon (×4), and more.

**Audit of the 8 leftover single-id override pins** (mention columns in `all_gsa_edges.csv` × author
ids per `canonical_gsp_id` from the crosswalk) — **all 8 mis-assign under document-level authorship:**

| family (pin) | generic string mis-assigns to another family id in plan(s) |
|---|---|
| Turner Island (gsa_223) | generic `turner_island_water_district_gsa` in plans 13, 15 → **220** (the `_gsa1*` strings only in plan 9 → 223, so those ARE correct) |
| Tehama (gsa_124) | `tehama_gsa` in 94→418, 139→420, 140→419, 134→422 |
| Sacramento County (gsa_496) | `*sacramento_county_gsa` in 111→**295**, 117→**497** |
| Fillmore-Piru (gsa_339) | plan 72 → **468** |
| Enterprise-Anderson (gsa_287) | plan 83 → **464** |
| Eastern San Joaquin (gsa_142) | plan 47 → 142/389, plan 122 → **146** |
| RD-1004 (gsa_138) | plan 98 → 138 **+** 416 (same-doc co-authors) |
| Tri-County (gsa_490) | plan 42 → **411** |

**Correct mechanism — per-`gsp_doc_id` resolution in Step 2, NOT a global pin:**
1. Authorship comes straight from the crosswalk `gsa_ids` per document (bare numeric; bag unit = the
   document). No string resolution is needed to KNOW a document's author set.
2. A mention-string that maps to a multi-MA org resolves *per document* to whichever family member(s)
   are in **that doc's** `gsa_ids`. The override/map's single `bag_id` for such a string is therefore
   only a **pointer into the family** (e.g. `gsa_124` ⇒ the Tehama family), expanded per-doc at build
   time from the roster base-name grouping. If no family member authors the doc → pure mention → drop.
3. **Same-document co-authors → keep ALL of them** (user decision 2026-10-07): where DWR co-lists two
   family ids on one document (RD-1004 plan 98 = 138+416; Eastern SJ plan 47 = 142+389), every
   co-author's names go into that document's bag for the document × alters aggregation — do NOT pick one.

**Consequence for the override file — KEEP the pins (decided 2026-10-07):** the single-id pins must NOT
drive resolution as written, but they are **kept** as documented family pointers. Audit against the auto
map `entity_bag_map.csv`: all 33 override rows pointing into a multi-MA family are *also* present in the
auto map (32 fold to the identical member; only `tri_county_water_authority_gsa_jpa` differed, 490-vs-411,
both same-family). So no pin is uniquely load-bearing for *presence* — yet deleting them is a cosmetic
no-op, because the auto map still points into the family and Step 2 must expand per-doc regardless; which
member is the pointer is immaterial (the base-name family is recovered either way). Deleting curated
rows to change nothing is the wrong trade, so they stay. No override re-target is warranted — the only
correctness lever is Step 2's per-doc expansion off the roster family map. (The earlier "retarget row 59
→ gsa_295" recommendation is withdrawn; the same string `county_of_sacramento_gsa` routes to **295** in
plan 111 but **497** in plan 117, so no single retarget could be right.)

Per-plan routing confirmed on real document authorship (`allpins_confirm.R`): every pin is correct for
only a minority of its authoring plans — Fillmore 73✓ / 72→468 / 19 drop; Enterprise 82✓ / 83→464;
Eastern SJ 47→**142+389** (co-auth) / 122→146 / 106,117,85 drop; RD-1004 98→**138+416** (co-auth);
Tehama 137✓ / 94→418 / 140→419 / 139→420 / 134→422 / 82,83,86,92,96,117 drop; Sacramento 106✓ /
111→295 / 117→497 / 100,47 drop; Tri-County 57✓ / 42→411 / 5 drop. Phantom ids that never author
(in no doc's `gsa_ids`): **110, 421, 423**.

**Tri-County B2 vestige — FIXED (2026-10-07):** line 22 `tri_county_water_authority_gsa_jpa` had
`parent_bag_id=gsa_411` ≠ `bag_id=gsa_490`; aligned the parent fields to `gsa_490` /
`Tri-County Water Authority GSA - Tule` (mirroring bag_id/canonical_label as every other row does).
B2 scan now reports **0** `parent_bag_id != bag_id` mismatches across all 117 rows.

---

## 4. WORTH-A-LOOK missed matches (strong but <exact; needs your eye)  [L]

**Groups A–C PROMOTED (2026-10-07, Gate 1 sign-off):** 48 agency-name variants folded into the override
file → 15 roster ids (gsa_15 ×17, gsa_62 ×5, gsa_217/260/267/273/281/314/342/347/348/349/352/357/390 the
rest). Override now 165 rows; 0 parent_bag_id mismatches; 0 duplicate variants. Each glob was resolved to
its actual normalized strings and **curated to agency-denoting forms only** — geographic/other-entity/
plan-title hits were deliberately EXCLUDED, not folded: Buena Vista `_lake`/`_lakebed`/`_slough`/`_school`/
`_aquatic_*recreation_area`/`_rancheria_mewuk_indians` (tribe)/bare `buena_vista`/`sbvgsa`; Grassland
`_drainage_area_coalition`/`_resource_conservation_district`/`_bypass_project`/`_ecological_area`/
`_wildlife_*`/bare `grassland`/`grassland_water_district(_board)`/`grasslandwd` (the underlying district,
not the GSA)/`_groundwater_sustainability_plan`/`_plan_area`; Yucaipa bare `yucaipa_subbasin`/
`letter_for_yucaipa_subbasin_draft`; Bedford `_basin`/`_groundwater_sustainability_plan`.

**Groups D & E PROMOTED (2026-10-07, Gate 1 sign-off):** 49 more variants folded → 12 roster ids; override
now **214 rows** (0 parent_bag_id mismatches, 0 dup variants). Adjudicated against REAL document authorship
(`resolve_groupsDE.R`): for each candidate string, which plans it appears in and whether the proposed roster
id actually authors those plans (strict-authorship test — a fold is load-bearing where the target is an
author of a crosswalk-joinable plan the string occurs in). **CAVEAT on the proxy (2026-10-07):**
`all_gsa_edges.csv` was built 2026-09-03, BEFORE the canonical-id consolidation (crosswalk is 2026-09-10).
It carries 132 `gsp_id`s on an OLD pre-canonical enumeration; 12 of them (160, 164–174) do NOT join to the
current crosswalk (max canonical id 157), and those 12 carry **26% of edge rows** (625/2366) — the large
Kern/Tulare/East-Kaweah/Delta-Mendota coordination docs. Authorship of THOSE docs could not be read through
this stale proxy. The 49 promotions below are all anchored on crosswalk-joinable ids (≤157), so they stand;
but any "inert" verdict that rests on a string appearing ONLY on 160/164–174 is UNCONFIRMED, not proven. Promoted (agency-denoting + ≥1 live AUTH plan): Pixley
`pix*`/`pixid*`/`pixley_irrigation_district(_gsa)`→gsa_43 ×10 (AUTH 65) · `ltridgsa`→gsa_42 (AUTH 56) ·
`deidgsa`→gsa_29 (AUTH 63) · Indian Wells `iwv*`/`indian_wells_valley_groundwater_authority*`/bare
`valley_groundwater_authority`→gsa_35 ×11 (AUTH 59) · McMullin `mcmullin_area_*`→gsa_266 ×5 (AUTH 28) ·
Semitropic `semitropic_water_storage_district_gsa`/`semitropic_gsa`→gsa_277 ×2 (AUTH 36,150) ·
`rd1001_gsa`→gsa_247 (AUTH 100) · `kre_gsa`→gsa_18 (AUTH 23) · Westlands `westlands_*`→gsa_40 ×6 (AUTH 8) ·
Arroyo Seco `arroyo_seco_*`→gsa_261 ×6 (AUTH 116) · `marina_coast_water_district_groundwater_sustainability_agency`→gsa_50
(AUTH 128) · Owens Valley `owens_valley_groundwater_authority_*`→gsa_403 ×4 (AUTH 103).

The two E ambiguities DISSOLVED under authorship: **marina** — the only live string (`marina_coast_water_district_*`)
is unambiguously Marina Coast WD and 50 authors plan 128; the bare `marina_groundwater_sustainability_agency*`
forms are inert (plan 29, neither 50 nor 399 authors) → left as identity. **valley GWA** — full
`indian_wells_valley_*` → 35 (AUTH 59), full `owens_valley_*` → 403 (AUTH 103), and bare
`valley_groundwater_authority` occurs ONLY in plan 59 (35 authors, 403 does not) → 35.

REJECTED with cause: **`smga`→338** — `smga` occurs only in plan 38, authored by 231/62 (Delta-Mendota), NOT
Santa Margarita; folding would fabricate a co-mention. **`mb_gsa`/`mbgsa`→354** — occur only in plan 155,
authored by 404 (Montecito) — which is Montecito Basin, a DIFFERENT agency than Mound Basin (Ventura).
Both rejections rest on VALID crosswalk joins (plans 38 & 155 are ≤157). SKIPPED as inert via valid joins
(target is not an author of the plans where the string appears, so strict authorship drops it): `deid_gsa`→29
(plans 48,57), `mullin_area_*`→266 (plan 94), `westland_water_district`→40 (plans 15,20); and um-only
(absent from the edge matrix entirely): `ttwdgsa`→394, `wkwd_gsa`→39. **DEFERRED, not folded (authorship
unconfirmable through the stale proxy):** `wrm_gsa`→484 and bare `storage_district_gsa`→277 occur ONLY on
edge ids 167–174, the unjoinable pre-canonical Kern docs — `wrm_gsa` (Wheeler Ridge-Maricopa) is almost
certainly a real author-reference there, but I cannot confirm gsa_484's authorship of those docs until the
edges are rebuilt on canonical ids, so I did not fold it speculatively (also hedges a possible spurious
alter-side tie). Bare `storage_district_gsa` additionally rejected as a non-unique suffix fragment (matches
Semitropic/Rosedale/Buena Vista/North Kern). EXCLUDED as non-agency within the globs: Pixley
`_national_wildlife_refuge`/`_public_utility_district`/`pixleytotal`/`pix_general_planc`/`_community_plan_update`/bare
`pixley`; Semitropic `_bank`/`_ridge_preserve`/`_groundwater_bank*`/`_water(_storage)` fragments/`_kern_groundwater_authority`/bare
`semitropic`; Westlands `_groundwater_sustainability_plan`/bare `westlands`; Arroyo Seco `_river`/`_gravels`/`_cone(_management_area)`/`mcwd_gsa_*`;
Owens `_communications_and_engagement_plan`. **Out-of-scope finds (not folded):** `rosedale_rio_bravo_water_storage_district_gsa`
and `north_kern_water_storage_district_gsa` are real GSAs co-authoring the Kern plans and resolve to their OWN
roster ids (not 277) — flag for a dedicated pass if wanted.

**STEP-2 DATA-HYGIENE FLAG (2026-10-07):** `all_gsa_edges.csv` is a STALE product of the OLD pipeline
(2026-09-03, pre-canonical; retired with `build_gsa_edges.R` in Step 4). 26% of its rows sit on 12 `gsp_id`s
that no longer exist in the crosswalk. It was used in this review ONLY as a convenience proxy to spot-check
co-occurrence; it is NOT a valid Step-2 input. Step 2 (`build_gsa_bag_edges.R`) must build the doc×variant
structure from the CURRENT `gsp_doc_id`-keyed sources (per-doc mentions + crosswalk `gsa_ids`), never from
`all_gsa_edges`. Confirm the Step-2 mention source is canonical-keyed before running.

Subbasin/"GSA"↔spelled-out swaps: `temescal_*_groundwater_sustainability_agency`→gsa_260 · `corning_sub_basin_*`→gsa_390 · `bedford_coldwater_*authority`→gsa_267 · `ukiah_valley_ground_water_*`→gsa_342 · `yolo_sub_basin_groundwater_agency`/`yolo_sga`→gsa_217 · `yucaipa_*_subbasin`/`yucaipa_smga`→gsa_349.

Buena Vista cluster (8 doc-suffixed + abbrevs `bvgsa`/`bvsa`/`bvsga`)→gsa_15. · `eastern_kaweah_groundwater_sustainability_agency`→gsa_314 (East≈Eastern). · `grassland_water_district_gsa`/`grassland_*`→gsa_62. · `santa_ynez_ema_gsa`→gsa_273 (EMA=Eastern Mgmt Area). · URL/email fragments `*greaterkaweahgsaorg`→gsa_281.

Ventura/Camrosa management areas: `oxnard_outlying_area(s)_gsa`→gsa_352 · `las_posas_valley_outlying_areas_gsa`→gsa_357 · `camrosa_las_posas_gsa`→gsa_348 · `camrosa_opv*`→gsa_347.

Abbreviation/OCR expansions (lower confidence): Pixley `pix*`/`pixid*`→gsa_43 · Lower Tule `ltrid*`→gsa_42 · Delano-Earlimart `deid*`→gsa_29 · Indian Wells `iwv*ga`→gsa_35 · `mullin_area_*`(McMullin)→gsa_266 · `semtrop_*`→gsa_277 · `rd1001_gsa`→gsa_247 · `ttwdgsa`→gsa_394 · `wrm_gsa`→gsa_484 · `wkwd_gsa`→gsa_39 · `kre*`→gsa_18 · `westland_gsa`→gsa_40 · `smga`→gsa_338 (weak) · `mb*`→gsa_354 (weak) · OCR-corrupt Arroyo Seco strings→gsa_261.

Also from §1 agent (ambiguous, left alone): `marina_groundwater_sustainability_agency` (gsa_50 vs gsa_399 city) · `valley_groundwater_authority` (gsa_403 vs gsa_35) · bare `storage_district_gsa` fragments in gsa_277 (non-unique).

---

## 5. Same-entity clusters with NO roster id (decision needed — identity is correct unless you add a bag)

- **North Delta GSA**: `north_delta_gsa`(+`_board`), `northern_delta_*`, `n_delta_gsa`, `nd_gsa`, `ndgsa` — one agency, not in roster.
- **McFarland GSA**: `mcfarland_*` — not in roster.
- **Pauma Valley GSA**: `pauma_valley_*` — members of Upper San Luis Rey GMA (gsa_359); no standalone roster entry.
- **Colusa+Glenn two-authority JPA**: `colusa_groundwater_authority_and_glenn_groundwater_authority` + Spanish `autoridad_*` — a combo of two authorities, correctly NOT a single GSA.
- **Salinas Valley Basin umbrella**: `svbgsa`-family + ~25 `salinas_valley_*` doc strings — multi-MA JPA (roster splits into gsa_268, 458–462), analogous to the Kern umbrella; keep identity unless MA-level resolution is wanted.

---

## Same-agency-across-MA note — RESOLVED by §0/§3 (keep MA-level ids distinct)
Under document-level strict authorship the management-area split is exactly what we want:
each MA id that DWR lists as a document's author counts as that document's author. Do NOT
collapse an agency's MA ids into one generic bag (that is the retired `bag_merges` error).
The earlier concern — Salinas Valley (gsa_458 + MA ids), Fox Canyon GMA (gsa_433), County of
Fresno (gsa_408), Santa Clara Valley WD (gsa_345), County of Madera (gsa_69) — is moot: keep
the roster ids distinct and let the per-`gsp_doc_id` `gsa_ids` decide membership.
