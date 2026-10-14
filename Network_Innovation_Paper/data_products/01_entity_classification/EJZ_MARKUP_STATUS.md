# EJZ markup of GSA_BAGS_review — status against the current bag map

Source: `GSA_BAGS_review_EJZ_markup.xlsx`, EJZ's markup of an earlier `GSA_BAGS_review.txt`.
Codes are cell fills on the variant line; the key is in columns D–E, and comments are in column B.
Checked against `entity_bag_map.csv`, `entity_bag_unmatched.csv` and
`inputs/entity_bag_overrides_gsa.csv` on branch `entity-classify-update` (2026-10-14).

| Code | Meaning |
|---|---|
| P | could be just a geographic or geopolitical entity (city, valley, county, basin) |
| O | name could be a subset of multiple GSA names |
| Y | could be a non-entity |
| B | abbreviation; unsure which GSA is meant |
| R | matched to a subset of a single, wrong GSA name |
| G | correct, but should also match another GSA ("and", "agencies") |
| L | probably fine; wants a second look |

EJZ flagged 43 names and left comments on 6 more without a code.

| Status | Count |
|---|---|
| Obviated by the guards (no longer folds) | 26 |
| Fixed by a reviewed override | 2 |
| Override removed (no longer folds) | 1 |
| Resolved 2026-10-14: multi-GSA names, generic names, `b_gsa` | 13 |
| Still open | 3 (`el_rico` regression, Northern Delta-Mendota members, gsa_458 tags) |

## Obviated: the name no longer folds into any bag

Each of these now sits in `entity_bag_unmatched.csv`, held by the bare-name, `subset_geo`
or `no_gsa_marker` guard, or left as ambiguous with several candidate bags.

| Code | Variant | Was in | EJZ comment |
|---|---|---|---|
| P | `kern_river` | gsa_20 Kern River | |
| P | `ukiah_valley_basin` | gsa_342 | |
| P | `cuyama_basin` | gsa_327 | |
| P | `yolo_subbasin` | gsa_217 | |
| P | `pleasant_valley` | gsa_190 | |
| P | `solano_subbasin` | gsa_340 | |
| P | `arvin` | gsa_481 | |
| P | `san_gorgonio_pass` | gsa_262 | |
| P | `santa_clarita_valley` | gsa_315 | |
| P | `spadra_basin` | gsa_212 | |
| P | `big_valley` | gsa_480 | |
| P | `city_of_madera` | gsa_47 | |
| P | `san_pasqual_valley` | gsa_370 | |
| P | `elsinore_valley` | gsa_21 | |
| P | `county_of_fresno` | gsa_408 | |
| O | `river_gsa` | gsa_20 | also Mid-Kings River GSA |
| O | `east_gsa` | gsa_314 | also Kings River East GSA |
| O | `gsa_east` | gsa_314 | also Kings River East, East Acton, etc. |
| O | `gsa_west` | gsa_209 | |
| O | `west_gsa` | gsa_209 | |
| O | `mendota_gsa` | gsa_67 | |
| Y | `north_kings_groundwater` | gsa_65 | |
| B | `merced_gsa` | gsa_219 | could be County of Merced or Merced Irrigation-Urban |
| B | `glenn_gsa` | gsa_369 | could be County of Glenn GSA |
| R | `of_merced_gsa` | gsa_219 | this is County of Merced GSA |
| L | `james` | gsa_12 | |

The guards now drop these names instead of folding them. A positive fold still needs an override:
- `of_merced_gsa` should go to a County of Merced GSA bag, per EJZ. That is a family of
  several management areas (gsa_191, gsa_231, …), so it needs a family-level override.
- `mendota_gsa` is a candidate override to gsa_67, City of Mendota. EJZ marked it O, so
  it stays out unless someone confirms it.

## Fixed by a reviewed override

| Code | Variant | Was in | Now | EJZ comment |
|---|---|---|---|---|
| R | `central_management_gsa` | gsa_211 Central Kings | gsa_11 Santa Ynez Central Mgmt Area | "I think this is Santa Ynez … Central Management Area GSA" |
| R | `western_management_gsa` | gsa_120 Western Canal WD | gsa_5 Santa Ynez Western Mgmt Area | "This is Santa Ynez RVBWMA GSA" |

## Override removed

| Code | Variant | Was in | Now |
|---|---|---|---|
| P | `county_of_tulare` | gsa_470 County of Tulare GSA - Tule (override) | unmatched (`no_gsa_marker`) |

The bare name could mean the county government, so the override row was deleted
(2026-10-14). `county_of_tulare_gsa` still folds to gsa_470 by override. `gsa_bag_edges.csv`
did not change.

## Resolved 2026-10-14

**Names that refer to several GSAs (G) count in each GSA's bag.** Each gets one override
row per GSA. The GSA lists come from the authors of the document that uses the name, which
matched EJZ's notes. Strict authorship still applies per document.

| Variant | Bags |
|---|---|
| `mcmullin_and_james_groundwater_sustainability_agencies` | gsa_12 James, gsa_266 McMullin Area |
| `carpinteria_montecito_gsa` | gsa_491 Carpinteria, gsa_404 Montecito |
| `corning_subbasin_groundwater_sustainability_agencies` | gsa_390, gsa_418 Tehama County FCWCD (gsa_108 dropped, see below) |
| `pleasant_valley_subbasin_groundwater_sustainability_agencies` | gsa_190, gsa_408 County of Fresno - PV, gsa_410 City of Coalinga |
| `solano_subbasin_groundwater_sustainability_agencies` | gsa_340, 180, 272, 384, 385, 497 |
| `vina_subbasin_groundwater_sustainability_agencies` | gsa_409 Vina, gsa_178 Rock Creek RD |
| `san_gorgonio_pass_groundwater_sustainability_agencies` | gsa_262, gsa_330 Verbenia, gsa_450 Desert Water Agency - SGP |
| `cosumnes_groundwater_sustainability_agencies` | all seven Cosumnes GSAs (97, 155, 160, 182, 193, 312, 496) |
| `northern_and_central_delta_mendota_groundwater_sustainability_agencies` | gsa_206 Central Delta-Mendota only (see Still open) |

The Corning row for gsa_108 (County of Glenn GSA - Corning, formed 2015) was removed. It
appears to be the predecessor of gsa_390 (Corning Subbasin GSA, 2017), and it authors no
document.

In `gsa_bag_edges.csv`, only `carpinteria_montecito_gsa` and the San Gorgonio Pass name
carry ties. The other seven names have no ties in their documents, so they add nothing to
the edges.

**Generic names (O) no longer fold.** A new `revsubset_generic` guard holds any variant
whose distinctive tokens are all entity-type words. That covers `storage_district_gsa`,
`conservation_district_groundwater_sustainability_agency`,
`storage_district_groundwater_sustainability_agency` and
`water_storage_district_groundwater_sustainability_agency`. The last two were previously in
gsa_277 Semitropic. EJZ had confirmed `district_water_authority_gsa` (→ gsa_506) and
`county_water_authority_…` (→ gsa_490) as unique, so they fold by override instead.

**Short tokens (L/O) no longer fold.** `revsubset_bare` now also holds a single
distinctive token shorter than three characters, so `b_gsa` and `el_gsa` do not fold.

**Kept:** `cawelo_groundwater_sustainability` → gsa_326, since Cawelo is unique.

## Still open

- `el_rico` (regression below).
- **"Northern" in `northern_and_central_delta_mendota_…_agencies`.** EJZ could not tell
  which GSAs "Northern" means. The name has no ties, so this does not affect the edges.
- **gsa_458 header comment** (Salinas Valley Basin GSA - Monterey): "if we ever
  supernetwork, do we want these to get monterey tags?" This is a modeling decision that
  the bag map does not settle.

## Regression: a name EJZ accepted no longer folds

| Variant | Was | Now | EJZ comment |
|---|---|---|---|
| `el_rico` | gsa_358 El Rico | unmatched (`no_gsa_marker`) | "fine since El Rico isn't a town name" |

The bare-name rule drops it. Add an override row for `el_rico` → gsa_358 to restore it.

## Accepted by EJZ and unchanged

- `north_kings_garfield_water_district_groundwater_sustainability_agency` → gsa_65
- `district_water_authority_gsa` → gsa_506 (now by override)
- `henry_miller_water_district_groundwater_sustainability_agencies` → gsa_254
- `tejon_castac_water_district_groundwater_sustainability_agencies` → gsa_482
- `county_water_authority_groundwater_sustainability_agencies` now folds by override to gsa_490,
  Tri-County Water Authority - Tule, not gsa_411. No variant maps to gsa_411 any more.
  It is still Tri-County, so EJZ's comment ("only matches tri county") holds.
