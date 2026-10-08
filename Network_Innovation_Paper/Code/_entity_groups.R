#' _entity_groups.R — single source of truth for entity-type groupings used by
#' the modeling scripts (make_binary0.9; explore/make_networks, explore/make_valued_networks).
#'
#' Every name in node_dictionary.csv has ONE type from the seven in
#' classify_entities.R (GSA, Consultant, Research, NGO, Institutional_other,
#' Institutional_unresolved, Non_institutional). Two of them never go into a
#' network, so every grouping below leaves them out: Non_institutional (not an
#' institution) and Institutional_unresolved (an institution named too vaguely to
#' say which one -- "university", "the district" -- so two plans mentioning it
#' aren't really connected). The `institutional` group used for the general network
#' is therefore the four org types + Institutional_other.
#'
#' Groupings (see build_shared_entity_matrix() for how they become gsp x gsp
#' shared-entity matrices):
#'   institutional          generic network: all institutional actors
#'   Consultant/Research/NGO the three focal subnetworks (each on its own)
#'   consultant_research_ngo the 4th "grouped" run: the three focal types pooled

# --- semantic-type groupings --------------------------------------------------

# The institutions that go into a network = the four org types + Institutional_other.
# Two types are deliberately left out because they never form a real tie:
#   - Non_institutional        not an institution (people, basins, features,
#                              infrastructure, projects, models, citations, OCR junk).
#   - Institutional_unresolved an institution named too vaguely to say which one
#                              ("university", "the district", "a consultant"). Two
#                              plans mentioning it aren't really connected, so it
#                              must stay out -- that is the whole point of the type.
# Institutional_other is a specific institution that isn't a GSA, consultant,
# research org, or NGO (a named city, county, district, government body, company, or
# committee).
INSTITUTIONAL_TYPES <- c(
  "GSA", "Consultant", "Research", "NGO", "Institutional_other"
)

# The three focal subnetworks and their pooled ("grouped") 4th run.
FOCAL_TYPES <- c("Consultant", "Research", "NGO")

# --- non-actor entities, excluded from every shared-entity matrix --------------

# Bare state/nation placenames. The classifier types these Institutional_other
# because they are proper nouns naming a polity, but as a shared-entity TIE they
# carry no information: every GSP is written in California and in the United
# States, so co-mention is true by construction, not a choice either plan made.
# This is a different objection from prevalence. A ubiquitous entity may still be
# a real actor -- DWR is in 115/118 plans precisely because it administers SGMA,
# and that ubiquity is substantive -- whereas "California" is not an actor in the
# plan at all. So the exclusion is by identity, not by frequency; see
# build_shared_entity_matrix(), which no longer filters on prevalence.
#
# Listed in BOTH the raw node_dictionary spelling and the `io_` bag id, so the
# exclusion holds before and after the modeling stage moves onto bags.
# Scope decision (2026-10-08): state/nation only. County names are NOT here --
# two plans both naming Kern County share real co-location, and counties are
# frequently governmental actors in these plans. The other bare-geography
# spellings (america, usa, north_america, ca, u_s, cal, american, states) need no
# entry: the classifier types them Non_institutional, and "state" is
# Institutional_unresolved, so no grouping admits them in the first place.
NON_ACTOR_ENTITIES <- c(
  "california", "calif", "united_states", "united_states_of_america", "us",
  "io_california", "io_calif", "io_united_states",
  "io_united_states_of_america", "io_us"
)

# Named group -> the set of semantic types it selects.
ENTITY_GROUPS <- list(
  institutional           = INSTITUTIONAL_TYPES,
  Consultant              = "Consultant",
  Research                = "Research",
  NGO                     = "NGO",
  consultant_research_ngo = FOCAL_TYPES
)

#' Names in `dict` whose entity_type belongs to a named group (or an explicit
#' vector of types).
#' @param dict data.table/data.frame with columns name, entity_type
#' @param group either a name in ENTITY_GROUPS or a character vector of types
entity_names <- function(dict, group) {
  types <- if (length(group) == 1L && group %in% names(ENTITY_GROUPS))
    ENTITY_GROUPS[[group]] else group
  unique(dict$name[dict$entity_type %in% types])
}

# --- gsp x gsp shared-entity matrix -------------------------------------------

#' Build the symmetric gsp x gsp shared-entity matrix for a set of entity names.
#'
#' Mirrors the long-standing per-category construction in the modeling scripts:
#'   1. keep only the gsa-edge columns that are entity names in `names`
#'   2. drop non-actor entities (NON_ACTOR_ENTITIES: bare state/nation placenames)
#'   3. cast to a gsp_id x entity incidence matrix (summed parsed-relation count)
#'   4. binarize the incidence matrix: relation observed / not
#'   5. tcrossprod -> gsp x gsp count of entities both plans have a relation to
#'
#' THE CONSTRUCT (decided 2026-10-08, user). A cell counts the entities for which
#' BOTH plans record an OBSERVED RELATIONAL EVENT -- a source->target relation
#' extracted from sentence syntax and dependency parsing (textNet SVO triples;
#' core_code/step5_build_igraphs.R holds one edge per triple, so the uniplex
#' `weight` is the number of parsed relations between two entities in a document).
#' The claim being made is that an observed parsed relation between two entities
#' is more informative than merely seeing both entities somewhere in the same
#' text. Everything below follows from that claim, and two things are deliberately
#' NOT part of it: the direction of the relation, and its volume.
#'
#' UNDIRECTED BY DECISION (2026-10-08, user), not by accident.
#' build_gsa_edges.R:55-58 rbinds both orientations into one (gsa, connected_to)
#' cell, so a GSA->alter and an alter->GSA relation become the same number. The
#' core uniplex graphs are directed and keep from/to, so this is recoverable --
#' it is simply not wanted here: the construct is the existence of a relational
#' event, and which end was the grammatical subject does not bear on it. Treat
#' this as settled; do not "fix" the fold on direction grounds. Measured, for the
#' reviewer who asks what is being set aside (scratchpad/direction.R, 118 docs,
#' loops excluded): 62.6% of GSA-touching triple weight has the GSA as source,
#' and the per-alter out-share tracks actor role rather than sitting at .5 --
#' DWR .74, USGS .64, SWRCB .63, Reclamation .61, CDFW .57 outbound, vs TNC .37
#' and Community Water Center .22 inbound; Consultant 42.5%, NGO 45.1%,
#' Research 55.1%. 83.6% of 26,593 (doc, ego, alter) cells carry one orientation
#' only, so the fold merges nothing for them and only drops a label; the
#' reciprocated 16.4% hold 53.4% of the weight and half are near-balanced.
#' A directed version would not be a transform of this matrix (tcrossprod is
#' symmetric by construction) but two ADDITIONAL edgecovs -- both plans act on e,
#' both are acted on by e -- which is a modeling extension, not a repair.
#'
#' BINARIZED INCIDENCE (step 4, added 2026-10-08). Before it, step 3's weights
#' went into the crossproduct directly, so cell [i,j] was a sum of products of
#' relation counts; the matrices did not match the description above, and since
#' they enter the models as valued edgecov that magnitude reached the coefficients
#' unchanged. Binary is the direct operationalization of the construct -- the
#' relational event either was observed or was not -- and it also removes two
#' quantities we are not trying to measure:
#'   - verbosity. DWR is in 115/118 plans with per-plan weight 1..74, so it
#'     carried 50.3% of the whole institutional covariate: the covariate was
#'     substantially "how much DWR boilerplate does this plan contain"
#'     (DWR's contribution falls from 940,506 to 6,555, i.e. from half the matrix
#'     to one entity's worth of it).
#'   - coalition size. Summing an entity's column over a plan's agency rows gives
#'     total parsed relations between that entity and ANY authoring agency, which
#'     scales with how many agencies co-wrote the plan. gsa_146 (San Joaquin
#'     County GSA) held 6.1% of the institutional covariate off TWO plans through
#'     no artifact at all -- one is a 31-agency joint GSP asserting 36-40 real
#'     relations between it and each co-agency. Substantive, but mostly coalition.
#' Robustness, if a reviewer presses on discarding intensity: every intensity-
#' preserving alternative concentrates MORE on DWR, not less -- DWR share of the
#' institutional covariate raw 55.4%, binary 41.3%, sqrt 64.3%, log1p 65.6%,
#' row-normalized 70.5%. Dyad mass goes as (sum w)^2 - sum w^2, which rewards
#' BREADTH, so compressing magnitudes strips the coalition spikes and leaves DWR's
#' presence in 115/118 plans standing taller. Intensity-with-less-concentration is
#' not reachable by any transform here; it needs a modeling-side change (an
#' author-count control, or separate terms).
#'
#' SCOPE of a 1, in the CURRENT (raw-name) pipeline: "a parsed relation to this
#' entity from at least one GSA-TYPED VERTEX IN THE DOCUMENT" -- not "appears in
#' the document", but also NOT restricted to the plan's authors. build_gsa_edges.R
#' folds on every GSA-typed vertex it finds, so a GSA that is merely mentioned
#' contributes its ego neighborhood too. (Step 3's build_gsa_bag_edges.R DOES
#' restrict to the author set via `intersect(.family_of(X, fam), author_set)`, so
#' the narrower "from >= 1 authoring agency" reading becomes true only after the
#' bag rewire. Corrected 2026-10-08 -- an earlier version of this comment claimed
#' the authoring reading for the current pipeline. It is wrong; doc 3712 has 32
#' GSA rows in the edge file and 0 recorded authors.)
#'
#' AGENCY BREADTH IS SIGNAL, NOT A CONFOUND (decided 2026-10-08, user). Documents
#' touching more GSA vertices register more distinct entities -- cor = 0.794 with
#' the number of agency rows in the edge file. This is retained deliberately: a
#' large coalition really does know more alters, so the breadth difference is the
#' phenomenon, not noise over it. Do NOT normalize it away per plan.
#' Note which quantity that is, because it is NOT the one the models control:
#' correlation with the count of AUTHORING agencies (crosswalk `gsa_ids`, the
#' mult_gsa term) is only 0.243 (spearman 0.30), and the two agency counts
#' correlate 0.461 with each other. So nodecov('mult_gsa') in the modeling
#' scripts is a weak control for this gradient while the covariate is built from
#' raw names; it becomes an apt one after the Step-3 rewire restricts entity
#' observation to authors.
#' The one thing that DID have to go is magnitude double-counting, and binarizing
#' removes it in a provable form rather than approximately: because
#' sum(value) > 0 is identical to any(value > 0), the binary cell is INVARIANT to
#' how many agency rows a plan contributes -- an entity reached by 10 of a plan's
#' agencies scores the same 1 as one reached by a single agency. So nothing is
#' counted once per author; breadth is counted once per entity, which is the
#' intended quantity. (Note this invariance is what makes step 4 safe to apply
#' after the dcast's fun.aggregate = sum -- the sum's magnitude never survives.)
#'
#' The same fold stores a GSA self-loop at 2x (it matches both rbind branches).
#' Immaterial by construction: self-loops land on the gsp x gsp diagonal and only
#' between-document cells are used as predictors (callers zero the diagonal).
#'
#' Pass binarize = FALSE to recover the old weighted construction.
#'
#' NO PREVALENCE FILTER (changed 2026-10-08). The old default dropped entities
#' present in >= 10% of plans. That was calibrated when entities were raw name
#' spellings, where no single spelling of a prolific firm ever reached the cut;
#' once variants fold onto bags (BAG_REVIEW_FINDINGS.md §10) the real actors
#' surface and the filter deleted exactly the consultants the fold was built to
#' find (LSCE and GEI at 14 of 118 plans each, EKI 13). Raising it to .40 was
#' tried and rejected: at .40 the filter is a no-op for every focal group, so its
#' only remaining effect was on the institutional network, where it conflated two
#' different things. Ubiquity is not the right test. DWR appears in 115/118 plans
#' *because it administers SGMA*, which is substantively meaningful and stays in;
#' "California" is not an actor at all and is excluded by name instead. Entities
#' that do not belong in a tie are now removed by identity (step 2), which is the
#' claim we can actually defend.
#'
#' @param gs_melt long data.table with columns gsp_doc_id, variable (entity name), value
#' @param names   entity names to include (from entity_names())
#' @param prevalence_max optional share-of-plans cut, OFF by default (NA). Retained
#'   only so a reviewer can reproduce the old behaviour or run a robustness check;
#'   passing a value re-enables the filter and prints nothing, so use it knowingly.
#' @param exclude entity names never admitted to any matrix (default
#'   NON_ACTOR_ENTITIES); pass character(0) to disable.
#' @param binarize reduce the incidence matrix to relation-observed/not before the
#'   crossproduct (default TRUE), so a cell counts shared entities rather than
#'   summing products of relation counts. FALSE restores the old weighted form.
#' @return numeric doc x doc matrix (rownames/colnames = gsp_doc_id); off-diagonal
#'   cell [i,j] = number of entities in `names` that plans i and j both record a
#'   parsed relation to
#'   (diagonal = the count plan i mentions, which callers zero out). If no entity
#'   survives filtering, a 0 x 0 matrix (callers should guard).
build_shared_entity_matrix <- function(gs_melt, names, prevalence_max = NA_real_,
                                       exclude = NON_ACTOR_ENTITIES,
                                       binarize = TRUE) {
  names <- setdiff(names, exclude)
  sub <- gs_melt[gs_melt$variable %in% names, ]
  if (!nrow(sub)) return(matrix(numeric(0), 0, 0))
  df  <- dcast(sub, gsp_doc_id ~ variable, value.var = "value",
               fun.aggregate = sum, na.rm = TRUE, fill = 0)
  mat <- as.matrix(df[, -1, with = FALSE])
  rownames(mat) <- df$gsp_doc_id
  if (!is.na(prevalence_max)) {
    # Opt-in only. Prevalence is a share of ALL plans, so the denominator comes
    # from the full gs_melt before subsetting; using nrow(mat) would divide by
    # only the plans mentioning this group, making the cut far stricter for the
    # sparse focal subnetworks and dropping their most-shared entities.
    n_plans <- length(unique(gs_melt$gsp_doc_id))
    mat <- mat[, (colSums(mat > 0) / n_plans) < prevalence_max, drop = FALSE]
  }
  # After the prevalence cut, which reads nonzero counts and so is unaffected by
  # the order, but keeping it second leaves the filter's semantics untouched.
  if (binarize) mat <- (mat > 0) * 1L
  tcrossprod(mat)
}

#' Re-index a doc x doc matrix onto a fixed set of ids (gsp_doc_id), zero-filling
#' any id absent from `mat`. Needed because the sparse focal subnetworks
#' (Consultant/Research) may not touch every plan, so `mat[ids, ids]` would
#' fail; this returns a length(ids) x length(ids) matrix aligned to `ids`.
align_gsp_matrix <- function(mat, ids) {
  out <- matrix(0, length(ids), length(ids), dimnames = list(ids, ids))
  if (length(mat) && nrow(mat)) {
    common <- intersect(ids, rownames(mat))
    if (length(common)) out[common, common] <- mat[common, common]
  }
  out
}
