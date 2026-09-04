#' _entity_groups.R — single source of truth for entity-type groupings used by
#' the modeling scripts (make_binary0.9; explore/make_networks, explore/make_valued_networks).
#'
#' Every entity name in node_dictionary.csv carries ONE semantic type from the
#' 6-way controlled vocabulary in classify_entities.R (GSA, Consultant, Research,
#' NGO, Institutional_other, Non_institutional). Those six sit on two axes: a
#' NOISE GATE (institutional vs Non_institutional) and, within institutional, the
#' focal type or the generic residual. `Institutional` is the SUPERSET of the five
#' non-Non_institutional leaves -- it is the grouping defined here, not a label.
#' The vocabulary is exhaustive, so the "all institutional" set (spaCy ORG u GPE u
#' NORP, junk-pruned, water utilities/agencies rescued, water features/
#' infrastructure removed) is exactly INSTITUTIONAL_TYPES below -- the raw spaCy
#' gate is redundant to it.
#'
#' Groupings (see build_shared_entity_matrix() for how they become gsp x gsp
#' shared-entity matrices):
#'   institutional          generic network: all institutional actors
#'   Consultant/Research/NGO the three focal subnetworks (each on its own)
#'   consultant_research_ngo the 4th "grouped" run: the three focal types pooled

# --- semantic-type groupings --------------------------------------------------

# All institutional actors = every leaf except Non_institutional (which is the
# gate's reject bucket: persons, basins, natural features, geographic units,
# infrastructure, projects, data systems, legal/reference/technical, OCR junk).
# GSA/Consultant/Research/NGO are the focal institutional leaves; Institutional_other
# is the generic residual (cities, counties, districts, state/federal/local
# government bodies, companies, stakeholder committees).
INSTITUTIONAL_TYPES <- c(
  "GSA", "Consultant", "Research", "NGO", "Institutional_other"
)

# The three focal subnetworks and their pooled ("grouped") 4th run.
FOCAL_TYPES <- c("Consultant", "Research", "NGO")

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
#'   2. cast to a gsp_id x entity incidence matrix (summed mention weight)
#'   3. drop ubiquitous entities (present in >= `prevalence_max` of plans), which
#'      would otherwise dominate the cross-product with uninformative ties
#'   4. tcrossprod -> gsp x gsp count of shared (co-mentioned) entities
#'
#' @param gs_melt long data.table with columns gsp_doc_id, variable (entity name), value
#' @param names   entity names to include (from entity_names())
#' @param prevalence_max drop entities present in >= this share of plans (default .10)
#' @return numeric doc x doc matrix (rownames/colnames = gsp_doc_id); if no entity
#'   survives filtering, a 0 x 0 matrix (callers should guard).
build_shared_entity_matrix <- function(gs_melt, names, prevalence_max = 0.10) {
  # Prevalence is a share of ALL plans, so take the denominator from the full
  # gs_melt before subsetting. Using nrow(mat) instead would divide by only the
  # plans that mention this group's entities, making the threshold far stricter
  # for sparse focal subnetworks (Consultant/Research/NGO) and dropping exactly
  # the most-shared focal entities.
  n_plans <- length(unique(gs_melt$gsp_doc_id))
  sub <- gs_melt[gs_melt$variable %in% names, ]
  if (!nrow(sub)) return(matrix(numeric(0), 0, 0))
  df  <- dcast(sub, gsp_doc_id ~ variable, value.var = "value",
               fun.aggregate = sum, na.rm = TRUE, fill = 0)
  mat <- as.matrix(df[, -1, with = FALSE])
  rownames(mat) <- df$gsp_doc_id
  keep <- (colSums(mat > 0) / n_plans) < prevalence_max
  mat  <- mat[, keep, drop = FALSE]
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
