#' _entity_groups.R — single source of truth for entity-type groupings used by
#' the modeling scripts (make_networks / make_valued_networks / make_binary0.9).
#'
#' Every entity name in node_dictionary.csv carries ONE semantic type from the
#' 23-way controlled vocabulary in classify_entities.R. Two label systems exist
#' per name: the coarse spaCy NER tag (ORG/GPE/NORP/LOC/FAC/...) carried in core,
#' and this cleaned semantic type. The semantic taxonomy is exhaustive and
#' partitions into institutional vs non-institutional, so the "all institutional"
#' (ORG + GPE + NORP, junk-pruned, water utilities/agencies rescued, water
#' features/infrastructure removed) set is exactly INSTITUTIONAL_TYPES below --
#' the raw spaCy gate is redundant to it.
#'
#' Groupings (see build_shared_entity_matrix() for how they become gsp x gsp
#' shared-entity matrices):
#'   institutional          generic network: all institutional actors
#'   Consultant/Research/NGO the three focal subnetworks (each on its own)
#'   consultant_research_ngo the 4th "grouped" run: the three focal types pooled

# --- semantic-type groupings --------------------------------------------------

# All institutional actors = spaCy ORG u GPE u NORP after the classifier prunes
# junk and rescues mis-tagged institutions. Excludes Person (spaCy PERSON) and
# the non-institutional types (Basin, Natural_Feature, Geographic_Unit,
# Infrastructure, Water_Project, Data_System, Legal, Reference, Technical,
# Nonsense).
INSTITUTIONAL_TYPES <- c(
  # ORG
  "GSA", "Local_Gov", "State_Gov", "Federal_Gov",
  "District", "Company", "Consultant", "NGO", "Research",
  # GPE
  "City", "County",
  # NORP
  "Group"
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
#' @param gs_melt long data.table with columns gsp_id, variable (entity name), value
#' @param names   entity names to include (from entity_names())
#' @param prevalence_max drop entities present in >= this share of plans (default .10)
#' @return numeric gsp x gsp matrix (rownames/colnames = gsp_id); if no entity
#'   survives filtering, a 0 x 0 matrix (callers should guard).
build_shared_entity_matrix <- function(gs_melt, names, prevalence_max = 0.10) {
  sub <- gs_melt[gs_melt$variable %in% names, ]
  if (!nrow(sub)) return(matrix(numeric(0), 0, 0))
  df  <- dcast(sub, gsp_id ~ variable, value.var = "value",
               fun.aggregate = sum, na.rm = TRUE, fill = 0)
  mat <- as.matrix(df[, -1, with = FALSE])
  rownames(mat) <- df$gsp_id
  keep <- (colSums(mat > 0) / nrow(mat)) < prevalence_max
  mat  <- mat[, keep, drop = FALSE]
  tcrossprod(mat)
}

#' Re-index a gsp x gsp matrix onto a fixed set of gsp_ids, zero-filling any
#' gsp absent from `mat`. Needed because the sparse focal subnetworks
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
