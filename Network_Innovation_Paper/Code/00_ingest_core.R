#' 00_ingest_core.R — the ONLY bridge from core_data to Network_Innovation_Paper
#'
#' Starts from the disambiguated core objects (textnet_objects/disambig +
#' igraph_objects/uniplex_weighted_graphs). It reads NO upstream raw docs
#' (source_pdfs, plan_txts_*). Its only other core reads are two small tables:
#' the plan-family manifest (ID crosswalk) and sgma_gsa_full (GSA reference).
#'
#' Produces, into Network_Innovation_Paper/data_products/:
#'   - id_crosswalk.csv     gspDocId <-> legacy gsp_id/version (from manifest; no derivation)
#'   - node_dictionary.csv  entity name -> semantic type (LLM-classified from core naming)
#'   - all_gsa_edges.csv     per-plan agency x entity mention-weight matrix
#'
#' The semantic entity types are NOT in core (core carries spaCy NER tags only)
#' and had no reproducible generator, so they are regenerated here with an LLM
#' classifier (see classify_entities.R), few-shot-prompted from curated in-code
#' exemplars and pinned by the core-dicts gazetteer. This is the one step that
#' calls an external API; it is cached, so re-runs are cheap and only newly-seen
#' entity names hit the API.
#'
#' Run from the repo root:  Rscript Network_Innovation_Paper/Code/00_ingest_core.R
#' Set CLOBBER=TRUE to overwrite existing products.

suppressMessages({
  library(data.table); library(stringr); library(igraph)
})

`%||%` <- function(a, b) if (is.null(a)) b else a
# Resolve this script's directory robustly whether sourced or Rscript-run.
.this_dir <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) NA)
if (is.na(.this_dir)) {
  a <- commandArgs(FALSE); m <- grep("^--file=", a, value = TRUE)
  .this_dir <- if (length(m)) dirname(normalizePath(sub("^--file=", "", m[1]))) else
    "Network_Innovation_Paper/Code"
}
source(file.path(.this_dir, "_paths.R"))
source(file.path(.this_dir, "_corpus.R"))        # select_plan_docs()
source(file.path(.this_dir, "01_entity_classification", "classify_entities.R"))

# ---- config ------------------------------------------------------------------
INGEST <- list(
  clobber  = toupper(Sys.getenv("CLOBBER", "FALSE")) %in% c("TRUE", "1", "YES"),
  gsa_type = "GSA"                                  # semantic type flagged as a GSA
                                                    # (v2 merged Local_GSA/Other_GSA -> GSA)
)

.skip <- function(path) {
  if (file.exists(path) && !INGEST$clobber) {
    message("exists, skipping (set CLOBBER=TRUE to rebuild): ", path); TRUE
  } else FALSE
}

# =============================================================================
# 1. ID crosswalk (pure core; columns are pre-joined in the manifest)
# =============================================================================
build_crosswalk <- function() {
  out <- nip_product("id_crosswalk.csv")
  man <- fread(core_manifest(), colClasses = list(character = c("gspDocId", "gspId")))
  xw <- man[, .(
    gsp_doc_id     = gspDocId,
    gsp_id         = gspId,
    canonical_gsp_id = canonical_gspId,
    version        = as.character(version),
    plan_section, submitted_date,
    from_gsp_id    = fromGspId,
    gsa_ids, gsa_names, basin,
    out_basename
  )]
  # doc_rank orders a plan's documents by submission date (1 = earliest). A plan
  # resubmitted after its original gets rank 2; single-document plans are always
  # 1. This — NOT the manifest `version` field, which is an unrelated
  # adoption-cycle axis — is how the paper selects one document per plan (see
  # select_plan_docs() / NIP_DOC_SELECT in _corpus.R).
  xw[, .subdate := as.IDate(submitted_date, format = "%m/%d/%Y")]
  setorder(xw, gsp_id, .subdate, gsp_doc_id)  # gsp_doc_id breaks any date tie deterministically
  xw[, doc_rank := seq_len(.N), by = gsp_id]
  xw[, .subdate := NULL]
  fwrite(xw, out)
  message("wrote ", out, " (", nrow(xw), " rows)")
  xw
}

# =============================================================================
# 2. node_dictionary: unique core entity names -> semantic type (LLM)
# =============================================================================
build_node_dictionary <- function() {
  out <- nip_product("node_dictionary.csv")
  # Few-shot examples are curated in-code (classify_entities.R: .FEWSHOT_EXAMPLES)
  # and the known cast is pinned by the core-dicts gazetteer, so nothing needs to be
  # frozen from the product -- the prompt is already stable across rebuilds.
  if (.skip(out)) return(fread(out))

  fs <- list.files(core_disambig(), pattern = "\\.RDS$", full.names = TRUE)
  message("collecting unique entities from ", length(fs), " disambig objects...")
  agg <- rbindlist(lapply(fs, function(f) {
    nl <- readRDS(f)$nodelist
    data.table(entity_name = as.character(nl$entity_name),
               spacy = as.character(nl$entity_type),
               n = suppressWarnings(as.numeric(nl$num_appearances)))
  }), fill = TRUE)
  agg <- agg[!is.na(entity_name) & nzchar(entity_name)]
  # one row per name: keep modal spacy tag + total appearances (a hint for the LLM)
  agg <- agg[, .(spacy = spacy[which.max(n %||% 0)][1], n = sum(n, na.rm = TRUE)),
             by = entity_name]
  message("unique entity names: ", nrow(agg))

  hints <- paste0("spaCy=", agg$spacy, ", n=", round(agg$n))
  res <- classify_entities(agg$entity_name, hints = hints)
  setnames(res, "name", "entity_name")
  # write with the historical schema: name, entity_type
  nd <- res[, .(name = entity_name, entity_type)]
  fwrite(nd, out)
  message("wrote ", out, " (", nrow(nd), " rows)")
  nd
}

# =============================================================================
# 3. all_gsa_edges: per-plan agency x connected-entity mention-weight matrix
#    (ports Network_Structure_Paper/2_edges_per_gsa.R onto current core objects)
# =============================================================================
.gsa_edges_one <- function(stem, gsp_id_int, nodedict) {
  f <- core_rds_for_stem(core_igraph_weighted(), stem)
  if (!file.exists(f)) return(NULL)
  g <- readRDS(f)
  vdf <- as.data.table(igraph::as_data_frame(g, what = "vertices"))
  edf <- as.data.table(igraph::as_data_frame(g, what = "edges"))
  if (!nrow(edf) || !"weight" %in% names(edf)) return(NULL)
  # semantic type by name (spaCy entity_type on the vertex is ignored here)
  vdf <- merge(vdf[, .(name)], nodedict, by = "name", all.x = TRUE)
  gsa_names <- vdf[entity_type == INGEST$gsa_type, name]
  if (!length(gsa_names)) return(NULL)
  ge <- edf[from %in% gsa_names | to %in% gsa_names]
  if (!nrow(ge)) return(NULL)
  folded <- rbind(
    ge[from %in% gsa_names, .(gsa = from, connected_to = to,   weight)],
    ge[to   %in% gsa_names, .(gsa = to,   connected_to = from, weight)]
  )
  agg <- folded[, .(weight = sum(weight, na.rm = TRUE)), by = .(gsa, connected_to)]
  w <- dcast(agg, gsa ~ connected_to, value.var = "weight", fill = 0)
  w[, gsp_id := gsp_id_int]
  w
}

build_gsa_edges <- function(xw, nodedict) {
  out <- nip_product("all_gsa_edges.csv")
  if (.skip(out)) return(invisible(NULL))
  # One document per plan (same selection the text-reuse maps use). Default keeps
  # the original submission; NIP_DOC_SELECT=latest switches to the resubmitted
  # doc where one exists. Keying edges to one doc per plan avoids representing a
  # resubmitted plan twice.
  keep <- select_plan_docs(xw)
  message("building all_gsa_edges for ", nrow(keep), " plans (",
          Sys.getenv("NIP_DOC_SELECT", "original"), " doc per plan)...")
  parts <- lapply(seq_len(nrow(keep)), function(i) {
    .gsa_edges_one(keep$gsp_doc_id[i], as.integer(keep$gsp_id[i]), nodedict)
  })
  parts <- Filter(Negate(is.null), parts)
  master <- rbindlist(parts, use.names = TRUE, fill = TRUE)  # union of entity cols; missing -> NA
  setcolorder(master, c("gsa", "gsp_id", setdiff(names(master), c("gsa", "gsp_id"))))
  fwrite(master, out)
  message("wrote ", out, " (", nrow(master), " rows x ", ncol(master), " cols)")
  invisible(master)
}

# =============================================================================
# main
# =============================================================================
if (sys.nframe() == 0) {
  message("== 00_ingest_core.R (CLOBBER=", INGEST$clobber, ") ==")
  xw <- build_crosswalk()
  nd <- build_node_dictionary()
  build_gsa_edges(xw, nd)
  message("== ingest complete ==")
}
