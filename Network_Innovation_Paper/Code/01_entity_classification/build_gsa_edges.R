#' build_gsa_edges.R — per-plan agency x connected-entity mention-weight matrix.
#'
#' (Ports Network_Structure_Paper/2_edges_per_gsa.R onto current core objects.)
#'
#' Folds the core weighted graphs (igraph_objects/uniplex_weighted_graphs) down to
#' the entities the classifier labelled as GSAs. It is therefore a downstream
#' product of entity classification, NOT of ingest: which vertices count as a GSA
#' is decided purely by the semantic type in node_dictionary.csv (the spaCy tag on
#' the vertex is ignored). Run build_node_dictionary.R first.
#'
#' Reads: id_crosswalk.csv + node_dictionary.csv (both paper products) and the core
#' weighted graphs. Writes into Network_Innovation_Paper/data_products/:
#'   - all_gsa_edges.csv   per-plan agency x connected-entity mention-weight matrix
#'
#' Run from the repo root:
#'   Rscript Network_Innovation_Paper/Code/01_entity_classification/build_gsa_edges.R
#' Set CLOBBER=TRUE to overwrite an existing all_gsa_edges.csv.

suppressMessages({
  library(data.table); library(igraph)
})

# Resolve this script's directory robustly whether sourced or Rscript-run.
.this_dir <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) NA)
if (is.na(.this_dir)) {
  a <- commandArgs(FALSE); m <- grep("^--file=", a, value = TRUE)
  .this_dir <- if (length(m)) dirname(normalizePath(sub("^--file=", "", m[1]))) else
    "Network_Innovation_Paper/Code/01_entity_classification"
}
source(file.path(.this_dir, "..", "_paths.R"))
source(file.path(.this_dir, "..", "_corpus.R"))   # select_plan_docs(), load_id_crosswalk()

GSA_TYPE <- "GSA"    # semantic type flagged as a GSA (v2 merged Local_GSA/Other_GSA -> GSA)
CLOBBER  <- toupper(Sys.getenv("CLOBBER", "FALSE")) %in% c("TRUE", "1", "YES")

.skip <- function(path) {
  if (file.exists(path) && !CLOBBER) {
    message("exists, skipping (set CLOBBER=TRUE to rebuild): ", path); TRUE
  } else FALSE
}

.gsa_edges_one <- function(stem, gsp_id_int, nodedict) {
  f <- core_rds_for_stem(core_igraph_weighted(), stem)
  if (!file.exists(f)) return(NULL)
  g <- readRDS(f)
  vdf <- as.data.table(igraph::as_data_frame(g, what = "vertices"))
  edf <- as.data.table(igraph::as_data_frame(g, what = "edges"))
  if (!nrow(edf) || !"weight" %in% names(edf)) return(NULL)
  # semantic type by name (spaCy entity_type on the vertex is ignored here)
  vdf <- merge(vdf[, .(name)], nodedict, by = "name", all.x = TRUE)
  gsa_names <- vdf[entity_type == GSA_TYPE, name]
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

build_gsa_edges <- function(xw = load_id_crosswalk(),
                            nodedict = fread(nip_product("node_dictionary.csv"),
                                             colClasses = "character")) {
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

if (sys.nframe() == 0) {
  if (!file.exists(nip_product("node_dictionary.csv"))) {
    stop("node_dictionary.csv is missing; run build_node_dictionary.R first.",
         call. = FALSE)
  }
  message("== build_gsa_edges.R (CLOBBER=", CLOBBER, ") ==")
  build_gsa_edges()
  message("== all_gsa_edges complete ==")
}
