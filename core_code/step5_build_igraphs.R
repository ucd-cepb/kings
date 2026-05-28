# step5_build_igraphs.R
# Builds igraph network objects from the disambiguated textnet_extract RDS
# files produced by step4. For each GSP, writes two igraph variants:
#
#   1. multiplex_directed_graphs/<stem>.RDS
#        One edge per SVO triple from the extract — multi-edges between the
#        same source/target pair are preserved. All verb-level edge
#        attributes (head_verb_*, parent_verb_*, helper_*, xcomp_*, neg,
#        is_future, has_hedge, edgeiscomplete, doc_sent_*) ride along on
#        the edges.
#
#   2. uniplex_weighted_graphs/<stem>.RDS
#        The multiplex graph collapsed via igraph::simplify(): verb-specific
#        edge attributes are dropped, each edge is given weight = 1, and
#        igraph::simplify(edge.attr.comb = list(weight = "sum")) folds
#        parallel edges into one with weight = count of merged edges.
#        Self-loops are kept (remove.loops = FALSE).
#
# Inputs:
#   disambiged_extracts_core/<stem>.RDS  (from step4)
#       textnet_extract list with $edgelist + $nodelist as data.tables.
#
# Outputs:
#   igraph_objects/multiplex_directed_graphs/<stem>.RDS
#   igraph_objects/uniplex_weighted_graphs/<stem>.RDS
#
# Pre-graph filtering applied to every extract (kept here, not in step4,
# because these filters are graph-construction prep — they discard rows
# that would otherwise produce malformed vertices/edges):
#   - drop edges where BOTH $source and $target are NA (truly broken).
#     Half-NA edges (one resolved, one missing) are KEPT — they're
#     informative as "this entity was the source of something" even if
#     the counterparty wasn't extracted.
#   - drop edges/nodes whose entity name has fewer than 2 a-z letters
#     (filters out punctuation-only or single-letter tokens). The letter
#     check is skipped on a NA side — it only fires on real strings.
#
# Stem convention: file stems carry the plan-version prefix (e.g. v1_0007).
# Each (version, GSP) pair gets its own pair of igraph outputs.

library(igraph)
library(stringr)
library(data.table)

# === Flags ===
CLOBBER <- FALSE  # set TRUE to rebuild graphs even if outputs already exist

filekey <- read.csv("filekey.csv")

disambig_dir   <- filekey[filekey$var_name == "disambiged_extracts_core",        ]$filepath
multiplex_dir  <- filekey[filekey$var_name == "multiplex_directed_graphs_core",  ]$filepath
uniplex_dir    <- filekey[filekey$var_name == "uniplex_weighted_graphs_core",    ]$filepath

dir.create(multiplex_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(uniplex_dir,   recursive = TRUE, showWarnings = FALSE)

# Edge attributes that describe a single SVO triple and don't aggregate
# meaningfully across collapsed parallel edges. Dropped before simplify().
verb_edge_attrs <- c(
  "head_verb_id", "head_verb_tense", "head_verb_name", "head_verb_lemma",
  "parent_verb_id", "neg",
  "doc_sent_verb", "doc_sent_parent",
  "helper_lemma", "helper_token",
  "xcomp_verb", "xcomp_helper_lemma", "xcomp_helper_token",
  "edgeiscomplete", "has_hedge", "is_future"
)

# Filter rows that can't legally become vertices/edges, then build both graphs.
build_graphs <- function(edgenodelist) {
  edgelist <- as.data.table(edgenodelist$edgelist)
  nodelist <- as.data.table(edgenodelist$nodelist)

  n_edges_in <- nrow(edgelist)
  n_nodes_in <- nrow(nodelist)

  # Drop only fully-NA edges; keep half-NA (informative anchor + unresolved other side)
  edgelist <- edgelist[!(is.na(source) & is.na(target))]
  n_after_doubleNA <- nrow(edgelist)

  # Drop edges/nodes whose entity name has <2 a-z letters. NA sides are
  # ignored — they pass the letter check (the previous filter already
  # decided whether they're worth keeping).
  edgelist[, esletters := str_remove_all(source,       "[^a-z_]")]
  edgelist[, etletters := str_remove_all(target,       "[^a-z_]")]
  nodelist[, nletters  := str_remove_all(entity_name,  "[^a-z_]")]
  nodelist <- nodelist[nchar(nletters) > 1L]
  edgelist <- edgelist[
    (is.na(source) | nchar(esletters) > 1L) &
    (is.na(target) | nchar(etletters) > 1L)
  ]
  edgelist[, c("esletters", "etletters") := NULL]
  nodelist[, nletters := NULL]

  message(sprintf(
    "    edges: %d in, %d after NA filter, %d after letter filter (dropped %d, %.0f%%)",
    n_edges_in, n_after_doubleNA, nrow(edgelist),
    n_edges_in - nrow(edgelist),
    100 * (n_edges_in - nrow(edgelist)) / max(n_edges_in, 1)))
  message(sprintf(
    "    nodes: %d in, %d after letter filter (dropped %d, %.0f%%)",
    n_nodes_in, nrow(nodelist),
    n_nodes_in - nrow(nodelist),
    100 * (n_nodes_in - nrow(nodelist)) / max(n_nodes_in, 1)))

  # graph_from_data_frame() expects source/target as the first two columns
  other_cols <- setdiff(names(edgelist), c("source", "target"))
  edgelist   <- edgelist[, c("source", "target", other_cols), with = FALSE]

  # === Multiplex directed (one edge per row) ===
  multiplex <- igraph::graph_from_data_frame(edgelist,
                                             vertices = nodelist,
                                             directed = TRUE)

  # === Uniplex weighted (collapse parallel edges, weight = count) ===
  uniplex <- multiplex
  for (a in verb_edge_attrs) {
    if (a %in% igraph::edge_attr_names(uniplex)) {
      uniplex <- igraph::delete_edge_attr(uniplex, a)
    }
  }
  igraph::E(uniplex)$weight <- 1
  uniplex <- igraph::simplify(uniplex,
                              edge.attr.comb = list(weight = "sum"),
                              remove.loops   = FALSE)

  list(multiplex = multiplex, uniplex = uniplex)
}

disambig_files <- list.files(disambig_dir, pattern = "\\.RDS$", full.names = TRUE)
cat(sprintf("Found %d disambiguated extract(s) in %s\n",
            length(disambig_files), disambig_dir))

built <- skipped <- failed <- 0L

for (f in disambig_files) {
  stem            <- str_remove(basename(f), "\\.RDS$")
  multiplex_path  <- file.path(multiplex_dir, paste0(stem, ".RDS"))
  uniplex_path    <- file.path(uniplex_dir,   paste0(stem, ".RDS"))

  if (!CLOBBER && file.exists(multiplex_path) && file.exists(uniplex_path)) {
    skipped <- skipped + 1L
    next
  }

  message("Building graphs: ", stem)
  res <- tryCatch({
    edgenodelist <- readRDS(f)
    build_graphs(edgenodelist)
  }, error = function(e) {
    message("  ERROR (", stem, "): ", conditionMessage(e))
    NULL
  })

  if (is.null(res)) { failed <- failed + 1L; next }

  saveRDS(res$multiplex, multiplex_path)
  saveRDS(res$uniplex,   uniplex_path)
  message(sprintf("  -> %d vertices, %d multiplex edges, %d uniplex edges",
                  igraph::vcount(res$multiplex),
                  igraph::ecount(res$multiplex),
                  igraph::ecount(res$uniplex)))
  built <- built + 1L
}

cat(sprintf("\nDone. Built: %d  Skipped: %d  Failed: %d\n",
            built, skipped, failed))
