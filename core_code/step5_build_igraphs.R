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
# that would otherwise produce malformed vertices/edges. The textnet_extract
# RDS from step4 is the source of truth and is not modified):
#   - drop edges where source OR target is NA. igraph::graph_from_data_frame()
#     rejects any NA in the edge endpoints, and a half-NA edge isn't really
#     traversable in graph terms (you can't follow it to a counterparty).
#     The textnet_extract object on disk still carries half-NA edges for any
#     downstream analysis that wants them; this is purely a graph-shape
#     filter. Isolate vertices (nodes that lose all their edges to this
#     filter) are kept in the graph as isolates — see the no-edge-participation
#     filter on nodelist below.
#   - drop edges/nodes whose entity name has fewer than 2 a-z letters
#     (filters out punctuation-only or single-letter tokens).
#   - drop nodelist rows where entity_name is NA (graph_from_data_frame
#     requires non-NA vertex ids).
#   - keep ALL remaining nodelist rows as vertices, even those that don't
#     appear in any edge — they show up as isolates in the graph. This is
#     deliberate: a vertex set is the entity list, not the edge endpoints.
#   - dedup nodelist by canonical entity_name (step4 may emit multiple rows
#     with the same canonical when several originals collapsed via
#     disambiguate). Required for graph_from_data_frame's unique-vertex
#     contract. The surviving row carries one raw_entity_name as a vertex
#     attribute (sample of the pre-disambig surface form); the dict files
#     are the authoritative source for the full canonical→aliases mapping.
#
# Stem convention: stems come from step1 and are gspDocId strings under
# the new step0 convention (each document has its own unique stem). Each
# stem maps 1:1 to one document and produces one pair of multiplex /
# uniplex igraph outputs. Plan-family lookup (which submission, which
# plan) is mediated by source_pdfs/plan_family_manifest.csv.

library(igraph)
library(stringr)
library(data.table)

source("core_code/_config.R")   # provides CLOBBER, filekey, fk()

disambig_dir   <- fk("disambiged_extracts_core")
multiplex_dir  <- fk("multiplex_directed_graphs_core")
uniplex_dir    <- fk("uniplex_weighted_graphs_core")

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

  # Drop any edge with a NA endpoint — graph_from_data_frame rejects NAs in
  # source/target, and a half-NA edge can't be represented in the graph
  # (no second endpoint to connect to). The textnet_extract RDS still holds
  # these rows for non-graph analyses; this filter only narrows the rows we
  # feed to igraph.
  edgelist <- edgelist[!is.na(source) & !is.na(target)]
  n_after_NA <- nrow(edgelist)

  # Drop edges/nodes whose entity name has <2 a-z letters (punctuation-only
  # or single-letter tokens).
  edgelist[, esletters := str_remove_all(source,       "[^a-z_]")]
  edgelist[, etletters := str_remove_all(target,       "[^a-z_]")]
  nodelist[, nletters  := str_remove_all(entity_name,  "[^a-z_]")]
  edgelist <- edgelist[nchar(esletters) > 1L & nchar(etletters) > 1L]
  nodelist <- nodelist[!is.na(entity_name) & nchar(nletters) > 1L]
  edgelist[, c("esletters", "etletters") := NULL]
  nodelist[, nletters := NULL]

  # Dedup nodelist by canonical entity_name so graph_from_data_frame has
  # unique vertex IDs. Step4 may emit multiple rows with the same canonical
  # name (different originals collapsed by disambiguate). unique(by=) keeps
  # the first occurrence; raw_entity_name on that row reflects one example
  # of the observed pre-disambig surface form for the vertex. To recover
  # the full alias set for a canonical, consult the dict files. All
  # remaining vertices are passed to graph_from_data_frame, including those
  # that no longer appear in any edge — they become isolates.
  n_nodes_pre_dedup <- nrow(nodelist)
  nodelist <- unique(nodelist, by = "entity_name")

  message(sprintf(
    "    edges: %d in, %d after NA filter, %d after letter filter (dropped %d, %.0f%%)",
    n_edges_in, n_after_NA, nrow(edgelist),
    n_edges_in - nrow(edgelist),
    100 * (n_edges_in - nrow(edgelist)) / max(n_edges_in, 1)))
  message(sprintf(
    "    nodes: %d in, %d after letter filter, %d after canonical dedup (dropped %d, %.0f%%)",
    n_nodes_in, n_nodes_pre_dedup, nrow(nodelist),
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

  atomic_saveRDS(res$multiplex, multiplex_path)
  atomic_saveRDS(res$uniplex,   uniplex_path)
  message(sprintf("  -> %d vertices, %d multiplex edges, %d uniplex edges",
                  igraph::vcount(res$multiplex),
                  igraph::ecount(res$multiplex),
                  igraph::ecount(res$uniplex)))
  built <- built + 1L
}

cat(sprintf("\nDone. Built: %d  Skipped: %d  Failed: %d\n",
            built, skipped, failed))
