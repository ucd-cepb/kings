#' build_node_dictionary.R — classify core entity names into semantic types.
#'
#' This is the paper's entity-classification step, and it happens ENTIRELY in this
#' project: core carries spaCy NER tags only, and the paper's six-leaf semantic
#' taxonomy has no reproducible generator upstream. So the types are regenerated
#' here with the LLM classifier (see classify_entities.R), few-shot-prompted from
#' curated in-code exemplars and pinned by the core-dicts gazetteer.
#'
#' Reads the disambiguated core objects (textnet_objects/disambig) for the unique
#' entity names, and writes into Network_Innovation_Paper/data_products/01_entity_classification/:
#'   - node_dictionary.csv  entity name -> semantic type (LLM-classified)
#'
#' This is the one step that calls an external API; it is cached
#' (entity_type_cache.csv), so re-runs are cheap and only newly-seen entity names
#' hit the API. Run build_overrides_from_dicts.R FIRST so the deterministic
#' gazetteer the classifier pins to is current.
#'
#' Run from the repo root:
#'   Rscript Network_Innovation_Paper/Code/01_entity_classification/build_node_dictionary.R
#' Set CLOBBER=TRUE to overwrite an existing node_dictionary.csv.

suppressMessages({
  library(data.table); library(stringr)
})

`%||%` <- function(a, b) if (is.null(a)) b else a
# Resolve this script's directory robustly whether sourced or Rscript-run.
.this_dir <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) NA)
if (is.na(.this_dir)) {
  a <- commandArgs(FALSE); m <- grep("^--file=", a, value = TRUE)
  .this_dir <- if (length(m)) dirname(normalizePath(sub("^--file=", "", m[1]))) else
    "Network_Innovation_Paper/Code/01_entity_classification"
}
source(file.path(.this_dir, "..", "_paths.R"))
source(file.path(.this_dir, "classify_entities.R"))

CLOBBER <- toupper(Sys.getenv("CLOBBER", "FALSE")) %in% c("TRUE", "1", "YES")

.skip <- function(path) {
  if (file.exists(path) && !CLOBBER) {
    message("exists, skipping (set CLOBBER=TRUE to rebuild): ", path); TRUE
  } else FALSE
}

# node_dictionary: unique core entity names -> semantic type (LLM)
build_node_dictionary <- function() {
  out <- nip_product("01_entity_classification", "node_dictionary.csv")
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

if (sys.nframe() == 0) {
  message("== build_node_dictionary.R (CLOBBER=", CLOBBER, ") ==")
  build_node_dictionary()
  message("== node_dictionary complete ==")
}
