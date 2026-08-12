#' Measure the entity-type classifier against the human labels.
#'
#' Produces model-vs-human AGREEMENT (not ground truth — the human labels are
#' themselves noisy), a per-category recall table, and a confusion list. The
#' point is to find which category *pairs* the model and humans can't separate:
#' those are the taxonomy's structural weak points.
#'
#' Design notes:
#'   - Routes to a SEPARATE eval cache (data_products/eval/) so it never touches
#'     the production entity_type_cache.csv. Delete that file to force a re-run.
#'   - Excludes the few-shot example names from the eval sample, so we never
#'     score the model on strings it was literally shown in the prompt.
#'   - Runs WITHOUT hints (worst case). The production pipeline passes spaCy
#'     type + frequency, which should only help.
#'
#' Usage (from repo root):
#'   NIP_EVAL_PER_CAT=30 Rscript Network_Innovation_Paper/Code/eval_classifier.R
#'   # optional: NIP_EVAL_TAG=baseline  -> names outputs eval_*_baseline.csv

source("Network_Innovation_Paper/Code/_paths.R")
source("Network_Innovation_Paper/Code/classify_entities.R")
suppressMessages(library(data.table))

PER_CAT <- as.integer(Sys.getenv("NIP_EVAL_PER_CAT", "30"))
TAG     <- Sys.getenv("NIP_EVAL_TAG", "current")

# Frozen eval sample: built once and reused, so different taxonomy versions are
# scored on IDENTICAL names (changing the vocabulary changes stratified sampling,
# which otherwise confounds any version-to-version comparison). Delete the file
# to rebuild. NOTE: gold is the human seed label, which evaluation has shown to
# be noisier than the model on the ambiguous categories -- treat agreement as a
# floor and inspect disagreements by hand, don't read it as accuracy.
eval_dir <- nip_product("eval")
dir.create(eval_dir, showWarnings = FALSE, recursive = TRUE)
sample_file <- file.path(eval_dir, "eval_sample.csv")

if (file.exists(sample_file)) {
  samp <- fread(sample_file, colClasses = "character")
  samp[, entity_type := .remap_labels(entity_type)]
  samp <- samp[entity_type %in% ENTITY_TYPES]
  cat(sprintf("Eval sample (frozen): %d names across %d categories\n",
              nrow(samp), samp[, uniqueN(entity_type)]))
} else {
  # Gold = the FROZEN human seed (see comment above), and it must be the same
  # source .build_examples() samples from so the few-shot names held out below
  # actually match what the prompt used. Fall back to the product only if no
  # seed exists yet (first run).
  seed_f <- nip_input("node_dictionary_seed.csv")
  d <- fread(if (file.exists(seed_f)) seed_f else nip_product("node_dictionary.csv"))
  d[, entity_type := .remap_labels(entity_type)]  # fold legacy splits (Local/Other_GSA -> GSA)
  d <- d[entity_type %in% ENTITY_TYPES & !is.na(name) & nzchar(name)]
  # Reproduce the few-shot example names so we can hold them out of the eval set.
  set.seed(20260806)
  ex_names <- d[, .SD[sample(.N, min(.N, CLASSIFIER_CONFIG$examples_per_category))],
                by = entity_type]$name
  pool <- d[!name %in% ex_names]
  # Stratified eval sample: up to PER_CAT names per human category.
  set.seed(20260810)
  samp <- pool[, .SD[sample(.N, min(.N, PER_CAT))], by = entity_type]
  fwrite(samp[, .(name, entity_type)], sample_file)
  cat(sprintf("Eval sample (built + frozen): %d names across %d categories (up to %d each)\n",
              nrow(samp), samp[, uniqueN(entity_type)], PER_CAT))
}

# Route to an isolated eval cache (never touches the production cache).
CLASSIFIER_CONFIG$cache_path <- file.path(eval_dir, sprintf("eval_cache_%s.csv", TAG))

pred <- classify_entities(samp$name)
setnames(pred, "entity_type", "pred")
res <- merge(samp[, .(name, gold = entity_type)], pred, by = "name",
             all.x = TRUE, sort = FALSE)
res[, agree := gold == pred]

cat(sprintf("\n=== Overall agreement: %.1f%% (%d/%d) ===\n",
            100 * mean(res$agree), sum(res$agree), nrow(res)))

cat("\nPer-category recall (gold label -> model reproduces it):\n")
byc <- res[, .(n = .N, recall = round(mean(agree), 3)), by = gold][order(recall)]
print(byc, nrow = 100)

cat("\nTop confusions (human gold -> model pred, disagreements only):\n")
conf <- res[gold != pred, .N, by = .(gold, pred)][order(-N)]
print(head(conf, 30))

out <- file.path(eval_dir, sprintf("eval_results_%s.csv", TAG))
fwrite(res[order(gold, pred)], out)
cat(sprintf("\nWrote %s\n", out))
