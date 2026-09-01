#' Seed-cleaning pass: flag confident model/seed disagreements for human review.
#'
#' The frozen human seed (inputs/node_dictionary_seed.csv) is both the few-shot
#' source AND the eval "gold" -- but eval has repeatedly shown it is NOISIER than
#' the model on the ambiguous leaves (see eval_classifier.R, nip-entity-taxonomy).
#' So "agreement" is only a FLOOR, never accuracy, because there is no
#' human-verified gold set. This script bootstraps one: it surfaces the seed rows
#' the classifier disagrees with, ranked by how confident that disagreement is, so
#' a human can adjudicate them. Two payoffs:
#'   (a) clean the few-shot POOL -- a mislabeled seed row shown in the prompt
#'       actively poisons every batch, so those are the highest-value fixes; and
#'   (b) bootstrap a real GOLD set -- adjudicated rows become verified labels.
#'
#' It does NOT edit the seed. It writes a review CSV with an empty `verdict`
#' column for a human to fill (seed_wrong / model_wrong / ambiguous), plus which
#' seed label to keep. Feed the verdicts back by hand.
#'
#' Design (mirrors eval_classifier.R conventions):
#'   - PASS 1 is FREE: seed names already in the production node_dictionary.csv
#'     were classified WITH production hints (spaCy tag + freq) and the gazetteer,
#'     so we score against that output directly -- no API calls. ~10k of the ~18k
#'     seed names are in-core; the rest are legacy names no longer in core.
#'   - Confidence tiers, most-confident first:
#'       override_conflict  the curated gazetteer says X, the seed says Y. The
#'                          gazetteer is hand-curated identity fact, so the seed is
#'                          almost certainly wrong (or, rarely, a gazetteer bug --
#'                          either way it MUST be reviewed).
#'       both_models_vs_seed (only with adjudication) Haiku AND Sonnet agree on X,
#'                          seed says Y -- two independent models beat one noisy
#'                          human label.
#'       llm_conflict       the Haiku production label disagrees with the seed, no
#'                          second signal yet.
#'       models_split       (only with adjudication) Haiku != Sonnet -- genuinely
#'                          ambiguous; low priority, likely a taxonomy grey zone.
#'   - OPT-IN adjudication (NIP_SEED_ADJUDICATE=1): re-classify the llm_conflict
#'     rows with a STRONGER model (NIP_ADJUDICATOR_MODEL, default claude-sonnet-5)
#'     to split them into both_models_vs_seed vs models_split. Costs API $, so it
#'     is off by default. Runs hint-free (like eval_classifier) and routes to an
#'     ISOLATED cache under data_products/eval/, never the production cache.
#'
#' Usage (from repo root):
#'   Rscript Network_Innovation_Paper/Code/seed_review.R              # free pass 1
#'   NIP_SEED_ADJUDICATE=1 Rscript Network_Innovation_Paper/Code/seed_review.R
#'   # NIP_ADJUDICATOR_MODEL=claude-opus-5  -> spend more on the adjudicator
#'   # NIP_SEED_INCLUDE_OUTOFCORE=1         -> also classify the ~8k legacy names
#'   #                                         not in current core (API $, hint-free)

source("Network_Innovation_Paper/Code/_paths.R")
source("Network_Innovation_Paper/Code/classify_entities.R")
suppressMessages(library(data.table))

eval_dir <- nip_product("eval")
dir.create(eval_dir, showWarnings = FALSE, recursive = TRUE)

ADJUDICATE       <- nzchar(Sys.getenv("NIP_SEED_ADJUDICATE", ""))
INCLUDE_OUTOFCORE <- nzchar(Sys.getenv("NIP_SEED_INCLUDE_OUTOFCORE", ""))
ADJ_MODEL        <- Sys.getenv("NIP_ADJUDICATOR_MODEL", "claude-sonnet-5")

# ---- Load the seed, folded into the six-leaf vocabulary ----------------------
seed <- fread(nip_input("node_dictionary_seed.csv"), colClasses = "character")
seed <- seed[!is.na(name) & nzchar(name)]
seed[, seed_raw := entity_type]
seed[, seed6 := .remap_labels(entity_type)]
seed <- seed[seed6 %in% ENTITY_TYPES]
seed[, entity_type := NULL]

# Mark the few-shot example names by parsing the ACTUAL prompt block
# .build_examples() emits (lines like `  "name" -> Type`), so the flag is exactly
# the set of names the model is shown in every batch -- not a re-sample that could
# drift from the real RNG draw. A mislabeled few-shot row poisons every prompt, so
# these are the top-priority fixes.
ex_names <- regmatches(.build_examples(),
                       gregexpr('(?<=")[^"]+(?=" ->)', .build_examples(), perl = TRUE))[[1]]
seed[, is_fewshot := name %in% ex_names]

# ---- Production predictions (free): the node_dictionary was built WITH hints --
nd <- fread(nip_product("node_dictionary.csv"), colClasses = "character")
setnames(nd, "entity_type", "prod_pred")
seed <- merge(seed, nd[, .(name, prod_pred)], by = "name", all.x = TRUE)
seed[, in_core := !is.na(prod_pred)]

# Which production predictions came from the deterministic gazetteer (authoritative)?
ov <- .load_overrides()
seed[, ov_type := .match_overrides(name, ov)]
seed[, source := fifelse(!is.na(ov_type), "override",
                  fifelse(in_core, "llm", "not_in_core"))]

cat(sprintf("Seed: %d rows (%d in-core / %d legacy-not-in-core), %d few-shot examples\n",
            nrow(seed), sum(seed$in_core), sum(!seed$in_core), sum(seed$is_fewshot)))

# ---- Optional: classify the legacy out-of-core names (API $, hint-free) -------
if (INCLUDE_OUTOFCORE) {
  todo <- seed[in_core == FALSE, unique(name)]
  if (length(todo)) {
    CLASSIFIER_CONFIG$cache_path <- file.path(eval_dir, "seed_outofcore_cache.csv")
    cat(sprintf("Classifying %d out-of-core legacy names (hint-free)...\n", length(todo)))
    p <- classify_entities(todo)
    setnames(p, "entity_type", "prod_pred")
    seed[p, prod_pred := i.prod_pred, on = "name"]
    # these carry no gazetteer/hint context; source stays 'not_in_core'
  }
}

# ---- Score model vs seed -----------------------------------------------------
seed[, agree := !is.na(prod_pred) & prod_pred == seed6]
scored <- seed[!is.na(prod_pred)]
cat(sprintf("\nScored %d names.  Agreement (floor, not accuracy): %.1f%% (%d/%d)\n",
            nrow(scored), 100 * mean(scored$agree), sum(scored$agree), nrow(scored)))

# ---- Confidence tier for each DISAGREEMENT -----------------------------------
disagree <- scored[agree == FALSE]
disagree[, tier := fifelse(source == "override", "override_conflict", "llm_conflict")]

# ---- Optional adjudication: a stronger model on the llm_conflict rows ---------
if (ADJUDICATE) {
  adj_rows <- disagree[tier == "llm_conflict", unique(name)]
  if (length(adj_rows)) {
    prev_model <- CLASSIFIER_CONFIG$model
    CLASSIFIER_CONFIG$model      <- ADJ_MODEL
    CLASSIFIER_CONFIG$cache_path <- file.path(eval_dir, "seed_adjudicate_cache.csv")
    cat(sprintf("\nAdjudicating %d llm_conflict rows with %s (hint-free, isolated cache)...\n",
                length(adj_rows), ADJ_MODEL))
    adj <- classify_entities(adj_rows)          # no hints, no overrides applied to result
    setnames(adj, "entity_type", "adj_pred")
    disagree <- merge(disagree, adj, by = "name", all.x = TRUE)
    disagree[tier == "llm_conflict" & !is.na(adj_pred),
             tier := fifelse(adj_pred == prod_pred, "both_models_vs_seed", "models_split")]
    CLASSIFIER_CONFIG$model <- prev_model
  }
} else {
  disagree[, adj_pred := NA_character_]
}

# ---- Rank, most-confident disagreement first ---------------------------------
TIER_ORDER <- c("override_conflict", "both_models_vs_seed", "llm_conflict", "models_split")
disagree[, tier := factor(tier, levels = TIER_ORDER)]
# few-shot rows float to the top within their tier: they poison the prompt.
disagree <- disagree[order(tier, -is_fewshot, name)]

review <- disagree[, .(
  name,
  seed_label   = seed6,
  seed_raw,
  model_pred   = prod_pred,
  adjudicator  = adj_pred,
  source,
  is_fewshot,
  tier,
  verdict      = "",   # human fills: seed_wrong | model_wrong | ambiguous
  keep_label   = ""    # human fills: the correct six-leaf label
)]

out <- file.path(eval_dir, "seed_review.csv")
fwrite(review, out)

# ---- Summary -----------------------------------------------------------------
cat("\n=== Disagreements by tier (review these top-down) ===\n")
print(review[, .(n = .N, fewshot = sum(is_fewshot)), by = tier])
cat("\n=== Confusion (seed label -> model pred), disagreements only ===\n")
print(disagree[, .N, by = .(seed6, prod_pred)][order(-N)][1:min(20, .N)])
cat(sprintf("\nWrote %s  (%d rows to review)\n", out, nrow(review)))
if (!ADJUDICATE)
  cat("Adjudication OFF. Re-run with NIP_SEED_ADJUDICATE=1 to split llm_conflict",
      "into both_models_vs_seed (high confidence) vs models_split (ambiguous).\n")
