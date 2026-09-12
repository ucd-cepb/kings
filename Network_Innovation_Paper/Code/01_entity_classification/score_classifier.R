#' score_classifier.R -- score the entity classifier against a filled gold sheet.
#'
#' Reads the human-labeled goldsheet_<tag>.csv produced by eval_classifier.R,
#' rejoins the model's shipped prediction (from node_dictionary.csv, by name), and
#' reports performance the way the modeling actually uses the labels
#' (_entity_groups.R), not raw 7-way accuracy:
#'
#'   1. NETWORK GATE -- does the name go into a network or not, as precision/recall/F1
#'      with Wilson 95% CIs. This matters most: the "yes" side is the four org types +
#'      Institutional_other; Non_institutional and Institutional_unresolved are both
#'      "no" (neither goes into a network). A miss here silently drops a real actor.
#'   2. PER-TYPE -- one-vs-rest P/R/F1 for GSA/Consultant/Research/NGO (the types the
#'      paper singles out), plus Institutional_other/Institutional_unresolved/
#'      Non_institutional for completeness, and the macro-F1 over the four org types.
#'   3. CONFUSABLE PAIRS -- the small confusion tables for the labels that get mixed
#'      up: Consultant vs Institutional_other, Research vs NGO, and a specific
#'      institution vs a too-vague one (Institutional_unresolved).
#'
#' Every rate is reported twice: NAME-LEVEL (each unique name once = label quality)
#' and MENTION-WEIGHTED (by n appearances = network impact). They routinely diverge.
#'
#' Also writes:
#'   errors_<tag>.csv            every gold != pred disagreement, n-sorted.
#'   promote_candidates_<tag>.csv disagreements where gold is institutional and
#'                                n >= NIP_EVAL_PROMOTE_N (default 6) and the tag was
#'                                LLM-decided -- ready to append to
#'                                inputs/entity_type_overrides.csv (pattern,entity_type,match,notes).
#'   metrics_<tag>.csv           the flat metrics table.
#'
#' Usage (from repo root, after filling the sheet):
#'   NIP_EVAL_TAG=current Rscript Network_Innovation_Paper/Code/01_entity_classification/score_classifier.R

suppressMessages(library(data.table))
source("Network_Innovation_Paper/Code/_paths.R")
source(nip_code("01_entity_classification", "classify_entities.R"))

TAG       <- Sys.getenv("NIP_EVAL_TAG", "current")
PROMOTE_N <- as.integer(Sys.getenv("NIP_EVAL_PROMOTE_N", "6"))
# Names that go into a network: the four org types + Institutional_other (per
# _entity_groups.R). Non_institutional and Institutional_unresolved both stay out,
# so both count as "no" here -- a real actor mislabeled Institutional_unresolved is
# a miss, and a vague name promoted to a specific type is a false alarm.
INST      <- setdiff(ENTITY_TYPES, c("Non_institutional", "Institutional_unresolved"))
FOCAL     <- c("GSA", "Consultant", "Research", "NGO")

out_dir    <- nip_product("01_entity_classification", "eval")
sheet_file <- file.path(out_dir, sprintf("goldsheet_%s.csv", TAG))
if (!file.exists(sheet_file))
  stop("no filled sheet at ", sheet_file, " -- run eval_classifier.R and label it first.")

# ---- load gold + rejoin predictions ------------------------------------------
g <- fread(sheet_file, colClasses = "character")
g <- g[!is.na(gold_type) & nzchar(trimws(gold_type))]
g[, gold_type := trimws(gold_type)]
if (!nrow(g)) stop("no rows in ", sheet_file, " have a `gold_type` -- nothing to score.")

bad <- g[!gold_type %in% ENTITY_TYPES]
if (nrow(bad)) {
  warning(nrow(bad), " row(s) have a gold_type outside the vocabulary; dropping them:\n",
          paste(sprintf("  %s -> '%s'", bad$name, bad$gold_type), collapse = "\n"))
  g <- g[gold_type %in% ENTITY_TYPES]
}

nd <- fread(nip_product("01_entity_classification", "node_dictionary.csv"), colClasses = "character")
g[, pred := nd$entity_type[match(name, nd$name)]]
miss <- g[is.na(pred)]
if (nrow(miss)) {
  warning(nrow(miss), " labeled name(s) not found in node_dictionary.csv; dropping.")
  g <- g[!is.na(pred)]
}
# Flag any labeled name the gazetteer now decides (shouldn't happen -- the sheet is
# the LLM tail -- but the overrides file may have changed since sampling).
ov <- .load_overrides()
g[, decided_by := ifelse(is.na(.match_overrides(name, ov)), "llm", "gazetteer")]
if (g[decided_by == "gazetteer", .N] > 0)
  message("note: ", g[decided_by == "gazetteer", .N],
          " scored name(s) are now gazetteer-decided (overrides changed since sampling).")

g[, w := pmax(suppressWarnings(as.integer(n)), 1L)]  # mention weight (>=1)
g[is.na(w), w := 1L]

# ---- metric helpers ----------------------------------------------------------
wilson <- function(k, m) {                 # 95% CI on a binomial proportion k/m
  if (is.na(m) || m == 0) return(c(NA, NA))
  z <- 1.96; p <- k / m; d <- 1 + z^2 / m
  c((p + z^2/(2*m) - z*sqrt(p*(1-p)/m + z^2/(4*m^2))) / d,
    (p + z^2/(2*m) + z*sqrt(p*(1-p)/m + z^2/(4*m^2))) / d)
}
# Precision/recall/F1 for a binary "positive" definition, name-level (counts) and
# mention-weighted (sums of w). CI is Wilson on the name-level P and R.
prf <- function(pred_pos, gold_pos, w, label) {
  tp <- sum( pred_pos &  gold_pos); fp <- sum( pred_pos & !gold_pos); fn <- sum(!pred_pos & gold_pos)
  wtp <- sum(w[pred_pos &  gold_pos]); wfp <- sum(w[pred_pos & !gold_pos]); wfn <- sum(w[!pred_pos & gold_pos])
  f1 <- function(p, r) if (is.na(p) || is.na(r) || (p + r) == 0) NA_real_ else 2*p*r/(p+r)
  P  <- if (tp+fp) tp/(tp+fp) else NA; R  <- if (tp+fn) tp/(tp+fn) else NA
  wP <- if (wtp+wfp) wtp/(wtp+wfp) else NA; wR <- if (wtp+wfn) wtp/(wtp+wfn) else NA
  pci <- wilson(tp, tp+fp); rci <- wilson(tp, tp+fn)
  data.table(metric = label, support = tp + fn,
             precision = P, prec_lo = pci[1], prec_hi = pci[2],
             recall = R, rec_lo = rci[1], rec_hi = rci[2], f1 = f1(P, R),
             w_precision = wP, w_recall = wR, w_f1 = f1(wP, wR))
}

fmt <- function(x) ifelse(is.na(x), "  -- ", sprintf("%.3f", x))
pl  <- function(r) cat(sprintf("  %-24s n=%-4d  P=%s [%s,%s]  R=%s [%s,%s]  F1=%s   | wtd P=%s R=%s F1=%s\n",
  r$metric, r$support, fmt(r$precision), fmt(r$prec_lo), fmt(r$prec_hi),
  fmt(r$recall), fmt(r$rec_lo), fmt(r$rec_hi), fmt(r$f1),
  fmt(r$w_precision), fmt(r$w_recall), fmt(r$w_f1)))

# ---- report -------------------------------------------------------------------
N <- nrow(g); W <- sum(g$w)
cat(sprintf("\n=== Classifier eval: tag=%s | %d gold names, %d mentions ===\n", TAG, N, W))
acc_n <- g[, mean(pred == gold_type)]; acc_w <- g[, sum(w[pred == gold_type]) / W]
cat(sprintf("Overall 7-way accuracy: %.3f name-level, %.3f mention-weighted\n", acc_n, acc_w))
cat("  (not the headline -- the big easy Non_institutional bucket inflates it; read the per-type metrics.)\n")

cat("\n-- 1. NETWORK GATE (goes into a network: 4 org types + Institutional_other vs the rest) --\n")
cat("  (the rest = Non_institutional + Institutional_unresolved; neither goes into a network.)\n")
gate <- prf(g$pred %in% INST, g$gold_type %in% INST, g$w, "network-entering"); pl(gate)

cat("\n-- 2. PER-TYPE (one-vs-rest) --\n")
leaf_rows <- rbindlist(lapply(ENTITY_TYPES, function(lf)
  prf(g$pred == lf, g$gold_type == lf, g$w, lf)))
for (i in seq_len(nrow(leaf_rows))) pl(leaf_rows[i])
macro <- leaf_rows[metric %in% FOCAL, mean(f1, na.rm = TRUE)]
cat(sprintf("  macro-F1 over the 4 org types (%s): %s\n", paste(FOCAL, collapse=","), fmt(macro)))

cat("\n-- 3. FULL confusion (rows=pred, cols=gold) --\n")
cm <- table(pred = factor(g$pred, ENTITY_TYPES), gold = factor(g$gold_type, ENTITY_TYPES))
print(cm)
cat("\n   confusable pair: Consultant vs Institutional_other\n")
print(cm[c("Consultant","Institutional_other"), c("Consultant","Institutional_other")])
cat("\n   confusable pair: Research vs NGO (vs Consultant)\n")
print(cm[c("Research","NGO","Consultant"), c("Research","NGO","Consultant")])
cat("\n   confusable pair: a specific institution vs a too-vague one (Institutional_unresolved)\n")
print(cm[c(INST, "Institutional_unresolved"), c(INST, "Institutional_unresolved")])

# ---- error + promotion outputs ------------------------------------------------
err <- g[pred != gold_type, .(name, pred, gold = gold_type, n = w, freq_band, decided_by, gold_notes)]
setorder(err, -n)
err_file <- file.path(out_dir, sprintf("errors_%s.csv", TAG)); fwrite(err, err_file)

promote <- err[gold %in% INST & n >= PROMOTE_N & decided_by == "llm",
               .(pattern = name, entity_type = gold, match = "exact",
                 notes = sprintf("from eval %s (was %s, n=%d)", TAG, pred, n))]
prom_file <- file.path(out_dir, sprintf("promote_candidates_%s.csv", TAG)); fwrite(promote, prom_file)

metrics <- rbind(gate, leaf_rows, fill = TRUE)
metrics[, c("tag","overall_acc_name","overall_acc_wtd","macro_f1_focal") := .(TAG, acc_n, acc_w, macro)]
met_file <- file.path(out_dir, sprintf("metrics_%s.csv", TAG)); fwrite(metrics, met_file)

cat(sprintf("\nMisclassifications : %s (%d rows)\n", err_file, nrow(err)))
cat(sprintf("Promote to gazetteer: %s (%d high-n institutional errors -> append to entity_type_overrides.csv)\n",
            prom_file, nrow(promote)))
cat(sprintf("Metrics table      : %s\n", met_file))
