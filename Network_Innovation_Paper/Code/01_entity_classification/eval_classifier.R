#' eval_classifier.R -- generate a BLIND gold-labeling workbook for the entity
#' classifier.
#'
#' There is no trusted gold label set (the old "seed" was retired as unvetted LLM
#' output -- see classify_entities.R). This script does not invent one; it produces
#' the SHEET a human fills in to CREATE one, then score_classifier.R reads it back.
#'
#' Design decisions, and why:
#'   - EVAL THE LLM TAIL, NOT THE GAZETTEER. Of ~92k shipped tags, ~1.8k are pinned
#'     by the deterministic gazetteer (entity_type_overrides.csv) -- those are true
#'     by assertion, so scoring them is meaningless. We recompute the gazetteer hit
#'     with the classifier's own .match_overrides() and sample ONLY the names it did
#'     not decide, i.e. the LLM's calls, which is where errors actually live.
#'   - BLIND. The sheet shows the labeler name + spaCy hint + frequency ONLY -- never
#'     the model's prediction. Seeing the guess anchors the human to it. The pred is
#'     stashed in a separate strata key (never opened while labeling) for auditing.
#'   - STRATIFY by predicted type x frequency band, oversampling the four org types
#'     (GSA/Consultant/Research/NGO) and the most-mentioned names. Those types are
#'     small but mention-heavy; a proportional sample would under-cover them and the
#'     one place a wrong label really distorts the network -- a much-mentioned name.
#'   - Frequency (n appearances) and spaCy tag are NOT stored in node_dictionary.csv,
#'     so we re-aggregate them from the core disambig objects exactly as
#'     build_node_dictionary.R does, and cache the table for cheap re-runs.
#'
#' Output (data_products/01_entity_classification/eval/):
#'   goldsheet_<tag>.csv   <- BLIND sheet: row_id,name,spacy,n,freq_band,gold_type,gold_notes
#'                            Fill `gold_type` with one of the seven types for each row
#'                            (leave `gold_notes` for anything ambiguous), then run
#'                            score_classifier.R.
#'   strata_<tag>.csv      <- hidden key (name,pred,decided_by,freq_band); do NOT
#'                            open while labeling.
#'   entity_frequencies.csv <- cached (name,spacy,n) aggregation.
#'
#' Usage (from repo root):
#'   Rscript Network_Innovation_Paper/Code/01_entity_classification/eval_classifier.R
#'   NIP_EVAL_TARGET=400 NIP_EVAL_TAG=run1 Rscript .../eval_classifier.R
#'   NIP_EVAL_REFRESH_FREQ=TRUE ...   # rebuild the frequency cache from disambig
#'
#' The seven types, for the labeler's reference (full rules: classify_entities.R):
#'   GSA                 groundwater sustainability agency (named or generic)
#'   Consultant          private engineering/hydro/technical CONSULTING firm (named)
#'   Research            knowledge-producing university/lab/institute, NAMED (non-govt)
#'   NGO                 advocacy/conservation/membership nonprofit (named)
#'   Institutional_other any other real, SPECIFIC actor: a named city/county/district,
#'                       state/federal agency (incl. usgs/usbr), non-consulting
#'                       company, or named committee
#'   Institutional_unresolved institutional but too generic to disambiguate to a
#'                       specific actor: a bare category/role -- "university", "the
#'                       district", "a consultant", "local agencies", "the county"
#'                       (no place named). The test: can you point to WHICH actor?
#'                       If not, but it IS an institution, it goes here (NOT into a
#'                       specific type, and NOT Non_institutional).
#'   Non_institutional   not an institution at all: person, basin/feature, project,
#'                       model, law/citation, journal, document ref, concept, OCR junk

suppressMessages(library(data.table))
source("Network_Innovation_Paper/Code/_paths.R")
# Sourcing the classifier pulls in ENTITY_TYPES + .load_overrides/.match_overrides
# so the gazetteer split here is byte-identical to production. It does NOT hit the
# API (only classify_entities() does).
source(nip_code("01_entity_classification", "classify_entities.R"))

TARGET       <- as.integer(Sys.getenv("NIP_EVAL_TARGET", "400"))
TAG          <- Sys.getenv("NIP_EVAL_TAG", "current")
REFRESH_FREQ <- toupper(Sys.getenv("NIP_EVAL_REFRESH_FREQ", "FALSE")) %in% c("TRUE","1","YES")
FOCAL        <- c("GSA", "Consultant", "Research", "NGO")
set.seed(20260901)  # reproducible sample across runs

out_dir <- nip_product("01_entity_classification", "eval")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ---- frequency + spaCy tag, re-aggregated from core disambig objects ----------
# Mirrors build_node_dictionary.R: one row per name, modal spaCy tag + total
# appearances. Cached so repeated eval runs don't re-read every disambig RDS.
freq_file <- file.path(out_dir, "entity_frequencies.csv")
if (file.exists(freq_file) && !REFRESH_FREQ) {
  message("using cached frequencies: ", freq_file, " (NIP_EVAL_REFRESH_FREQ=TRUE to rebuild)")
  freq <- fread(freq_file, colClasses = c(name = "character", spacy = "character"))
} else {
  fs <- list.files(core_disambig(), pattern = "\\.RDS$", full.names = TRUE)
  if (!length(fs)) stop("no disambig objects found under ", core_disambig())
  message("aggregating frequencies from ", length(fs), " disambig objects...")
  agg <- rbindlist(lapply(fs, function(f) {
    nl <- readRDS(f)$nodelist
    data.table(name  = as.character(nl$entity_name),
               spacy = as.character(nl$entity_type),
               n     = suppressWarnings(as.numeric(nl$num_appearances)))
  }), fill = TRUE)
  agg <- agg[!is.na(name) & nzchar(name)]
  freq <- agg[, .(spacy = spacy[which.max(ifelse(is.na(n), 0, n))][1],
                  n = sum(n, na.rm = TRUE)), by = name]
  fwrite(freq, freq_file)
  message("wrote ", freq_file, " (", nrow(freq), " names)")
}

# ---- predictions + gazetteer split -------------------------------------------
nd_file <- nip_product("01_entity_classification", "node_dictionary.csv")
if (!file.exists(nd_file))
  stop("node_dictionary.csv not found -- run build_node_dictionary.R first: ", nd_file)
nd <- fread(nd_file, colClasses = "character")
nd <- nd[!is.na(entity_type) & nzchar(entity_type) & !is.na(name) & nzchar(name)]

ov <- .load_overrides()
nd[, decided_by := ifelse(is.na(.match_overrides(name, ov)), "llm", "gazetteer")]

pool <- merge(nd, freq, by = "name", all.x = TRUE, sort = FALSE)
pool[is.na(n), n := 0]
pool[, n := round(n)]
# Frequency bands: singletons vs a low-repeat middle vs the mention-heavy tail
# (where a wrong tag most distorts the network -- so it is oversampled below).
pool[, freq_band := fifelse(n <= 1, "1", fifelse(n <= 5, "2-5", "6+"))]
pool[, freq_band := factor(freq_band, levels = c("6+", "2-5", "1"))]

cat(sprintf("Shipped tags: %d total | %d gazetteer-pinned (excluded) | %d LLM-decided (eligible)\n",
            nrow(nd), nd[decided_by == "gazetteer", .N], nd[decided_by == "llm", .N]))
llm <- pool[decided_by == "llm"]

# ---- stratified allocation ----------------------------------------------------
# Per-type target: the four org types (rare, mention-heavy, error-prone) and the
# other small types get a bigger slice than a proportional draw would. Capped at
# what the LLM actually produced for each type -- a shortfall is reported, not
# backfilled from another type (so org-type coverage stays honest).
leaf_props <- c(GSA = .14, Consultant = .16, Research = .16, NGO = .16,
                Institutional_other = .14, Institutional_unresolved = .14,
                Non_institutional = .10)
present  <- intersect(names(leaf_props), unique(llm$entity_type))
leaf_tgt <- setNames(round(TARGET * leaf_props[present] / sum(leaf_props[present])), present)

# Within a type, split across bands oversampling the high-frequency end; any band
# shortfall backfills from the other bands, higher-frequency first.
band_props <- c("6+" = .45, "2-5" = .35, "1" = .20)
alloc_bands <- function(avail_by_band, tgt) {
  bands <- c("6+", "2-5", "1")
  want  <- round(tgt * band_props[bands]); names(want) <- bands
  take  <- pmin(want, avail_by_band[bands]); take[is.na(take)] <- 0
  short <- tgt - sum(take)
  for (b in bands) {                      # backfill leftover capacity, hi-freq first
    if (short <= 0) break
    room <- (avail_by_band[b] %||% 0) - take[b]
    add  <- min(room, short); take[b] <- take[b] + add; short <- short - add
  }
  take
}

samp <- rbindlist(lapply(present, function(lf) {
  d <- llm[entity_type == lf]
  avail <- table(factor(d$freq_band, levels = c("6+", "2-5", "1")))
  take  <- alloc_bands(avail, leaf_tgt[[lf]])
  rbindlist(lapply(names(take), function(b) {
    k <- take[[b]]; if (!k) return(NULL)
    db <- d[freq_band == b]
    db[sample(.N, min(.N, k))]
  }))
}))

# ---- write the blind sheet + hidden strata key --------------------------------
setorder(samp, name)                       # order by NAME so the sheet's row order
samp[, row_id := .I]                        # leaks nothing about the predicted type
sheet <- samp[, .(row_id, name, spacy, n, freq_band, gold_type = "", gold_notes = "")]
sheet_file  <- file.path(out_dir, sprintf("goldsheet_%s.csv", TAG))
strata_file <- file.path(out_dir, sprintf("strata_%s.csv",  TAG))
fwrite(sheet, sheet_file)
fwrite(samp[, .(name, pred = entity_type, decided_by, freq_band, n)], strata_file)

cat(sprintf("\nSampled %d LLM-decided names across %d types.\n", nrow(samp), samp[, uniqueN(entity_type)]))
cat("\nRealized allocation (predicted type x frequency band):\n")
print(dcast(samp, entity_type ~ freq_band, fun.aggregate = length, value.var = "name"))
shortfall <- leaf_tgt[present] - samp[, .N, by = entity_type][match(present, entity_type), N]
if (any(shortfall > 0, na.rm = TRUE)) {
  cat("\nTypes under target (too few LLM names -- expected for gazetteer-heavy types):\n")
  print(data.table(type = present, target = leaf_tgt[present],
                    got = leaf_tgt[present] - shortfall)[shortfall > 0])
}
cat(sprintf("\nBLIND sheet : %s\n  -> fill `gold_type` (one of: %s) for every row, WITHOUT peeking at strata_%s.csv\n",
            sheet_file, paste(ENTITY_TYPES, collapse = " / "), TAG))
cat(sprintf("Strata key  : %s  (hidden; for scoring/audit only)\n", strata_file))
cat(sprintf("Then score  : NIP_EVAL_TAG=%s Rscript Network_Innovation_Paper/Code/01_entity_classification/score_classifier.R\n", TAG))
