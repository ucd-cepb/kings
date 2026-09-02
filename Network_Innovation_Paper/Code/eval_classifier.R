#' Spot-check the entity-type classifier's OUTPUT by hand.
#'
#' There is NO trusted gold label set. The old "seed" (inputs/node_dictionary_seed.csv)
#' was the output of an early, ad hoc LLM pass, not human ground truth, and was
#' retired as unreliable -- see classify_entities.R. The only authoritative labels
#' are the deterministic gazetteer built from core_code/dicts, which already pins the
#' known, enumerable cast. So this script computes NO agreement/accuracy number
#' (scoring against a reference noisier than the model is meaningless). Instead it
#' draws a stratified sample of the SHIPPED labels in data_products/node_dictionary.csv
#' -- up to PER_CAT names per predicted leaf -- and writes them out for a human to
#' eyeball for obvious errors. That is the meaningful quality check here; it surfaces
#' the long-tail LLM calls, which is where mistakes actually live.
#'
#' Usage (from repo root):
#'   NIP_EVAL_PER_CAT=40 Rscript Network_Innovation_Paper/Code/eval_classifier.R
#'   # optional: NIP_EVAL_TAG=run1  -> names output spotcheck_run1.csv

source("Network_Innovation_Paper/Code/_paths.R")
suppressMessages(library(data.table))

PER_CAT <- as.integer(Sys.getenv("NIP_EVAL_PER_CAT", "40"))
TAG     <- Sys.getenv("NIP_EVAL_TAG", "current")

nd_file <- nip_product("node_dictionary.csv")
if (!file.exists(nd_file))
  stop("node_dictionary.csv not found -- run 00_ingest_core.R first: ", nd_file)

nd <- fread(nd_file, colClasses = "character")
nd <- nd[!is.na(entity_type) & nzchar(entity_type) & !is.na(name) & nzchar(name)]

# Stratified sample: up to PER_CAT names per PREDICTED leaf, so the small focal
# leaves (Consultant/Research/NGO) are represented as heavily as the huge
# Non_institutional bucket. Fixed seed -> reproducible sample across runs.
set.seed(20260901)
samp <- nd[, .SD[sample(.N, min(.N, PER_CAT))], by = entity_type]
setorder(samp, entity_type, name)

cat(sprintf("Spot-check sample: %d names across %d predicted leaves (up to %d each)\n",
            nrow(samp), samp[, uniqueN(entity_type)], PER_CAT))
cat("\nLabel distribution in the FULL node_dictionary:\n")
print(nd[, .N, by = entity_type][order(-N)])

out_dir <- nip_product("eval")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
out <- file.path(out_dir, sprintf("spotcheck_%s.csv", TAG))
# `looks_wrong` is an empty column for the reviewer to flag any clearly-off label
# (mark it with an X). There is no automatic verdict -- this is a human read.
samp[, looks_wrong := ""]
fwrite(samp[, .(entity_type, name, looks_wrong)], out)
cat(sprintf("\nWrote %s -- scan by hand and mark the `looks_wrong` column.\n", out))
