#' write_bag_review.R — human-readable variant -> GSA bag dictionary.
#'
#' Writes data_products/01_entity_classification/GSA_BAGS_review.txt: every GSA
#' roster bag (bag_id gsa_<id>) in entity_bag_map.csv, with each variant that
#' folds into it and the tier that folded it (<exact>, <subset>, <override>, ...).
#' Single-name bags are listed too, so every bag can be eyeballed. A variant
#' naming several GSAs appears in each of their bags, tagged
#' <override; also gsa_266,...> with the other bags it counts in.
#'
#' Bags are ordered by name count (desc), then label; variants alphabetically.
#' Label = the map's bag_label, else the override file's canonical_label, else
#' the bag_id.
#'
#' Reads:  entity_bag_map.csv, inputs/entity_bag_overrides_gsa.csv
#' Writes: GSA_BAGS_review.txt
#' Run after build_entity_bag_map.R.

suppressMessages(library(data.table))

.this_dir <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) NA)
if (is.na(.this_dir)) {
  a <- commandArgs(FALSE); m <- grep("^--file=", a, value = TRUE)
  .this_dir <- if (length(m)) dirname(normalizePath(sub("^--file=", "", m[1]))) else
    "Network_Innovation_Paper/Code/01_entity_classification"
}
source(file.path(.this_dir, "..", "_paths.R"))

m <- fread(nip_product("01_entity_classification", "entity_bag_map.csv"))[grepl("^gsa_[0-9]+$", bag_id)]
lab <- m[bag_label != bag_id, .(lab = bag_label[1]), by = bag_id]
o <- fread(nip_input("entity_bag_overrides_gsa.csv"))[canonical_label != "", .(lab2 = canonical_label[1]), by = bag_id]
m <- lab[m, on = "bag_id"]
m <- o[m, on = "bag_id"]
m[, lbl := fcoalesce(lab, lab2, bag_id)]
# a variant naming several GSAs sits in each of their bags; tag it with the others
m[, also := if (.N > 1) vapply(bag_id, function(b) paste(setdiff(bag_id, b), collapse = ","), "") else "", by = variant]
m[, tag := fifelse(nzchar(also), sprintf("%s; also %s", method, also), method)]

n <- m[, .N, by = .(bag_id, lbl)][order(-N, lbl)]
out <- character()
for (i in seq_len(nrow(n))) {
  r <- m[bag_id == n$bag_id[i]][order(variant)]
  out <- c(out, "",
           sprintf("### %s  [%s]  (%d name%s)", n$lbl[i], n$bag_id[i], n$N[i], ifelse(n$N[i] == 1, "", "s")),
           sprintf("    %-56s <%s>", r$variant, r$tag))
}
f <- nip_product("01_entity_classification", "GSA_BAGS_review.txt")
writeLines(out, f)
message(sprintf("wrote %s: %d bags, %d names (%d single-name)", f, nrow(n), uniqueN(m$variant), sum(n$N == 1)))
