#' build_entity_bag_lookup.R — one join table for every in-network name variant.
#'
#' The bag redesign runs as several INDEPENDENT processes, each owning one entity
#' type and one bag namespace. This script does no matching of its own; it just
#' assembles their outputs into the single lookup the edge builders and the
#' modeling grouping code join on, and proves the namespaces stay disjoint.
#'
#'   entity_type           bag namespace   authority / producer
#'   --------------------  --------------  -------------------------------------
#'   GSA                   gsa_<GSA_ID>    build_entity_bag_map.R  + the signed-off
#'                                         inputs/entity_bag_overrides_gsa.csv
#'   Consultant            con_*           build_alter_bag_map.R (consultant_dictionary)
#'   NGO                   ngo_*           build_alter_bag_map.R (self-cluster)
#'   Research              res_*           build_alter_bag_map.R (self-cluster)
#'   Institutional_other   io_*            build_alter_bag_map.R (identity, DEFERRED)
#'
#' GSA rows are taken ONLY from entity_bag_map.csv: that process is the load-bearing
#' one, it is signed off, and nothing in the alter processes may move a GSA bag. The
#' alter types are taken ONLY from alter_bag_map.csv, which supersedes the identity
#' passthrough those types still have inside entity_bag_map.csv.
#'
#' `bag_type` is the entity type that owns the bag. Every bag is type-homogeneous by
#' construction (each process only ever folds within its own type), and that is
#' asserted here rather than assumed.
#'
#' Reads : entity_bag_map.csv, alter_bag_map.csv
#' Writes: data_products/01_entity_classification/entity_bag_lookup.csv
#'   columns: variant, entity_type, bag_id, bag_label, bag_type, method, source
#'
#' Run from the repo root:
#'   Rscript Network_Innovation_Paper/Code/01_entity_classification/build_entity_bag_lookup.R
#' Set CLOBBER=TRUE to overwrite an existing entity_bag_lookup.csv.

suppressMessages(library(data.table))

.this_dir <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) NA)
if (is.na(.this_dir)) {
  a <- commandArgs(FALSE); m <- grep("^--file=", a, value = TRUE)
  .this_dir <- if (length(m)) dirname(normalizePath(sub("^--file=", "", m[1]))) else
    "Network_Innovation_Paper/Code/01_entity_classification"
}
source(file.path(.this_dir, "..", "_paths.R"))

CLOBBER <- toupper(Sys.getenv("CLOBBER", "FALSE")) %in% c("TRUE", "1", "YES")

# entity_type -> the namespace prefix that owns its bags
BAG_TYPE_OF <- c(GSA = "gsa", Consultant = "con", NGO = "ngo",
                 Research = "res", Institutional_other = "io")

build_entity_bag_lookup <- function() {
  out <- nip_product("01_entity_classification", "entity_bag_lookup.csv")
  if (file.exists(out) && !CLOBBER) {
    message("exists, skipping (set CLOBBER=TRUE to rebuild): ", out); return(invisible(NULL))
  }

  ego <- fread(nip_product("01_entity_classification", "entity_bag_map.csv"),
               colClasses = "character")[entity_type == "GSA"]
  alt <- fread(nip_product("01_entity_classification", "alter_bag_map.csv"),
               colClasses = "character")

  keep <- c("variant", "entity_type", "bag_id", "bag_label", "method")
  if (!"joined" %in% names(ego)) ego[, joined := NA_character_]
  if (!"joined" %in% names(alt)) alt[, joined := NA_character_]
  keep <- c(keep, "joined")
  lk <- rbind(ego[, ..keep][, source := "entity_bag_map"],
              alt[, ..keep][, source := "alter_bag_map"])
  lk[, bag_type := BAG_TYPE_OF[entity_type]]

  # ---- assertions ----------------------------------------------------------
  if (anyDuplicated(lk$variant)) {
    d <- lk$variant[duplicated(lk$variant)]
    stop("variant appears in more than one process: ", paste(head(d, 5), collapse = ", "))
  }
  if (anyNA(lk$bag_type)) stop("entity_type with no owning namespace: ",
                               paste(unique(lk[is.na(bag_type), entity_type]), collapse = ", "))
  leak <- lk[, uniqueN(bag_type), by = bag_id][V1 > 1L]
  if (nrow(leak)) stop("bag_id spans >1 namespace: ", paste(head(leak$bag_id, 5), collapse = ", "))
  # The check above only catches one bag_id STRING claimed by two types, which the
  # upstream processes already prevent — it never tested the stated invariant that a
  # bag sits inside its own type's namespace. This does.
  #
  # The GSA namespace is the documented exception: build_entity_bag_map.R leaves an
  # unmatched GSA variant on a BARE identity bag ("los_angeles_gsa"), so a gsa_* bag
  # is not prefix-identifiable. Six of them are even spelled "gsa_<word>" because the
  # VARIANT starts with "gsa_". Downstream code must therefore always carry bag_type
  # and never re-derive a type from the bag_id prefix.
  mis <- lk[entity_type != "GSA" & sub("_.*$", "", bag_id) != bag_type]
  if (nrow(mis)) stop(sprintf("%d bag_id(s) outside their type's namespace: %s",
                              nrow(mis), paste(head(mis$bag_id, 5), collapse = ", ")))
  nbare <- lk[entity_type == "GSA" & !grepl("^gsa_", bag_id), .N]
  # a GSA bag that resolves to a roster id is the only kind that can author a plan
  lk[, is_roster_gsa := grepl("^gsa_[0-9]+$", bag_id)]

  fwrite(lk, out)
  message(sprintf("wrote %s (%d variants -> %d bags)", out, nrow(lk), uniqueN(lk$bag_id)))
  cat("\n== bags by owning type ==\n")
  print(lk[, .(variants = .N, bags = uniqueN(bag_id),
               folded = sum(!method %in% c("identity")),
               shrink = sprintf("%.1f%%", 100 * (1 - uniqueN(bag_id) / .N))),
           by = .(entity_type, bag_type)][order(entity_type)])
  cat(sprintf("\nGSA bags on a roster id (can author a plan): %d of %d GSA bags\n",
              lk[is_roster_gsa == TRUE, uniqueN(bag_id)], lk[entity_type == "GSA", uniqueN(bag_id)]))
  cat(sprintf("GSA variants on a BARE (un-prefixed) identity bag: %d — bag_type is NOT\n  derivable from the bag_id prefix for GSA; always carry the bag_type column.\n",
              nbare))
  invisible(lk)
}

if (sys.nframe() == 0) {
  message("== build_entity_bag_lookup.R (CLOBBER=", CLOBBER, ") ==")
  build_entity_bag_lookup()
  message("== entity_bag_lookup complete ==")
}
