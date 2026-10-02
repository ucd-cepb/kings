#' build_entity_bag_map.R — fold messy entity-name variants onto canonical "bags".
#'
#' The research question asks how a GSA's ties shape its plan content, but the raw
#' node_dictionary fragments each real agency across dozens of spelling variants
#' ("aliso_gsa", "aliso_water_district_gsa", "aliso_wd_gsa", ...). This script
#' builds a variant -> bag_id map so downstream edge/matrix code can represent each
#' agency (and each consultant) ONCE instead of once per spelling.
#'
#' Bag authority (locked design decisions):
#'   - GSA bags: the 259 canonical gsa_ids AFFILIATED with some plan, per
#'     id_crosswalk.csv (gsa_ids column). Canonical names come from the core
#'     roster sgma_gsa_full.csv (GSA_ID -> GSA_Name). bag_id = "gsa_<GSA_ID>".
#'   - Consultant bags: the flat consultant_dictionary.csv firm roster.
#'     bag_id = "con_<normalized firm name>".
#'   - Research / NGO / Institutional_other: NO dictionary exists, so identity
#'     passthrough (bag_id = the variant itself). YAGNI until a roster exists.
#'
#' Matching is HIGH-PRECISION-auto / everything-else-to-review. A variant is folded
#' automatically only when the evidence is unambiguous; every uncertain case is
#' written to entity_bag_unmatched.csv WITH a best suggestion for a human to
#' confirm. Unconfirmed variants default to their OWN identity bag, so the pipeline
#' always runs and review is monotonic improvement, never a blocker.
#'
#' Tiers (GSA), conservative stoplist = SGMA boilerplate only:
#'   exact       node == roster key                              -> auto
#'   subset      roster distinctive tokens ⊆ node tokens, 1 id   -> auto  (elaborated variant)
#'   revsubset   node distinctive tokens ⊆ roster tokens, 1 id   -> auto  (abbreviated variant)
#'   *_tie       the above but >1 candidate id                   -> review (suggest)
#'   fuzzy       best base-R adist similarity                    -> review (suggest)
#'   none        no candidate                                    -> review (no suggestion)
#' Consultants: exact -> auto; anything else -> review (fuzzy suggestion), identity default.
#'
#' A curated inputs/entity_bag_overrides.csv (variant,bag_id[,entity_type]) wins
#' over everything — this is where confirmed review decisions get promoted.
#'
#' Reads : id_crosswalk.csv, node_dictionary.csv (paper products); sgma_gsa_full.csv
#'         (core roster); consultant_dictionary.csv (core dict); optional override.
#' Writes: data_products/01_entity_classification/entity_bag_map.csv
#'         data_products/01_entity_classification/entity_bag_unmatched.csv
#'
#' Run from the repo root:
#'   Rscript Network_Innovation_Paper/Code/01_entity_classification/build_entity_bag_map.R
#' Set CLOBBER=TRUE to overwrite existing outputs.

suppressMessages(library(data.table))

.this_dir <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) NA)
if (is.na(.this_dir)) {
  a <- commandArgs(FALSE); m <- grep("^--file=", a, value = TRUE)
  .this_dir <- if (length(m)) dirname(normalizePath(sub("^--file=", "", m[1]))) else
    "Network_Innovation_Paper/Code/01_entity_classification"
}
source(file.path(.this_dir, "..", "_paths.R"))

CLOBBER <- toupper(Sys.getenv("CLOBBER", "FALSE")) %in% c("TRUE", "1", "YES")

# ---- name normalization (identical convention to build_overrides_from_dicts.R) ----
.norm <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("[^a-z0-9]+", "_", x)
  gsub("^_+|_+$", "", x)
}
# conservative stoplist: SGMA boilerplate + articles only. county/city/water/
# district/authority/basin etc are KEPT -- they distinguish sibling agencies
# (City of Madera vs County of Madera vs Madera Water District).
.STOP <- c("gsa","gsas","groundwater","sustainability","sustainable","agency",
           "agencies","the","of","a","for","and","board","directors","director",
           "committee","plan","plans","gsp","gsps","management","act","sgma")
.toks  <- function(x) { t <- strsplit(x, "_", fixed = TRUE)[[1]]; t[nzchar(t)] }
.dtoks <- function(x) setdiff(.toks(x), .STOP)
# normalized edit-distance similarity (base R; avoids a stringdist dependency)
.sim <- function(a, b) { d <- as.integer(adist(a, b)); 1 - d / pmax(nchar(a), nchar(b)) }
FUZZY_SHOW <- 0.75   # below this, offer no suggestion (method = none)

# ---- rosters -----------------------------------------------------------------
load_gsa_roster <- function(xw) {
  ids <- unique(trimws(unlist(strsplit(xw$gsa_ids, ","))))
  ids <- ids[nzchar(ids) & !is.na(ids)]
  g <- fread(core_gsa_full(), colClasses = "character")[GSA_ID %in% ids]
  ros <- unique(g[, .(gsa_id = GSA_ID, label = GSA_Name)])
  ros[, key := .norm(label)]
  ros <- ros[nzchar(key)]
  ros[, dt := lapply(key, .dtoks)]
  ros[, ndt := lengths(dt)]
  ros[]
}
load_consultant_roster <- function() {
  d <- fread(core_dict("consultant_dictionary.csv"), colClasses = "character")
  d <- d[!is.na(all_names) & nzchar(all_names)]
  ros <- data.table(label = d$all_names)
  ros[, key := .norm(label)]
  unique(ros[nzchar(key)])
}

# ---- GSA matcher -------------------------------------------------------------
# returns list(bag_id, label, method, score) ; bag_id NA_character_ when no auto.
.match_gsa <- function(node, ros) {
  usable <- ros[ndt >= 1]
  nt  <- .toks(node)
  ndd <- .dtoks(node)
  # exact
  hit <- ros[key == node]
  if (nrow(hit)) return(list(id = hit$gsa_id[1], lab = hit$label[1], m = "exact", s = 1))
  # subset: roster distinctive tokens all present in node
  if (length(nt)) {
    ok <- vapply(usable$dt, function(d) all(d %in% nt), logical(1))
    if (any(ok)) {
      cand <- usable[ok]; best <- cand[ndt == max(ndt)]
      ib <- unique(best$gsa_id)
      if (length(ib) == 1) return(list(id = ib, lab = best$label[1], m = "subset", s = max(best$ndt)))
      return(list(id = NA_character_, sug = best$gsa_id[1], lab = best$label[1], m = "subset_tie", s = length(ib)))
    }
  }
  # revsubset: node distinctive tokens all contained in a roster entry
  if (length(ndd)) {
    ok <- vapply(usable$dt, function(d) all(ndd %in% d), logical(1))
    if (any(ok)) {
      cand <- usable[ok]; best <- cand[ndt == min(ndt)]
      ib <- unique(best$gsa_id)
      if (length(ib) == 1) return(list(id = ib, lab = best$label[1], m = "revsubset", s = length(ndd)))
      return(list(id = NA_character_, sug = best$gsa_id[1], lab = best$label[1], m = "revsubset_tie", s = length(ib)))
    }
  }
  # fuzzy suggestion
  s <- .sim(node, usable$key); j <- which.max(s)
  if (length(j) && s[j] >= FUZZY_SHOW)
    return(list(id = NA_character_, sug = usable$gsa_id[j], lab = usable$label[j], m = "fuzzy", s = round(s[j], 3)))
  list(id = NA_character_, lab = NA_character_, m = "none", s = NA_real_)
}

# ---- consultant matcher (exact -> auto; else fuzzy suggestion) ---------------
.match_consultant <- function(node, ros) {
  hit <- ros[key == node]
  if (nrow(hit)) return(list(id = paste0("con_", hit$key[1]), lab = hit$label[1], m = "exact", s = 1))
  s <- .sim(node, ros$key); j <- which.max(s)
  if (length(j) && s[j] >= FUZZY_SHOW)
    return(list(id = NA_character_, sug = paste0("con_", ros$key[j]), lab = ros$label[j], m = "fuzzy", s = round(s[j], 3)))
  list(id = NA_character_, lab = NA_character_, m = "none", s = NA_real_)
}

build_entity_bag_map <- function() {
  out_map <- nip_product("01_entity_classification", "entity_bag_map.csv")
  out_un  <- nip_product("01_entity_classification", "entity_bag_unmatched.csv")
  if ((file.exists(out_map) || file.exists(out_un)) && !CLOBBER) {
    message("exists, skipping (set CLOBBER=TRUE to rebuild): ", out_map); return(invisible(NULL))
  }
  xw   <- fread(nip_product("00_ingest", "id_crosswalk.csv"), colClasses = "character")
  nd   <- fread(nip_product("01_entity_classification", "node_dictionary.csv"), colClasses = "character")
  gros <- load_gsa_roster(xw)
  cros <- load_consultant_roster()
  message(sprintf("rosters: %d GSA bags, %d consultant firms", nrow(gros), nrow(cros)))

  # curated overrides win over everything
  ov_path <- nip_input("entity_bag_overrides.csv")
  ov <- if (file.exists(ov_path)) fread(ov_path, colClasses = "character") else
    data.table(variant = character(), bag_id = character())

  IN_NET <- c("GSA", "Consultant", "Research", "NGO", "Institutional_other")
  nodes <- nd[entity_type %in% IN_NET, .(variant = name, entity_type)]
  message(sprintf("classifying %d in-network variants...", nrow(nodes)))

  map_rows <- vector("list", nrow(nodes))
  for (i in seq_len(nrow(nodes))) {
    v <- nodes$variant[i]; et <- nodes$entity_type[i]
    ovr <- ov[variant == v]
    if (nrow(ovr)) {
      map_rows[[i]] <- data.table(variant = v, entity_type = et, bag_id = ovr$bag_id[1],
                                  bag_label = ovr$bag_id[1], method = "override", score = NA_real_)
      next
    }
    if (et == "GSA") {
      m <- .match_gsa(v, gros)
      bid <- if (!is.na(m$id)) paste0("gsa_", m$id) else v
    } else if (et == "Consultant") {
      m <- .match_consultant(v, cros)
      bid <- if (!is.na(m$id)) m$id else v
    } else {
      m <- list(id = NA_character_, lab = NA_character_, m = "identity", s = NA_real_); bid <- v
    }
    folded <- !is.na(m$id)
    map_rows[[i]] <- data.table(
      variant = v, entity_type = et, bag_id = bid,
      bag_label = if (folded) m$lab else v,
      method = if (folded) m$m else "identity",
      score = m$s)
    sug_id <- m$sug %||% ""
    if (nzchar(sug_id) && et == "GSA") sug_id <- paste0("gsa_", sug_id)   # match map bag_id format
    attr(map_rows[[i]], "sugg") <- if (!folded && et %in% c("GSA","Consultant") && m$m != "identity")
      data.table(variant = v, entity_type = et, sugg_bag_id = sug_id,
                 sugg_label = m$lab %||% "", method = m$m, score = m$s, confirm_bag_id = "") else NULL
  }
  map <- rbindlist(map_rows)
  sugg <- rbindlist(lapply(map_rows, attr, "sugg"), fill = TRUE)

  fwrite(map, out_map)
  fwrite(sugg, out_un)

  # ---- report ----
  message(sprintf("wrote %s (%d rows)", out_map, nrow(map)))
  cat("\n== auto-fold vs identity, by type ==\n")
  rep <- map[, .(folded = sum(method %in% c("exact","subset","revsubset","override")),
                 identity = sum(method == "identity"), n = .N), by = entity_type]
  print(rep[order(entity_type)])
  cat("\n== GSA auto-fold methods ==\n")
  print(map[entity_type == "GSA" & method != "identity", .N, by = method][order(-N)])
  cat(sprintf("\nwrote %s (%d rows needing review)\n", out_un, nrow(sugg)))
  cat("== review file by suggestion method ==\n")
  if (nrow(sugg)) print(sugg[, .N, by = .(entity_type, method)][order(entity_type, -N)])
  invisible(list(map = map, unmatched = sugg))
}

if (sys.nframe() == 0) {
  message("== build_entity_bag_map.R (CLOBBER=", CLOBBER, ") ==")
  build_entity_bag_map()
  message("== entity_bag_map complete ==")
}
