#' build_gsa_bag_edges.R — Step 2 of the entity-bag redesign.
#'
#' Replaces build_gsa_edges.R / all_gsa_edges.csv. Produces, for each GSP
#' DOCUMENT, the co-mention ("tie") profile of its AUTHORING GSAs, where:
#'
#'   (1) EGO side folded to bags. Every GSA-typed vertex name is resolved to a
#'       canonical bag_id via inputs/entity_bag_overrides_gsa.csv (wins) then
#'       entity_bag_map.csv. Only bags that resolve to a roster id (gsa_<id>)
#'       can be authors; identity/unmatched bags (KGA, umbrella JPAs, §5
#'       no-roster clusters) can never author and are dropped.
#'
#'   (2) STRICT DWR AUTHORSHIP. A resolved ego is kept for a document only if its
#'       bag id is in THAT document's crosswalk `gsa_ids` (the ids; gsa_names is
#'       display-only/non-parallel). Membership = authorship, not mention.
#'
#'   (3) MULTI-MA PER-DOCUMENT EXPANSION. A mention string that folds to one
#'       member of a multi-management-area family (roster base name shared by
#'       >1 id, e.g. "Arroyo Seco GSA - 1"/"- 2") is only a POINTER into the
#'       family; it resolves per-document to whichever family member(s) are in
#'       that doc's gsa_ids. Same-doc co-authors are all kept.
#'
#'   (4) CORPUS-WIDE POOLED tie profile. A bag's alter co-mentions are pooled
#'       across every document it authors, then that pooled profile is attached
#'       to each plan it authors. So a plan is represented by its authoring
#'       GSAs' full corpus-wide relational signature, not only what one
#'       document's text happened to name.
#'
#'   (5) Keyed on gsp_doc_id + plan_family_id directly (NO raw-gsp_id
#'       translation) — this is what fixes the stale-enumeration namespace trap
#'       that dropped 26% of the old all_gsa_edges rows at the modeling join.
#'
#'   (6) GSA ALTERS FOLDED TO BAGS. A GSA-typed alter that resolves to a roster
#'       id is folded through the same resolver, with NO authorship filter (a GSA
#'       mentioned but not authoring still counts as an alter). Columns are
#'       "bag:gsa_<id>". A name pointing into a multi-MA family folds to the whole
#'       family, "bag:gsafam_<ids>", unless it carries a token unique to one
#'       member's MA suffix (e.g. "..._delta_mendota_..." -> County of Merced -
#'       Delta-Mendota), since no author set picks the member on the alter side.
#'       A multi-GSA name credits each of its bags. Weights of several names for
#'       the same bag sum. Alters in the ego's own bag (its other names, or its
#'       own family) are dropped as self-ties.
#'
#' Non-GSA alters, and GSA names the bag map holds unmatched, stay as raw
#' node_dictionary names (the modeling groups those by node name/type via
#' _entity_groups.R). Folding consultant and other alters is a later step.
#'
#' Reads: id_crosswalk.csv, node_dictionary.csv, entity_bag_map.csv,
#'        inputs/entity_bag_overrides_gsa.csv, core sgma_gsa_full.csv, and the
#'        core per-document weighted graphs.
#' Writes: data_products/01_entity_classification/gsa_bag_edges.csv
#'   columns: gsp_doc_id, plan_family_id, gsa (authoring bag label), then one
#'   column per alter (folded GSA bag key, else raw node name; pooled
#'   co-mention weight, 0-filled).
#'
#' Run from the repo root:
#'   Rscript Network_Innovation_Paper/Code/01_entity_classification/build_gsa_bag_edges.R
#' Set CLOBBER=TRUE to overwrite an existing gsa_bag_edges.csv.

suppressMessages({
  library(data.table); library(igraph)
})

.this_dir <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) NA)
if (is.na(.this_dir)) {
  a <- commandArgs(FALSE); m <- grep("^--file=", a, value = TRUE)
  .this_dir <- if (length(m)) dirname(normalizePath(sub("^--file=", "", m[1]))) else
    "Network_Innovation_Paper/Code/01_entity_classification"
}
source(file.path(.this_dir, "..", "_paths.R"))
source(file.path(.this_dir, "..", "_corpus.R"))   # load_id_crosswalk()

GSA_TYPE <- "GSA"
CLOBBER  <- toupper(Sys.getenv("CLOBBER", "FALSE")) %in% c("TRUE", "1", "YES")

# ---- resolver: node-variant -> bag_id(s) (override wins over the auto map) ----
# A variant naming several GSAs has one override row per GSA, so the resolver
# returns a list: variant -> character vector of bag_ids.
.build_resolver <- function() {
  ov <- fread(nip_input("entity_bag_overrides_gsa.csv"), colClasses = "character")
  mp <- fread(nip_product("01_entity_classification", "entity_bag_map.csv"),
              colClasses = "character")[entity_type == GSA_TYPE, .(variant, bag_id)]
  ov <- unique(ov[, .(variant, bag_id)])
  res <- rbind(ov, unique(mp[!variant %in% ov$variant]))
  split(res$bag_id, res$variant)
}

# ---- roster base-name families (multi-MA) ------------------------------------
# base = roster GSA_Name with any trailing " - <MA>" / " – <MA>" stripped.
# A family is the set of ids sharing a base name held by >1 id.
.build_families <- function() {
  rost <- fread(core_gsa_full(), colClasses = "character")
  rost[, base := sub("\\s+[-\u2013]\\s+.*$", "", GSA_Name)]
  fam  <- rost[, .(ids = list(unique(GSA_ID))), by = base]
  lk <- list()
  for (i in seq_len(nrow(fam))) {
    ids <- fam$ids[[i]]
    if (length(ids) > 1L) for (x in ids) lk[[x]] <- ids
  }
  lk  # id -> sibling id set; absent id => family is just {id}
}
.family_of <- function(x, fam) { f <- fam[[x]]; if (is.null(f)) x else f }

# ---- alter-side fold: GSA variant -> bag key(s) -----------------------------
# Returns data.table(variant, akey, ids): `akey` is the alter column name and
# `ids` is ",<id>,<id>," (the roster ids the key covers, for the self-tie test).
.build_alter_fold <- function(resolve, gtypes, fam) {
  rost <- fread(core_gsa_full(), colClasses = "character")
  .tok <- function(s) unique(strsplit(gsub("[^a-z0-9]+", " ", tolower(s)), " +")[[1]])
  base <- setNames(sub("\\s+[-\u2013]\\s+.*$", "", rost$GSA_Name), rost$GSA_ID)
  sfx  <- setNames(ifelse(grepl("\\s[-\u2013]\\s", rost$GSA_Name),
                          sub("^.*?\\s[-\u2013]\\s+", "", rost$GSA_Name, perl = TRUE), ""), rost$GSA_ID)
  # tokens unique to one member's MA suffix within its family
  uniq <- lapply(setNames(nm = names(fam)), function(x) {
    sib <- setdiff(fam[[x]], x)
    setdiff(.tok(sfx[x]), c("", .tok(base[x]), unlist(lapply(sfx[sib], .tok))))
  })
  vs <- intersect(names(resolve), names(gtypes)[gtypes == GSA_TYPE])
  rows <- lapply(vs, function(v) {
    b <- resolve[[v]]; b <- b[!is.na(b) & grepl("^gsa_[0-9]+$", b)]
    if (!length(b)) return(NULL)
    vt <- strsplit(v, "_")[[1]]
    rbindlist(lapply(sub("^gsa_", "", b), function(x) {
      if (is.null(fam[[x]]) || any(uniq[[x]] %in% vt))
        data.table(variant = v, akey = paste0("bag:gsa_", x), ids = paste0(",", x, ","))
      else { f <- sort(as.integer(fam[[x]]))
        data.table(variant = v, akey = paste0("bag:gsafam_", paste(f, collapse = "_")),
                   ids = paste0(",", paste(f, collapse = ","), ",")) }
    }))
  })
  unique(rbindlist(rows))
}

# ---- per-document authoring-ego alter contributions --------------------------
# Returns a long data.table (member, alter, weight) for ONE document, where
# `member` is the bare roster id actually authoring the doc that the ego folds to.
.contrib_one <- function(stem, author_set, gtypes, resolve, fam, afold) {
  f <- core_rds_for_stem(core_igraph_weighted(), stem)
  if (!file.exists(f)) return(NULL)
  g   <- readRDS(f)
  edf <- as.data.table(igraph::as_data_frame(g, what = "edges"))
  if (!nrow(edf) || !"weight" %in% names(edf)) return(NULL)

  vnames <- igraph::as_data_frame(g, what = "vertices")$name
  # ego candidates: GSA-typed vertices whose resolved bag is a roster id
  cand <- vnames[vnames %in% names(gtypes)[gtypes == GSA_TYPE]]
  if (!length(cand)) return(NULL)
  bag  <- lapply(resolve[cand], function(b) b[!is.na(b) & grepl("^gsa_[0-9]+$", b)])
  keep <- lengths(bag) > 0
  cand <- cand[keep]; bag <- bag[keep]
  if (!length(cand)) return(NULL)

  out <- vector("list", length(cand))
  for (k in seq_along(cand)) {
    v  <- cand[k]
    X  <- sub("^gsa_", "", bag[[k]])
    # per-doc multi-MA expand; a multi-GSA variant credits each authoring GSA once
    present <- intersect(unique(unlist(lapply(X, .family_of, fam))), author_set)
    if (!length(present)) next                             # mention, not author -> drop
    ie <- edf[from == v | to == v]
    if (!nrow(ie)) next
    alt <- ifelse(ie$from == v, ie$to, ie$from)
    aw  <- data.table(alter = alt, weight = ie$weight)[alter != v]
    # (6) fold GSA alters to bag keys; others keep their raw name
    aw  <- merge(aw, afold, by.x = "alter", by.y = "variant", all.x = TRUE,
                 allow.cartesian = TRUE)
    aw[is.na(akey), `:=`(akey = alter, ids = "")]
    if (!nrow(aw)) next
    # same-doc co-authors (both family members in the author set) -> keep all;
    # drop alters in the member's own bag (self-ties)
    out[[k]] <- rbindlist(lapply(present, function(m) {
      a <- aw[!grepl(paste0(",", m, ","), ids, fixed = TRUE),
              .(weight = sum(weight, na.rm = TRUE)), by = akey]
      if (nrow(a)) data.table(member = m, alter = a$akey, weight = a$weight)
    }))
  }
  rbindlist(Filter(Negate(is.null), out))
}

build_gsa_bag_edges <- function() {
  out <- nip_product("01_entity_classification", "gsa_bag_edges.csv")
  if (file.exists(out) && !CLOBBER) {
    message("exists, skipping (set CLOBBER=TRUE to rebuild): ", out); return(invisible(NULL))
  }

  # Raw crosswalk carries the per-document gsa_ids (load_id_crosswalk drops them).
  cw <- fread(nip_product("00_ingest", "id_crosswalk.csv"), colClasses = "character")
  xw <- unique(cw[, .(gsp_doc_id, plan_family_id, gsa_ids)])
  # author set per doc (bare numeric ids)
  xw[, aset := lapply(strsplit(gsa_ids, ","), function(z) trimws(z[z != ""]))]

  dict   <- fread(nip_product("01_entity_classification", "node_dictionary.csv"),
                  colClasses = "character")
  gtypes <- setNames(dict$entity_type, dict$name)
  resolve <- .build_resolver()
  fam     <- .build_families()
  afold   <- .build_alter_fold(resolve, gtypes, fam)
  message("GSA alter fold: ", uniqueN(afold$variant), " names -> ",
          uniqueN(afold$akey), " bag keys (", sum(grepl("gsafam_", unique(afold$akey))),
          " families)")

  message("building gsa_bag_edges over ", nrow(xw), " documents...")
  parts <- lapply(seq_len(nrow(xw)), function(i)
    .contrib_one(xw$gsp_doc_id[i], xw$aset[[i]], gtypes, resolve, fam, afold))
  parts <- Filter(function(d) !is.null(d) && nrow(d), parts)
  long  <- rbindlist(parts)
  if (!nrow(long)) stop("no authoring-ego contributions produced; check inputs.")

  # (4) corpus-wide pool: member x alter summed across every authored document
  pooled <- long[, .(weight = sum(weight, na.rm = TRUE)), by = .(member, alter)]

  # attach each member's pooled profile to every doc it authors
  doc_member <- xw[, .(member = unlist(aset)), by = .(gsp_doc_id, plan_family_id)]
  emit <- merge(doc_member, pooled, by = "member", allow.cartesian = TRUE)
  if (!nrow(emit)) stop("no (doc x authoring bag) rows after join.")

  # label the ego by its canonical roster name
  rost <- fread(core_gsa_full(), colClasses = "character")
  lbl  <- setNames(rost$GSA_Name, rost$GSA_ID)
  emit[, gsa := paste0("gsa_", member)]
  emit[, gsa_label := fifelse(member %in% names(lbl), lbl[member], gsa)]

  wide <- dcast(emit, gsp_doc_id + plan_family_id + gsa + gsa_label ~ alter,
                value.var = "weight", fun.aggregate = sum, fill = 0)
  fwrite(wide, out)
  message("wrote ", out, " (", nrow(wide), " rows x ", ncol(wide), " cols; ",
          uniqueN(wide$gsp_doc_id), " docs, ", uniqueN(wide$plan_family_id),
          " plans, ", uniqueN(wide$gsa), " authoring bags)")
  invisible(wide)
}

if (sys.nframe() == 0) {
  message("== build_gsa_bag_edges.R (CLOBBER=", CLOBBER, ") ==")
  build_gsa_bag_edges()
  message("== gsa_bag_edges complete ==")
}
