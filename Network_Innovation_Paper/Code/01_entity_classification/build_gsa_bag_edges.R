#' build_gsa_bag_edges.R — Step 2 of the entity-bag redesign.
#'
#' Replaced build_gsa_edges.R / all_gsa_edges.csv (retired 2026-11-16). Produces,
#' for each GSP DOCUMENT, the co-mention ("tie") profile of its AUTHORING GSAs,
#' where:
#'
#'   (1) EGO side folded to bags. Every GSA name is resolved to a canonical
#'       bag_id via inputs/entity_bag_overrides_gsa.csv (wins) then
#'       entity_bag_map.csv. Only bags that resolve to a roster id (gsa_<id>)
#'       can be authors; identity/unmatched bags (KGA, umbrella JPAs, §5
#'       no-roster clusters) can never author and are dropped.
#'       GSA-ness is decided by the resolver, not the LLM type: a variant in the
#'       override file is a GSA whatever node_dictionary says (24 override names
#'       are typed Institutional_other, e.g. `pixid` -> gsa_43 and the Eastern
#'       Tule board/committee names -> gsa_205; 4 Non_institutional, e.g.
#'       `greaterkaweahgsaorg` -> gsa_281). Before 2026-11-16 those were gated
#'       out on type on both the ego and the alter side.
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
#'       plan_family_id is written 4-digit zero-padded, the form
#'       modeling_plan_selection(), gsp_covariates.csv and the shapefile key on.
#'
#'   (6) GSA ALTERS FOLDED TO BAGS. A GSA alter that resolves to a roster id is
#'       folded through the same resolver, with NO authorship filter (a GSA
#'       mentioned but not authoring still counts as an alter). Columns are
#'       "bag:gsa_<id>". A name pointing into a multi-MA family folds to the whole
#'       family, "bag:gsafam_<ids>", unless it carries a token unique to one
#'       member's MA suffix (e.g. "..._delta_mendota_..." -> County of Merced -
#'       Delta-Mendota), since no author set picks the member on the alter side.
#'       A GSA name whose bag is NOT a roster id (a bare identity bag, the KGA
#'       umbrella) folds to "bag:<bag_id>" with no family logic. A multi-GSA
#'       name credits each of its bags. Weights of several names for the same
#'       bag sum. Alters in the ego's own bag (its other names, or its own
#'       family) are dropped as self-ties.
#'
#'   (7) NON-GSA ALTERS FOLDED TO BAGS (added 2026-11-16). Every other alter
#'       that entity_bag_lookup.csv covers (Consultant con_*, NGO ngo_*,
#'       Research res_*, Institutional_other io_*) folds to "bag:<bag_id>".
#'       A multi-organization mention listed in alter_bag_compounds.csv credits
#'       its own bag AND every other bag it names ("woodard_curran_and_davids_
#'       engineering" -> con_woodard_curran and con_davids_engineering): a 1:1
#'       fold would delete the second firm's tie, so this adds ties only.
#'       Alters outside every namespace (Non_institutional,
#'       Institutional_unresolved) keep their raw node_dictionary name; no
#'       grouping in _entity_groups.R admits them anyway.
#'
#' Reads: id_crosswalk.csv, node_dictionary.csv, entity_bag_map.csv,
#'        entity_bag_lookup.csv, alter_bag_compounds.csv,
#'        inputs/entity_bag_overrides_gsa.csv, core sgma_gsa_full.csv, and the
#'        core per-document weighted graphs.
#' Writes: data_products/01_entity_classification/gsa_bag_edges.csv
#'   columns: gsp_doc_id, plan_family_id (4-digit), gsa (authoring bag id),
#'   gsa_label, then one column per alter key (pooled co-mention weight,
#'   0-filled).
#'         data_products/01_entity_classification/gsa_bag_edges_alters.csv
#'   one row per alter column: name (the column key), entity_type (the type that
#'   OWNS the bag; for a raw name, its node_dictionary type), bag_type, bag_id,
#'   bag_label, n_variants (names folded into the column, corpus-wide), folded.
#'   This is the dictionary _entity_groups.R::entity_names() groups columns by.
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

CLOBBER  <- toupper(Sys.getenv("CLOBBER", "FALSE")) %in% c("TRUE", "1", "YES")
# alter_bag_compounds.csv: a multi-organization mention credits every bag it names
CREDIT_COMPOUNDS <- TRUE
ROSTER_RE <- "^gsa_[0-9]+$"
# bag namespace prefix -> the entity type that owns it (mirrors _entity_groups.R)
TYPE_OF_BAG <- c(gsa = "GSA", con = "Consultant", ngo = "NGO",
                 res = "Research", io = "Institutional_other")

# ---- resolver: GSA name-variant -> bag_id(s) (override wins over the auto map) --
# A variant naming several GSAs has one override row per GSA, so the resolver
# returns a list: variant -> character vector of bag_ids. Its names() are the
# set of GSA names: every override variant plus the map's GSA-typed rows.
.build_resolver <- function() {
  ov <- fread(nip_input("entity_bag_overrides_gsa.csv"), colClasses = "character")
  mp <- fread(nip_product("01_entity_classification", "entity_bag_map.csv"),
              colClasses = "character")[entity_type == "GSA", .(variant, bag_id)]
  ov <- unique(ov[, .(variant, bag_id)])
  res <- rbind(ov, unique(mp[!variant %in% ov$variant]))
  split(res$bag_id, res$variant)
}

# ---- roster base-name families (multi-MA) ------------------------------------
# base = roster GSA_Name with any trailing " - <MA>" / " – <MA>" stripped.
# A family is the set of ids sharing a base name held by >1 id. The base is
# compared in normal form ("County of X GSA" == "X County GSA") so the roster's
# inconsistent naming does not split a family (gsa_295 "County of Sacramento
# GSA - South American" is a sibling of 496/497 "Sacramento County GSA - ...").
.build_families <- function() {
  rost <- fread(core_gsa_full(), colClasses = "character")
  rost[, base := tolower(sub("\\s+[-\u2013]\\s+.*$", "", GSA_Name))]
  rost[, base := sub("^county of (.+) gsa$", "\\1 county gsa", base)]
  fam  <- rost[, .(ids = list(unique(GSA_ID))), by = base]
  lk <- list()
  for (i in seq_len(nrow(fam))) {
    ids <- fam$ids[[i]]
    if (length(ids) > 1L) for (x in ids) lk[[x]] <- ids
  }
  lk  # id -> sibling id set; absent id => family is just {id}
}
.family_of <- function(x, fam) { f <- fam[[x]]; if (is.null(f)) x else f }

# ---- alter-side fold, GSA names: variant -> bag key(s) -------------------------
# Returns data.table(variant, akey, ids): `akey` is the alter column name and
# `ids` is ",<id>,<id>," (the roster ids the key covers, for the self-tie test;
# "" for a non-roster bag).
.build_gsa_alter_fold <- function(resolve, fam) {
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
  rows <- lapply(names(resolve), function(v) {
    b <- resolve[[v]]; b <- b[!is.na(b) & nzchar(b)]
    if (!length(b)) return(NULL)
    vt <- strsplit(v, "_")[[1]]
    rbindlist(lapply(b, function(bid) {
      if (!grepl(ROSTER_RE, bid))        # identity / umbrella bag: no family, no roster ids
        return(data.table(variant = v, akey = paste0("bag:", bid), ids = ""))
      x <- sub("^gsa_", "", bid)
      if (is.null(fam[[x]]) || any(uniq[[x]] %in% vt))
        data.table(variant = v, akey = paste0("bag:gsa_", x), ids = paste0(",", x, ","))
      else { f <- sort(as.integer(fam[[x]]))
        data.table(variant = v, akey = paste0("bag:gsafam_", paste(f, collapse = "_")),
                   ids = paste0(",", paste(f, collapse = ","), ",")) }
    }))
  })
  unique(rbindlist(rows))
}

# ---- alter-side fold, every other namespace: variant -> "bag:<bag_id>" ---------
# From entity_bag_lookup.csv (con_/ngo_/res_/io_ rows; the gsa rows are the
# resolver's and are handled above). A compound mention credits its extra bags.
.build_other_alter_fold <- function(resolve) {
  lk <- fread(nip_product("01_entity_classification", "entity_bag_lookup.csv"),
              colClasses = "character")[bag_type != "gsa" & !variant %in% names(resolve),
                                        .(variant, bag_id)]
  rows <- lk[, .(variant, akey = paste0("bag:", bag_id), ids = "")]
  if (CREDIT_COMPOUNDS) {
    cpf <- nip_product("01_entity_classification", "alter_bag_compounds.csv")
    if (file.exists(cpf)) {
      cp <- fread(cpf, colClasses = "character")[variant %in% lk$variant & nzchar(also_names)]
      if (nrow(cp)) {
        extra <- cp[, .(akey = paste0("bag:", trimws(strsplit(also_names, "|", fixed = TRUE)[[1]]))),
                    by = variant][akey != "bag:"]
        extra[, ids := ""]
        rows <- rbind(rows, extra)
        message("compound mentions: ", uniqueN(cp$variant), " names credit ",
                nrow(extra), " extra bag ties")
      }
    }
  }
  unique(rows)
}

# ---- per-document authoring-ego alter contributions --------------------------
# Returns a long data.table (member, alter, weight) for ONE document, where
# `member` is the bare roster id actually authoring the doc that the ego folds to.
.contrib_one <- function(stem, author_set, resolve, fam, afold) {
  f <- core_rds_for_stem(core_igraph_weighted(), stem)
  if (!file.exists(f)) return(NULL)
  g   <- readRDS(f)
  edf <- as.data.table(igraph::as_data_frame(g, what = "edges"))
  if (!nrow(edf) || !"weight" %in% names(edf)) return(NULL)

  vnames <- igraph::as_data_frame(g, what = "vertices")$name
  # ego candidates: GSA names (per the resolver) whose bag is a roster id
  cand <- vnames[vnames %in% names(resolve)]
  if (!length(cand)) return(NULL)
  bag  <- lapply(resolve[cand], function(b) b[!is.na(b) & grepl(ROSTER_RE, b)])
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
    # (6)/(7) fold alters to bag keys; names outside every namespace keep their raw name
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

# ---- the alter-column dictionary ----------------------------------------------
# One row per alter column of the wide file, typed by the namespace that OWNS the
# bag (never by the LLM type of any one spelling). Raw columns carry their
# node_dictionary type and no bag.
.alter_dictionary <- function(keys, afold, gtypes) {
  lk   <- fread(nip_product("01_entity_classification", "entity_bag_lookup.csv"),
                colClasses = "character")
  rost <- fread(core_gsa_full(), colClasses = "character")
  lbl  <- setNames(rost$GSA_Name, rost$GSA_ID)
  base <- setNames(sub("\\s+[-\u2013]\\s+.*$", "", rost$GSA_Name), rost$GSA_ID)
  blab <- lk[, .(bag_type = bag_type[1],
                 bag_label = { l <- bag_label[bag_label != bag_id]; if (length(l)) l[1] else bag_id[1] }),
             by = bag_id]
  nvar <- afold[, .(n_variants = uniqueN(variant)), by = akey]

  d <- data.table(name = keys)
  d[, folded := startsWith(name, "bag:")]
  d[, bag_id := fifelse(folded, sub("^bag:", "", name), NA_character_)]
  is_f <- d$folded
  d[, `:=`(bag_type = NA_character_, bag_label = NA_character_)]
  fam_i <- is_f & grepl("^gsafam_", d$bag_id)
  d[fam_i, `:=`(bag_type = "gsa",
                bag_label = vapply(bag_id, function(b)
                  unname(base[strsplit(sub("^gsafam_", "", b), "_")[[1]][1]]), character(1)))]
  ros_i <- is_f & grepl(ROSTER_RE, d$bag_id)
  d[ros_i, `:=`(bag_type = "gsa", bag_label = unname(lbl[sub("^gsa_", "", bag_id)]))]
  oth_i <- is_f & !fam_i & !ros_i
  h <- match(d$bag_id[oth_i], blab$bag_id)
  d[oth_i, `:=`(bag_type = blab$bag_type[h], bag_label = blab$bag_label[h])]
  d[, entity_type := fifelse(folded, unname(TYPE_OF_BAG[bag_type]), unname(gtypes[name]))]
  d[folded == FALSE, bag_label := name]
  d <- merge(d, nvar, by.x = "name", by.y = "akey", all.x = TRUE, sort = FALSE)
  d[is.na(n_variants), n_variants := 1L]
  bad <- d[folded == TRUE & (is.na(bag_type) | is.na(entity_type))]
  if (nrow(bad)) stop("folded alter column(s) with no owning namespace: ",
                      paste(head(bad$name, 5), collapse = ", "))
  d[match(keys, name), .(name, entity_type, bag_type, bag_id, bag_label, n_variants, folded)]
}

build_gsa_bag_edges <- function() {
  out     <- nip_product("01_entity_classification", "gsa_bag_edges.csv")
  out_alt <- nip_product("01_entity_classification", "gsa_bag_edges_alters.csv")
  if (file.exists(out) && file.exists(out_alt) && !CLOBBER) {
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
  gfold   <- .build_gsa_alter_fold(resolve, fam)
  ofold   <- .build_other_alter_fold(resolve)
  afold   <- unique(rbind(gfold, ofold))
  message("GSA alter fold: ", uniqueN(gfold$variant), " names -> ",
          uniqueN(gfold$akey), " bag keys (", sum(grepl("gsafam_", unique(gfold$akey))),
          " families, ", sum(!grepl("^bag:gsa", unique(gfold$akey))), " non-roster)")
  message("other alter fold: ", uniqueN(ofold$variant), " names -> ",
          uniqueN(ofold$akey), " bag keys")
  n_ov_untyped <- sum(gtypes[names(resolve)] != "GSA", na.rm = TRUE)
  message("GSA names by resolver: ", length(resolve), " (", n_ov_untyped,
          " override names the classifier typed otherwise)")

  message("building gsa_bag_edges over ", nrow(xw), " documents...")
  parts <- lapply(seq_len(nrow(xw)), function(i)
    .contrib_one(xw$gsp_doc_id[i], xw$aset[[i]], resolve, fam, afold))
  parts <- Filter(function(d) !is.null(d) && nrow(d), parts)
  long  <- rbindlist(parts)
  if (!nrow(long)) stop("no authoring-ego contributions produced; check inputs.")

  # (4) corpus-wide pool: member x alter summed across every authored document
  pooled <- long[, .(weight = sum(weight, na.rm = TRUE)), by = .(member, alter)]

  # attach each member's pooled profile to every doc it authors
  doc_member <- xw[, .(member = unlist(aset)), by = .(gsp_doc_id, plan_family_id)]
  emit <- merge(doc_member, pooled, by = "member", allow.cartesian = TRUE)
  if (!nrow(emit)) stop("no (doc x authoring bag) rows after join.")

  # (5) plan_family_id in the 4-digit form every plan-level join uses
  emit[, plan_family_id := formatC(as.integer(plan_family_id), width = 4L, flag = "0")]

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

  keys <- setdiff(names(wide), c("gsp_doc_id", "plan_family_id", "gsa", "gsa_label"))
  alters <- .alter_dictionary(keys, afold, gtypes)
  fwrite(alters, out_alt)
  message("wrote ", out_alt, " (", nrow(alters), " alter columns: ",
          sum(alters$folded), " bag keys, ", sum(!alters$folded), " raw names)")
  cat("\n== alter columns by owning type ==\n")
  print(alters[, .(columns = .N, folded = sum(folded)), by = entity_type][order(-columns)])
  invisible(wide)
}

if (sys.nframe() == 0) {
  message("== build_gsa_bag_edges.R (CLOBBER=", CLOBBER, ") ==")
  build_gsa_bag_edges()
  message("== gsa_bag_edges complete ==")
}
