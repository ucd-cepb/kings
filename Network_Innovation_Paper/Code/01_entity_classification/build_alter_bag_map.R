#' build_alter_bag_map.R — fold the ALTER side of the co-mention network onto bags.
#'
#' Companion to build_entity_bag_map.R, which owns the GSA (ego) side. That process
#' stays SEPARATE and untouched: the GSA fold is the load-bearing one, it is signed
#' off (Gate 1), and nothing here can move a GSA bag.
#'
#' DESIGN: one INDEPENDENT process per entity type, each with its OWN bag namespace,
#' so focal subnetworks can never cross-merge:
#'
#'   Consultant          -> con_*   authority = core_code/dicts/consultant_dictionary.csv
#'                                  (exact firm match auto-folds), then self-clustering
#'                                  over the residual that the dictionary does not know.
#'   NGO                 -> ngo_*   no roster exists -> SELF-CLUSTER within type.
#'                                  ("NGO" is the classifier leaf covering nonprofits,
#'                                   advocacy groups and interest groups.)
#'   Research            -> res_*   no roster exists -> SELF-CLUSTER within type.
#'                                  (universities + research organizations)
#'   Institutional_other -> io_*    DEFERRED by decision (10,099 variants): identity
#'                                  passthrough only, no clustering this pass.
#'
#' Every variant gets a namespaced bag_id even when it folds to nobody (an unfolded
#' variant becomes its own bag, e.g. ngo_<variant>). That makes the namespaces
#' disjoint by construction. NB: the reverse is NOT true for the GSA namespace, whose
#' identity bags are un-prefixed — never re-derive a type from a bag_id prefix;
#' always carry bag_type.
#'
#' SELF-CLUSTERING uses the same token/fuzzy machinery as the GSA matcher — no model
#' in the loop — and is HIGH-PRECISION-auto / everything-else-to-review:
#'
#'   collapse    identical with tokenization and citation digits removed — a
#'               hyphenation/OCR split or a footnote number, not a different org
#'               ("uni_versity_of_california_davis", "oregon_state_university1")
#'                                                           -> auto
#'   tokset      distinctive token SETS are equal AND the orders are a contiguous
#'               block ROTATION ("land_trust_of_napa_county" ~ "napa_county_land_trust")
#'                                                           -> auto
#'   contains    one variant's distinctive tokens are a CONTIGUOUS RUN inside
#'               another's, the subset is substantive, AND every extra token is a
#'               generic org-form word or GSP document boilerplate
#'                                                           -> auto (elaborated name)
#'   acronym     a bare initialism whose letters are the initials of exactly ONE
#'               other bag in the same type ("lsce" -> LSCE, "wspa" -> WSPA)
#'                                                           -> auto
#'   reorder     token sets equal but the order is NOT a rotation — the signal that
#'               separates two real organizations ("california_polytechnic_state_
#'               university" = Cal Poly SLO vs "california_state_polytechnic_
#'               university" = Cal Poly Pomona)              -> REVIEW
#'   fuzzy       best within-type adist similarity >= FUZZY_SHOW
#'                                                           -> REVIEW (suggestion)
#'   singleton   nothing else is close                       -> own bag
#'
#' Auto pairs are edges of an undirected graph; each connected component is one bag.
#' The canonical member (bag label) is the cleanest spelling in the component: least
#' document debris, then most real tokens, then fewest abbreviations, then shortest
#' string, then alphabetical.
#'
#' `method` names the tier of the edge joining the variant DIRECTLY to its bag's
#' representative; a variant pulled in only through a chain reports method
#' "transitive". `joined` says which: anchor / direct / transitive / singleton.
#'
#' Curated per-type override files win over everything, and are where confirmed
#' review decisions get promoted:
#'   inputs/entity_bag_overrides_consultant.csv
#'   inputs/entity_bag_overrides_ngo.csv
#'   inputs/entity_bag_overrides_research.csv
#'   inputs/entity_bag_overrides_institutional_other.csv
#' (variant,bag_id[,...] — same 7-column schema as the GSA override file; only
#'  variant and bag_id are read here.)
#'
#' Reads : node_dictionary.csv, consultant_dictionary.csv, per-type overrides.
#' Writes: data_products/01_entity_classification/alter_bag_map.csv       (all types)
#'         data_products/01_entity_classification/alter_bag_clusters.csv  (audit: folded bags)
#'         data_products/01_entity_classification/alter_bag_review_<type>.csv (review menus)
#'         data_products/01_entity_classification/alter_bag_compounds.csv (multi-org mentions)
#'
#' Run from the repo root:
#'   Rscript Network_Innovation_Paper/Code/01_entity_classification/build_alter_bag_map.R
#' Set CLOBBER=TRUE to overwrite existing outputs.

suppressMessages({ library(data.table); library(igraph) })

.this_dir <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) NA)
if (is.na(.this_dir)) {
  a <- commandArgs(FALSE); m <- grep("^--file=", a, value = TRUE)
  .this_dir <- if (length(m)) dirname(normalizePath(sub("^--file=", "", m[1]))) else
    "Network_Innovation_Paper/Code/01_entity_classification"
}
source(file.path(.this_dir, "..", "_paths.R"))

CLOBBER        <- toupper(Sys.getenv("CLOBBER", "FALSE")) %in% c("TRUE", "1", "YES")
FUZZY_SHOW     <- 0.75   # review-suggestion floor (same as the GSA matcher)
MIN_SUB_TOKENS <- 2L     # a 1-token name never auto-folds into a longer one
ACRO_MIN       <- 2L     # shortest initialism considered
ACRO_MAX       <- 7L     # longest initialism considered
MAGNET_MIN     <- 4L     # a bag that is the subset in >= this many review pairs is a
                         # bare institutional form, not a parent (26 county Farm Bureaus)

# ---- namespace / authority table (one row per independent process) -----------
TYPES <- data.table(
  entity_type = c("Consultant", "NGO", "Research", "Institutional_other"),
  prefix      = c("con",        "ngo", "res",      "io"),
  override    = c("entity_bag_overrides_consultant.csv", "entity_bag_overrides_ngo.csv",
                  "entity_bag_overrides_research.csv",   "entity_bag_overrides_institutional_other.csv"),
  roster      = c("consultant_dictionary",               NA, NA, NA),
  self_fold   = c(TRUE, TRUE, TRUE, FALSE)   # Institutional_other DEFERRED -> identity only
)

# ---- normalization (identical convention to build_entity_bag_map.R) ----------
.norm <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("[^a-z0-9]+", "_", x)
  gsub("^_+|_+$", "", x)
}
.toks <- function(x) { t <- strsplit(x, "_", fixed = TRUE)[[1]]; t[nzchar(t)] }

# STOP: words that can never distinguish two organizations — articles, prepositions,
# conjunctions and legal-form suffixes. Deliberately tiny; everything substantive is
# KEPT.
.STOP <- c("the", "of", "a", "an", "and", "for", "to", "in", "at", "on",
           "inc", "incorporated", "llc", "llp", "lp", "ltd", "co", "corp",
           "corporation", "pc", "plc",
           "et", "al")   # citation boilerplate: "..._et_al"

# EXTRACTION ARTIFACTS stripped before any token comparison. These are emitted by
# the upstream text pipeline and are never part of an organization's name:
#   "_x"                     placeholder suffix (80 variants corpus-wide)
#   "ngo016", "mcr_2", "a12" DWR comment-response-matrix commenter/row ids. The
#                            giveaways are the siblings "mcr_2_ngo020_a_gl_..." and
#                            "ngo021_minimum_thresholds" (MCR = Multiple Comment
#                            Response). See BAG_REVIEW_FINDINGS.md §9.
# NB: stripping these here keeps the folder honest, but the real fix is upstream —
# these strings should never have been extracted as entity names.
.depunct <- function(x) {
  y <- sub("_x$", "", x)
  y <- gsub("(^|_)mcr(_[0-9]+)*(_|$)", "\\1", y)
  y <- gsub("(^|_)ngo[0-9]{3}[a-z]?(_|$)", "\\1", y)
  y <- gsub("^_+|_+$", "", y)
  ifelse(nzchar(y), y, x)
}

# Citation digits: a PURE-digit token is a page/footnote/year number, and a digit run
# trailing a real word is a footnote marker ("oregon_state_university1"). A LEADING
# digit is usually part of the brand, so it is kept — stripping it per-token produced
# "con_ndnature" (2NDNATURE), "con_creeks" (4Creeks), "ngo_h" (4-H) and split CH2M
# Hill across five bags. A 1-3 digit code behind one or two letters is a
# comment-matrix row id ("a12", "ct006").
.strip_cit <- function(t) {
  if (!length(t)) return(t)
  t <- t[!grepl("^[0-9]+$", t)]                 # page / footnote / year numbers
  t <- t[!grepl("^[a-z]{1,2}[0-9]{2,3}$", t)]   # comment-matrix row ids
  if (!length(t)) return(t)
  s <- sub("[0-9]+$", "", t)
  ifelse(nchar(s) >= 4L, s, t)                  # keep "ch2","r2"; fix "university1"
}

.dtoks <- function(x) {
  t <- .strip_cit(.toks(.depunct(x)))
  setdiff(t[nzchar(t)], .STOP)
}
# "flat" form: letters only. Two names with the same flat form differ purely in
# tokenization/hyphenation ("uni_versity_of_california_davis", "labo_ratory",
# "califor_nia") or in a citation digit — the same name, split differently.
.flat <- function(x) gsub("[^a-z]", "", tolower(.depunct(x)))
# a name with its citation digits and extraction artifacts removed, for use as a
# bag's own name. Reverts to the original if cleaning would leave nothing usable.
.clean_name <- function(x) vapply(x, function(s) {
  t <- .strip_cit(.toks(.depunct(s))); t <- t[nzchar(t)]
  if (!length(t) || sum(nchar(t)) < 2L) s else paste(t, collapse = "_")
}, character(1), USE.NAMES = FALSE)

# Light stemming for the EQUALITY tiers only — never applied to a label. Folds
# plurals, the handful of abbreviations that are unambiguous in this corpus, and
# the "counsel"/"council" misspelling. The last is measurably free: all 12 NGO bags
# containing "counsel" are Leadership Counsel for Justice & Accountability, the 21
# legitimate "council" organizations are untouched, and it creates exactly 2
# collision groups, both correct (BAG_REVIEW_FINDINGS.md §7d).
.ABBREV <- c(counsel = "council", natl = "national", socy = "society",
             assn = "association", univ = "university", dept = "department",
             lab = "laboratory", labs = "laboratory")
.stem1 <- function(t) {
  t <- ifelse(t %in% names(.ABBREV), .ABBREV[t], t)
  t <- ifelse(nchar(t) > 4L & grepl("ies$", t), sub("ies$", "y", t), t)   # laboratories
  ifelse(nchar(t) > 3L & grepl("s$", t) & !grepl("ss$", t), sub("s$", "", t), t)
}
# radix sort is locale-independent, but it refuses non-UTF-8 input and two bag names
# carry accents (res_ecole_polytechnique_federale..., ngo_lideres_campesinas) — fall
# back to the default collation for those rather than failing the build
.rsort <- function(x) tryCatch(sort(x, method = "radix"), error = function(e) sort(x))
.dseq  <- function(x) .stem1(.dtoks(x))             # stemmed, IN ORDER
.dstem <- function(x) .rsort(unique(.dseq(x)))      # stemmed, as a SET

# GENERIC: org-form words that are allowed to be the ONLY difference between an
# abbreviated and an elaborated spelling of the same org (the `contains` tier).
# Used ONLY as that filter — these words still count as distinctive tokens.
#
# TWO CLASSES OF WORD ARE DELIBERATELY ABSENT, both because they distinguish real
# organizations rather than describing one:
#   (1) GEOGRAPHIC/JURISDICTIONAL ("state", "national", "international"). "states"
#       stems to "state", which merged CSU into UC, Oregon State into U of Oregon,
#       Washington State into Washington University; "national" merged a bare
#       "audubon" into "national_audubon_society".
#   (2) ENTITY-NAMING forms ("foundation", "fund", "institute", "center", "program",
#       "council", "division", "extension", "laboratory"). Unlike "inc"/"llc" these
#       name separate legal entities: Planning & Conservation League vs its
#       Foundation, Clean Water Action vs the Clean Water Fund, the Irrigation
#       Training & Research CENTER vs INSTITUTE vs PROGRAM, and the UC system vs UC
#       Extension / UC Division of Ag & Natural Resources.
.GENERIC_COMMON <- c("group", "company", "companies", "organization", "organisation",
                     "org", "nonprofit", "non", "profit")
.GENERIC <- list(
  Consultant = c(.GENERIC_COMMON, "consulting", "consultants", "consultant", "engineering",
                 "engineers", "engineer", "associates", "associate", "partners", "partnership",
                 "services", "service", "solutions", "systems", "sciences", "science",
                 "technologies", "technology", "firm", "advisors", "advisory"),
  NGO        = c(.GENERIC_COMMON, "association", "associations"),
  Research   = c(.GENERIC_COMMON, "university", "universities", "college", "colleges",
                 "school", "schools", "institution", "academy", "academic", "faculty")
)

# WEAK: domain vocabulary so common in this corpus that a name built only out of
# these words cannot identify an organization. Used as a guard on the roster
# `subset` tier: a firm whose every token is weak/generic must never claim a
# sentence fragment that happens to contain those words.
.WEAK <- c("water", "waters", "groundwater", "resource", "resources", "basin",
           "basins", "county", "counties", "city", "district", "districts",
           "plan", "plans", "management", "environmental", "environment",
           "engineering", "engineers", "consulting", "consultants", "sustainability",
           "sustainable", "agency", "california", "state", "regional", "region",
           "valley", "central", "north", "south", "east", "west", "general")

# DEBRIS: GSP document boilerplate. These words surround an organization's name
# because the mention was extracted from a plan's prose, a section heading, a figure
# caption or a comment-response table — they are never part of the name. Allowed as
# `contains` extras, which is what collapses the 55 Nature Conservancy bags
# ("section_7_nature_conservancy_checklist", "umbrella_groundwater_sustainability_
# plan_nature_conservancy") onto one actor.
#
# "sustainable" is deliberately NOT here: as a standalone word it is part of real
# names, and allowing it folded "central_coast_alliance_united_for_a_sustainable_
# economy" into "..._for_economy", dropping CAUSE's actual name.
.DEBRIS <- c("section", "sections", "appendix", "appendices", "chapter", "chapters",
             "table", "tables", "figure", "figures", "exhibit", "exhibits",
             "comment", "comments", "response", "responses", "note", "notes", "noted",
             "checklist", "guidance", "guideline", "guidelines", "dataset", "data",
             "viewer", "draft", "final", "umbrella", "goal", "goals",
             "objective", "objectives", "threshold", "thresholds", "measurable",
             "minimum", "condition", "conditions", "best", "practice", "practices",
             "using", "code", "codes", "wqo", "implementation", "related",
             "memorandum", "memo", "report", "reports", "letter", "letters",
             "page", "pages", "attachment", "attachments", "submittal", "revised",
             "plan", "plans", "groundwater", "sustainability", "management",
             "multiple", "act", "conceptual", "pulse", "common", "diverse",
             "distribution", "surface", "area", "areas", "this", "these", "all")

# ---- rosters -----------------------------------------------------------------
load_consultant_roster <- function() {
  d <- fread(core_dict("consultant_dictionary.csv"), colClasses = "character")
  d <- d[!is.na(all_names) & nzchar(all_names)]
  # NB: `key` is a reserved data.table() argument — build then rename.
  ros <- data.table(label = d$all_names, nkey = .norm(d$all_names))
  setnames(ros, "nkey", "key")
  unique(ros[nzchar(key)], by = "key")
}

# ---- roster `subset` tier (the GSA matcher's "elaborated variant" rule) -------
# A firm name appearing INTACT inside a variant is a mention of that firm, however
# much sentence debris surrounds it:
#   "aqueduct_luhdorff_scalmanini_consulting_engineers_81"            -> LSCE
#   "davids_engineering_k_hydraulic_conductivity_luhdorff_and_scalmanini_..." -> LSCE
# "Intact" means the firm's distinctive tokens form a CONTIGUOUS RUN, in order, in
# the variant's distinctive-token sequence. A bare subset test is not enough: it
# folds "applied_development_economics" into the unrelated firm "Applied Economics"
# because an interleaved token is invisible to it. Debris before/after is fine, and
# stopwords inside are too ("brown_and_caldwell_report" -> Brown and Caldwell).
#
# Both sides are STEMMED. The roster says "GEI Consultant, Inc." while every corpus
# mention says "GEI Consultants", so an unstemmed comparison gave GEI no anchor at
# all and shattered it across 25 bags (BAG_REVIEW_FINDINGS.md §7d).
#
# Auto only when the longest matching firm name is unique, only for firm names
# carrying a token that is neither generic org-form nor weak domain vocabulary, and
# only when the surrounding debris does NOT itself name a second roster firm —
# "woodard_curran_and_davids_engineering" names two firms, and folding it onto one
# silently deletes the other's tie (BAG_REVIEW_FINDINGS.md §9).
.run_at <- function(needle, hay) {
  n <- length(needle); h <- length(hay)
  if (!n || n > h) return(integer(0))
  hit <- integer(0)
  for (s in seq_len(h - n + 1L))
    if (identical(hay[s:(s + n - 1L)], needle)) hit <- c(hit, s)
  hit
}
.run_in <- function(needle, hay) length(.run_at(needle, hay)) > 0L

.roster_subset <- function(vars, ros, generic) {
  rd <- lapply(ros$key, .dseq)
  strong <- vapply(rd, function(d)
    length(setdiff(d, unique(.stem1(c(generic, .WEAK))))) > 0L, logical(1))
  ok <- lengths(rd) >= 2L & strong
  rd <- rd[ok]; rid <- which(ok)
  if (!length(rid)) return(data.table())
  # index firms by each distinctive token: a firm can only match a variant that
  # contains every one of its tokens, so it must appear under one of them
  tix <- split(rep(seq_along(rid), lengths(rd)), unlist(rd))
  out <- vector("list", length(vars))
  for (i in seq_along(vars)) {
    vt <- .dseq(vars[i])
    cnd <- unique(unlist(tix[intersect(names(tix), vt)], use.names = FALSE))
    if (!length(cnd)) next
    hitk <- cnd[vapply(cnd, function(k) .run_in(rd[[k]], vt), logical(1))]
    if (!length(hitk)) next
    nl <- lengths(rd[hitk])
    best <- hitk[nl == max(nl)]
    ids <- unique(ros$key[rid[best]])
    # second-firm guard: does the debris around the winning run name another firm?
    bk <- best[1]; at <- .run_at(rd[[bk]], vt)[1]
    rest <- vt[-seq(at, at + length(rd[[bk]]) - 1L)]
    others <- character(0)
    if (length(rest)) {
      c2 <- unique(unlist(tix[intersect(names(tix), rest)], use.names = FALSE))
      # exclude only the WINNER: a second firm that also matched the whole variant
      # is still a second firm ("..._consulting_engineers_and_mbk_engineers" matches
      # both LSCE and MBK Engineers, and MBK is exactly what must not be dropped)
      c2 <- setdiff(c2, best)
      if (length(c2)) {
        h2 <- c2[vapply(c2, function(k) .run_in(rd[[k]], rest), logical(1))]
        others <- unique(ros$key[rid[h2]])
      }
    }
    out[[i]] <- data.table(variant = vars[i], n_cand = length(ids),
                           rkey = ids[1], rlabel = ros$label[rid[best]][1],
                           score = max(nl),
                           second = if (length(others)) others[1] else NA_character_)
  }
  rbindlist(out)
}

.read_ov <- function(fn, pfx) {
  p <- nip_input(fn)
  if (!file.exists(p)) return(data.table(variant = character(), bag_id = character()))
  ov <- fread(p, colClasses = "character")
  if (!all(c("variant", "bag_id") %in% names(ov)))
    stop("override file lacks variant/bag_id columns: ", p)
  ov <- unique(ov[nzchar(variant) & nzchar(bag_id), .(variant, bag_id)], by = "variant")
  # a hand-written bag_id that misses its namespace prefix would land in no
  # namespace at all and pass every downstream assertion
  bad <- ov[!grepl(paste0("^", pfx, "_"), bag_id)]
  if (nrow(bad)) stop(sprintf("override %s: %d bag_id(s) not in the %s_ namespace: %s",
                              basename(p), nrow(bad), pfx,
                              paste(head(bad$bag_id, 5), collapse = ", ")))
  ov
}

# ---- self-clustering ---------------------------------------------------------
# A reordering of the same tokens is usually a harmless "X of Y" / "Y X" inversion
# ("land_trust_of_napa_county" ~ "napa_county_land_trust", "oregon_university" ~
# "university_of_oregon"), which is a contiguous block ROTATION. An interior swap is
# NOT, and it is exactly what distinguishes two real organizations:
# "california_polytechnic_state_university" (Cal Poly San Luis Obispo) vs
# "california_state_polytechnic_university" (Cal Poly Pomona). Only rotations fold.
.is_rotation <- function(a, b) {
  n <- length(a)
  if (n != length(b)) return(FALSE)
  if (identical(a, b)) return(TRUE)
  if (n < 2L) return(FALSE)
  for (k in seq_len(n - 1L))
    if (identical(c(a[(k + 1L):n], a[seq_len(k)]), b)) return(TRUE)
  FALSE
}

# vars: character vector of (already normalized) variant names within ONE type.
# Returns list(tab, edges): tab has one row per variant (comp id, tier summary),
# edges is the auto-fold pair list with its tier, kept so `method` can be reported
# honestly against the bag's representative rather than against any incident edge.
.self_cluster <- function(vars, generic) {
  allow <- unique(.stem1(c(generic, .DEBRIS)))
  strongwords <- unique(.stem1(c(generic, .WEAK, .DEBRIS)))
  n  <- length(vars)
  sq <- lapply(vars, .dseq)                  # distinctive tokens, stemmed, in order
  ds <- lapply(sq, function(z) .rsort(unique(z)))
  nd <- lengths(ds)

  # inverted index on stemmed tokens -> only compare variants that share a token.
  # Both token tiers require a shared token, so this prunes without losing pairs.
  idx <- split(rep(seq_len(n), nd), unlist(ds))
  cand <- unique(rbindlist(lapply(idx[lengths(idx) > 1L], function(ii) {
    cb <- combn(sort(ii), 2L); data.table(i = cb[1, ], j = cb[2, ])
  })))
  ei <- integer(0); ej <- integer(0); em <- character(0)
  ri <- integer(0); rj <- integer(0)         # rejected reorderings -> review
  # tier: collapse — identical once tokenization and citation digits are removed.
  # Computed globally (not through the shared-token index) so it cannot be missed.
  fl <- split(seq_len(n), .flat(vars))
  for (ii in fl[lengths(fl) > 1L]) {
    cb <- combn(sort(ii), 2L)
    ei <- c(ei, cb[1, ]); ej <- c(ej, cb[2, ]); em <- c(em, rep("collapse", ncol(cb)))
  }
  if (length(cand) && nrow(cand)) {
    for (r in seq_len(nrow(cand))) {
      i <- cand$i[r]; j <- cand$j[r]
      if (nd[i] == 0L || nd[j] == 0L) next
      si <- ds[[i]]; sj <- ds[[j]]
      if (identical(si, sj)) {               # tier: tokset (rotations only)
        if (.is_rotation(sq[[i]], sq[[j]])) {
          ei <- c(ei, i); ej <- c(ej, j); em <- c(em, "tokset")
        } else {
          ri <- c(ri, i); rj <- c(rj, j)     # interior swap -> two organizations
        }
        next
      }
      # tier: contains — the subset must appear as a CONTIGUOUS RUN in the superset,
      # must be substantive, and every extra token must be generic or document debris
      sub <- NULL; sup <- NULL
      if (all(si %in% sj)) { sub <- i; sup <- j } else if (all(sj %in% si)) { sub <- j; sup <- i }
      if (is.null(sub) || nd[sub] < MIN_SUB_TOKENS) next
      if (!length(setdiff(ds[[sub]], strongwords))) next   # bare generic/weak parent
      if (!.run_in(sq[[sub]], sq[[sup]])) next             # interleaved -> not a mention
      extra <- setdiff(ds[[sup]], ds[[sub]])
      if (length(extra) && all(extra %in% allow)) {
        ei <- c(ei, i); ej <- c(ej, j); em <- c(em, "contains")
      }
    }
  }

  g <- igraph::make_empty_graph(n = n, directed = FALSE)
  if (length(ei)) g <- igraph::add_edges(g, as.vector(rbind(ei, ej)))
  comp <- igraph::components(g)$membership

  dstem <- unique(.stem1(.DEBRIS))
  list(tab = data.table(variant = vars, comp = comp,
                        ndt = nd, ntok = lengths(lapply(vars, .toks)),
                        ndeb = vapply(ds, function(z) sum(z %in% dstem), integer(1)),
                        nstr = vapply(ds, function(z) sum(!z %in% dstem), integer(1)),
                        nabb = vapply(vars, function(z)
                          sum(.dtoks(z) %in% names(.ABBREV)), integer(1), USE.NAMES = FALSE)),
       edges = data.table(i = ei, j = ej, tier = em),
       reorder = data.table(i = ri, j = rj))
}

# Canonical member of a component, deterministic, in two stages:
#   (1) collapse each flat form to its LEAST-FRAGMENTED spelling, so a tokenization
#       artifact ("uni_versity_of_california_davis", "73_lawrence_...") can never be
#       the label while its well-formed twin is present;
#   (2) among those, take the name carrying the LEAST document debris, then the
#       fullest real name (most non-debris distinctive tokens), then the FEWEST
#       abbreviated tokens, then the shortest string, then alphabetical. The
#       abbreviation step is what stops a name from being labelled by its own
#       shorthand: "natl_audubon_socy" and "national_audubon_society" tie on both
#       token counts, and shortest-string alone picked the abbreviation.
#       So the bag is labelled
#       "environmental_defense_fund", not the truncated "environmental_defense",
#       and the Nature Conservancy bag is "nature_conservancy", not the longest
#       debris string in its component
#       ("umbrella_groundwater_sustainability_plan_nature_conservancy_checklist").
.canon_pick <- function(d) {
  x <- copy(d)[, flat := .flat(variant)]
  x <- x[order(flat, ntok, nchar(variant), variant)][, .SD[1L], by = flat]
  x[order(ndeb, -nstr, nabb, nchar(variant), variant)][1L]
}

# ---- acronym / initialism tier -----------------------------------------------
# Neither token containment nor adist can bridge "lsce" and
# "luhdorff_scalmanini_consulting_engineers", so initialisms never met their
# expansions: 0 acronym pairs appeared among the 406 Research review candidates.
# Initials are taken over the name's own tokens with only articles/prepositions
# dropped — legal forms are kept, because they are in the acronym ("rcac" = Rural
# Community Assistance CORPORATION).
#
# UNIQUENESS GUARD: fold only when exactly one bag in the type expands to the
# initialism. That is what refuses "bgc" (BGC Engineering vs Bondy Groundwater
# Consulting), "ch" (CH2M Hill vs Cleath-Harris), "scs" (Stantec Consulting Services
# vs SCS Engineers), "uc" (U of California vs U of Colorado) and "csu" (California /
# Chico / Colorado State University).
.ACRO_SKIP <- c("the", "of", "a", "an", "and", "for", "to", "in", "at", "on", "et", "al")
# Initialisms whose conventional expansion is NOT the only name in this corpus whose
# initials match, so the structural guards below cannot see the ambiguity:
#   lsu  conventionally Louisiana State University, which has no bag here, while
#        "la_sierra_university" matches the initials and would wrongly claim it.
.ACRO_BLOCK <- c("lsu")
.acro_of <- function(x) {
  # roster labels carry spaces and punctuation ("S.S. Papadopulos & Associates"),
  # so normalize before tokenizing or the whole label is one token
  t <- setdiff(.strip_cit(.toks(.depunct(.norm(x)))), .ACRO_SKIP)
  if (length(t) < 2L) return(NA_character_)
  paste(substr(t, 1L, 1L), collapse = "")
}
.acronym_merge <- function(res) {
  bags <- unique(res[, .(bag_id, bag_label)])
  cand <- res[, .N, by = .(variant, bag_id, bag_label)][
    grepl("^[a-z]{2,7}$", variant) & nchar(variant) >= ACRO_MIN &
      nchar(variant) <= ACRO_MAX]
  if (!nrow(cand)) return(data.table())
  # only a bag that is JUST this acronym can move; a bag with other members has
  # already been given an identity by a stronger tier
  sz <- res[, .N, by = bag_id]
  cand <- merge(cand, sz, by = "bag_id", suffixes = c("", ".bag"))[N.bag == 1L]
  if (!nrow(cand)) return(data.table())
  exp_acro <- vapply(bags$bag_label, .acro_of, character(1), USE.NAMES = FALSE)
  # Every token of every bag label, so a competing firm that SPELLS the initialism
  # in its own name can veto the fold. The initials test alone cannot see these:
  # "con_bgc_engineering_inc" has initials "bei", not "bgc", so without this guard
  # the bare "bgc" folded onto Bondy Groundwater Consulting while BGC Engineering
  # sat in its own bag; likewise "gsi" onto Groundwater Solutions Inc while
  # GSI Water Solutions and GSI Environmental both exist.
  own <- split(bags$bag_id, seq_len(nrow(bags)))
  tok <- lapply(bags$bag_label, function(z) .dtoks(.norm(z)))
  out <- list(); ref <- list()
  for (r in seq_len(nrow(cand))) {
    a <- cand$variant[r]
    tgt <- bags[!is.na(exp_acro) & exp_acro == a & bag_id != cand$bag_id[r]]
    # No bag's initials spell it, so it is not an initialism we can resolve at all —
    # most single-token firm names land here ("aecom", "stantec", "psomas"). Skip
    # silently; these are not ambiguous, they are simply not acronyms.
    if (!nrow(tgt)) next
    rival <- bags$bag_id[vapply(seq_along(tok), function(k)
      a %in% tok[[k]] && bags$bag_id[k] != cand$bag_id[r], logical(1))]
    why <- NA_character_
    if (a %in% .ACRO_BLOCK)  why <- "conventional expansion absent from corpus"
    else if (length(rival))  why <- paste0("another bag spells it: ",
                                           paste(head(rival, 3), collapse = " "))
    else if (nrow(tgt) > 1L) why <- paste0(nrow(tgt), " expansions match: ",
                                           paste(head(tgt$bag_id, 3), collapse = " "))
    if (!is.na(why)) {
      ref[[length(ref) + 1L]] <- data.table(
        variant = a, bag_id = cand$bag_id[r], note = why)
      next
    }
    out[[length(out) + 1L]] <- data.table(
      variant = a, bag_id = tgt$bag_id[1], bag_label = tgt$bag_label[1])
  }
  list(merge = rbindlist(out), refused = rbindlist(ref))
}

# ---- multi-organization mentions ---------------------------------------------
# A mention string that names TWO organizations is co-authorship evidence. Folding
# it onto one of them silently deletes the other's tie, so these are reported rather
# than trusted. The 1:1 variant -> bag schema cannot carry two bags per variant, so
# the second organization is emitted here for the edge builder to pick up later.
.find_compounds <- function(res) {
  bl <- unique(res[, .(bag_id, bag_label)])
  core <- lapply(bl$bag_label, .dseq)
  ok <- lengths(core) >= 2L
  bl <- bl[ok]; core <- core[ok]
  if (!nrow(bl)) return(data.table())
  tix <- split(rep(seq_len(nrow(bl)), lengths(core)), unlist(core))
  out <- list()
  for (r in seq_len(nrow(res))) {
    vt <- .dseq(res$variant[r])
    if (length(vt) < 4L) next
    cnd <- unique(unlist(tix[intersect(names(tix), vt)], use.names = FALSE))
    cnd <- cnd[vapply(cnd, function(k) .run_in(core[[k]], vt), logical(1))]
    cnd <- setdiff(bl$bag_id[cnd], res$bag_id[r])
    if (!length(cnd)) next
    out[[length(out) + 1L]] <- data.table(
      entity_type = res$entity_type[r], variant = res$variant[r],
      bag_id = res$bag_id[r], also_names = paste(sort(cnd), collapse = " | "),
      n_other = length(cnd))
  }
  rbindlist(out)
}

# ---- review menu: candidate BAG-to-BAG merges --------------------------------
# Runs over bags, not variants, so near-misses between two already-folded bags are
# visible too. Labels are NORMALIZED first: a roster label like "CH2M Hill" or
# "EKI Environment & Water, Inc." tokenizes to a SINGLE token on "_", which made the
# `contains` tier structurally unreachable for the 76 roster-labelled consultant bags
# and left adist comparing "CH2M Hill" to "ch_m_hill_engineers_inc". Only 4 of 469
# consultant review rows involved a roster label before this (findings §7c).
#
# Tiers, all REVIEW-only (nothing here folds automatically):
#   contains  one bag's tokens sit inside the other's but an extra token is
#             substantive, so it needs a human call
#   reorder   token sets equal, order is not a rotation (Cal Poly SLO vs Pomona)
#   fuzzy     adist similarity >= FUZZY_SHOW
#
# Two flags keep the menu honest rather than filtering it:
#   magnet    the subset bag is a bare institutional form that many distinct
#             organizations elaborate ("farm_bureau", "audubon", "sierra_club").
#             154 of 345 NGO `contains` rows were anchored on one; a reviewer
#             working the menu top-down would have merged 26 independent county
#             Farm Bureaus into a single bag.
#   sibling   a fuzzy pair whose differing tokens are unrelated words, i.e. two
#             members of a family rather than two spellings of one name
#             ("university_of_nebraska" vs "university_of_nevada"). Research fuzzy
#             is dense with these: all the UC and CSU campuses sit at 0.80-0.90.
.bag_review <- function(bags, generic) {
  n <- nrow(bags)
  if (n < 2L) return(data.table())
  allow <- unique(.stem1(c(generic, .DEBRIS)))
  raw <- bags$bag_label
  lab <- .norm(raw)
  sq  <- lapply(lab, .dseq)
  ds  <- lapply(sq, function(z) .rsort(unique(z))); nd <- lengths(ds)

  pairs <- list()
  idx <- split(rep(seq_len(n), nd), unlist(ds))
  cand <- unique(rbindlist(lapply(idx[lengths(idx) > 1L], function(ii) {
    cb <- combn(sort(ii), 2L); data.table(i = cb[1, ], j = cb[2, ])
  })))
  if (length(cand) && nrow(cand)) {
    for (r in seq_len(nrow(cand))) {
      i <- cand$i[r]; j <- cand$j[r]
      if (nd[i] == 0L || nd[j] == 0L) next
      si <- ds[[i]]; sj <- ds[[j]]
      if (identical(si, sj)) {
        if (!.is_rotation(sq[[i]], sq[[j]]))
          pairs[[length(pairs) + 1L]] <- data.table(
            i = i, j = j, method = "reorder", score = 1, sub = NA_integer_,
            note = "same tokens, different order")
        next
      }
      sub <- if (all(si %in% sj)) i else if (all(sj %in% si)) j else next
      sup <- if (sub == i) j else i
      extra <- setdiff(ds[[sup]], ds[[sub]])
      if (!length(extra) || all(extra %in% allow)) next   # would have auto-folded
      pairs[[length(pairs) + 1L]] <- data.table(
        i = i, j = j, method = "contains",
        score = round(nd[sub] / nd[sup], 3), sub = sub,
        note = paste0("extra: ", paste(extra, collapse = " ")))
    }
  }
  m  <- adist(lab, lab)
  s  <- 1 - m / outer(nchar(lab), nchar(lab), pmax)
  s[lower.tri(s, diag = TRUE)] <- -Inf
  fz <- which(s >= FUZZY_SHOW, arr.ind = TRUE)
  if (nrow(fz)) pairs[[length(pairs) + 1L]] <- data.table(
    i = fz[, 1], j = fz[, 2], method = "fuzzy",
    score = round(s[fz], 3), sub = NA_integer_, note = "")

  pr <- rbindlist(pairs, fill = TRUE)
  if (!nrow(pr)) return(data.table())
  # one row per bag pair; structural evidence outranks a bare fuzzy score
  pr[, `:=`(a = pmin(i, j), b = pmax(i, j))]
  pr <- pr[order(a, b, method == "fuzzy", -score)][, .SD[1L], by = .(a, b)]

  # flag the bare institutional forms that many organizations elaborate
  mg <- pr[method == "contains" & !is.na(sub), .N, by = sub][N >= MAGNET_MIN, sub]
  pr[, flag := ""]
  pr[method == "contains" & sub %in% mg, flag := "magnet"]
  # separate misspellings from family siblings among the fuzzy pairs
  if (any(pr$method == "fuzzy")) {
    sib <- vapply(which(pr$method == "fuzzy"), function(k) {
      da <- setdiff(ds[[pr$a[k]]], ds[[pr$b[k]]])
      db <- setdiff(ds[[pr$b[k]]], ds[[pr$a[k]]])
      if (!length(da) || !length(db)) return(FALSE)
      # a spelling difference pairs each odd token with a near-identical partner
      !all(vapply(da, function(u) any(1 - as.integer(adist(u, db)) /
                                        pmax(nchar(u), nchar(db)) >= 0.7), logical(1)))
    }, logical(1))
    pr[which(pr$method == "fuzzy")[sib], flag := "sibling"]
  }
  data.table(entity_type = bags$entity_type[1],
             bag_a = bags$bag_id[pr$a], label_a = raw[pr$a], n_a = bags$n_members[pr$a],
             bag_b = bags$bag_id[pr$b], label_b = raw[pr$b], n_b = bags$n_members[pr$b],
             method = pr$method, score = pr$score, flag = pr$flag, note = pr$note,
             confirm_merge_bag_id = "")[
               order(flag != "", method == "fuzzy", -score)]
}

build_alter_bag_map <- function() {
  out_map <- nip_product("01_entity_classification", "alter_bag_map.csv")
  out_cl  <- nip_product("01_entity_classification", "alter_bag_clusters.csv")
  out_cp  <- nip_product("01_entity_classification", "alter_bag_compounds.csv")
  if (file.exists(out_map) && !CLOBBER) {
    message("exists, skipping (set CLOBBER=TRUE to rebuild): ", out_map); return(invisible(NULL))
  }

  nd <- fread(nip_product("01_entity_classification", "node_dictionary.csv"),
              colClasses = "character")
  all_map <- list(); all_cl <- list(); all_cp <- list()

  for (ti in seq_len(nrow(TYPES))) {
    et  <- TYPES$entity_type[ti]; pfx <- TYPES$prefix[ti]
    gen <- .GENERIC[[et]] %||% .GENERIC_COMMON
    vars <- .rsort(unique(nd[entity_type == et, name]))
    vars <- vars[nzchar(vars)]
    if (!length(vars)) { message(sprintf("[%s] no variants, skipping", et)); next }
    ov  <- .read_ov(TYPES$override[ti], pfx)
    message(sprintf("\n[%s] %d variants, namespace %s_*, %d override rows%s",
                    et, length(vars), pfx, nrow(ov),
                    if (!TYPES$self_fold[ti]) "  (DEFERRED: identity only)" else ""))

    # ---- 0. ANCHORS: curated override (wins) then roster authority -------------
    # Anchors are not held out of the clustering — they SEED it (step 2). Holding
    # them out would partition the variant set, so an override on the clean
    # spelling would leave all the messy spellings in a separate bag of their own.
    res <- data.table(variant = vars, entity_type = et,
                      bag_id = NA_character_, bag_label = NA_character_,
                      method = NA_character_, score = NA_real_)
    hit <- match(res$variant, ov$variant)
    oi  <- which(!is.na(hit))
    if (length(oi)) res[oi, `:=`(bag_id = ov$bag_id[hit[oi]],
                                 bag_label = sub(paste0("^", pfx, "_"), "", ov$bag_id[hit[oi]]),
                                 method = "override")]

    if (identical(TYPES$roster[ti], "consultant_dictionary")) {
      cros <- load_consultant_roster()
      message(sprintf("[%s] dictionary authority: %d firms", et, nrow(cros)))
      # exact on the raw key, then on the stemmed key ("GEI Consultant, Inc." in the
      # roster vs "gei_consultants" in every corpus mention)
      # The stemmed key is a SET, so it also absorbs a word-order swap of a complete
      # firm name ("eki_water_environment_inc" vs the roster's "EKI Environment &
      # Water, Inc."). That is safe here in a way it is not between two corpus
      # variants: the roster is an authority and the match is the firm's WHOLE name,
      # not a fragment of it. A 1-token firm name is still refused.
      rk <- vapply(cros$key, function(z) {
        s <- .dstem(z); if (length(s) < 2L) NA_character_ else paste(s, collapse = "_")
      }, character(1), USE.NAMES = FALSE)
      vk <- vapply(res$variant, function(z) paste(.dstem(z), collapse = "_"),
                   character(1), USE.NAMES = FALSE)
      h  <- match(res$variant, cros$key)
      h2 <- match(vk, rk)
      h[is.na(h)] <- h2[is.na(h)]
      ok <- which(!is.na(h) & is.na(res$bag_id))
      if (length(ok)) {
        res[ok, `:=`(bag_id = paste0(pfx, "_", cros$key[h[ok]]),
                     bag_label = cros$label[h[ok]], method = "roster_exact", score = 1)]
        message(sprintf("[%s] roster_exact anchors: %d", et, length(ok)))
      }
      # elaborated mentions: firm tokens contained in the variant, unique longest
      sv <- res[is.na(bag_id), variant]
      rs <- .roster_subset(sv, cros, gen)
      if (nrow(rs)) {
        au <- rs[n_cand == 1L & is.na(second)]
        if (nrow(au)) {
          h3 <- match(res$variant, au$variant); k3 <- which(!is.na(h3))
          res[k3, `:=`(bag_id = paste0(pfx, "_", au$rkey[h3[k3]]),
                       bag_label = au$rlabel[h3[k3]], method = "roster_subset",
                       score = au$score[h3[k3]])]
        }
        message(sprintf("[%s] roster_subset anchors: %d (+%d ambiguous, +%d name a second firm -> review)",
                        et, nrow(au), rs[n_cand > 1L, .N], rs[n_cand == 1L & !is.na(second), .N]))
      }
    }

    # ---- 1./2. self-cluster ALL variants; anchors propagate to their component -
    rvw_extra <- data.table()
    if (TYPES$self_fold[ti] && length(vars) > 1L) {
      sc <- .self_cluster(vars, gen)
      cl <- merge(sc$tab, res[, .(variant, abag = bag_id, alab = bag_label, ameth = method)],
                  by = "variant", all.x = TRUE)
      setkey(cl, variant)
      vi <- setNames(seq_along(vars), vars)        # variant -> index in `vars`
      ed <- sc$edges
      # adjacency for the honest `method`: tier of the edge to the bag's rep
      adj <- if (nrow(ed)) rbind(ed[, .(v = i, w = j, tier)], ed[, .(v = j, w = i, tier)])
             else data.table(v = integer(), w = integer(), tier = character())
      TRANK <- c(contains = 1L, tokset = 2L, collapse = 3L)
      asn <- list(); ncf <- 0L; clash <- list()
      for (cc in split(seq_len(nrow(cl)), cl$comp)) {
        d <- cl[cc]
        anc <- unique(d[!is.na(abag), abag])
        if (length(anc) > 1L) {
          # >1 authority in one cluster: do NOT merge across them. Anchored
          # spellings keep their own bag; the rest stay on identity and the clash
          # is written to the review menu rather than becoming a new bag.
          ncf <- ncf + 1L
          da <- d[!is.na(abag)]; du <- d[is.na(abag)]
          o <- data.table(variant = da$variant, bag_id = da$abag,
                          bag_label = da$alab, method = da$ameth, joined = "anchor")
          if (nrow(du)) o <- rbind(o, data.table(
            variant = du$variant, bag_id = paste0(pfx, "_", du$variant),
            bag_label = du$variant, method = "clash_review", joined = "singleton"))
          asn[[length(asn) + 1L]] <- o
          clash[[length(clash) + 1L]] <- data.table(
            entity_type = et, bag_a = anc[1], label_a = d[abag == anc[1], alab][1],
            n_a = NA_integer_, bag_b = anc[2], label_b = d[abag == anc[2], alab][1],
            n_b = NA_integer_, method = "anchor_clash", score = 1, flag = "clash",
            note = paste0("one cluster, ", length(anc), " authorities"),
            confirm_merge_bag_id = "")
          next
        }
        if (length(anc) == 1L) {
          bid <- anc; blab <- d[abag == anc, alab][1]
          # prefer a curated roster/override label over a mechanically derived one
          pref <- d[abag == anc & ameth %in% c("roster_exact", "roster_subset"), alab]
          if (length(pref)) blab <- pref[1]
          rep <- d[abag == anc, variant]
        } else {
          cn <- .canon_pick(d); bid <- paste0(pfx, "_", cn$variant)
          blab <- cn$variant; rep <- cn$variant
        }
        ridx <- vi[rep]
        mth <- character(nrow(d)); jnd <- character(nrow(d))
        for (k in seq_len(nrow(d))) {
          if (!is.na(d$abag[k])) { mth[k] <- d$ameth[k]; jnd[k] <- "anchor"; next }
          if (nrow(d) == 1L)     { mth[k] <- "identity"; jnd[k] <- "singleton"; next }
          if (d$variant[k] %in% rep) { mth[k] <- "canonical"; jnd[k] <- "anchor"; next }
          e <- adj[v == vi[[d$variant[k]]] & w %in% ridx]
          if (nrow(e)) {
            mth[k] <- e$tier[which.max(TRANK[e$tier])]; jnd[k] <- "direct"
          } else if (nrow(d) > 1L) {
            mth[k] <- "transitive"; jnd[k] <- "transitive"
          } else { mth[k] <- "singleton"; jnd[k] <- "singleton" }
        }
        asn[[length(asn) + 1L]] <- data.table(
          variant = d$variant, bag_id = bid, bag_label = blab,
          method = mth, joined = jnd)
      }
      asn <- rbindlist(asn)
      res[, joined := NA_character_]
      m <- match(res$variant, asn$variant); mi <- which(!is.na(m))
      res[mi, `:=`(bag_id = asn$bag_id[m[mi]], bag_label = asn$bag_label[m[mi]],
                   method = asn$method[m[mi]], joined = asn$joined[m[mi]])]
      nfold <- sc$tab[, .N, by = comp][N > 1L]
      message(sprintf("[%s] self-cluster: %d variants -> %d bags (%d multi-member clusters absorbing %d variants%s)",
                      et, nrow(cl), uniqueN(res$bag_id), nrow(nfold), sum(nfold$N),
                      if (ncf) sprintf("; %d anchor clashes NOT merged", ncf) else ""))

      # ---- 2b. acronym tier ---------------------------------------------------
      ac <- .acronym_merge(res); am <- ac$merge
      if (nrow(am)) {
        ha <- match(res$variant, am$variant); ka <- which(!is.na(ha))
        res[ka, `:=`(bag_id = am$bag_id[ha[ka]], bag_label = am$bag_label[ha[ka]],
                     method = "acronym", joined = "direct")]
      }
      message(sprintf("[%s] acronym tier: %d folded (%s)%s", et, nrow(am),
                      paste(head(am$variant, 8), collapse = ", "),
                      if (nrow(ac$refused))
                        sprintf("; %d refused as ambiguous: %s", nrow(ac$refused),
                                paste(ac$refused$variant, collapse = ", ")) else ""))
      # a refused initialism is a real organization with no home — surface it
      if (nrow(ac$refused)) clash[[length(clash) + 1L]] <- data.table(
        entity_type = et, bag_a = ac$refused$bag_id, label_a = ac$refused$variant,
        n_a = NA_integer_, bag_b = "", label_b = "", n_b = NA_integer_,
        method = "acronym_ambiguous", score = NA_real_, flag = "ambiguous",
        note = ac$refused$note, confirm_merge_bag_id = "")

      # reorderings rejected by the tokset tier, surfaced for a human call
      rvw_extra <- rbindlist(clash, fill = TRUE)
    }

    # ---- 3. anything left is its own namespaced bag ---------------------------
    li <- which(is.na(res$bag_id))
    if (length(li)) res[li, `:=`(bag_id = paste0(pfx, "_", variant), bag_label = variant,
                                 method = "identity")]
    res[method %in% c("singleton", "clash_review") | is.na(method), method := "identity"]
    if (!"joined" %in% names(res)) res[, joined := NA_character_]
    res[is.na(joined), joined := fifelse(method == "identity", "singleton", "anchor")]

    # ---- 3b. a self-named bag should not be named after a footnote number -----
    # Only self-cluster/identity bags are renamed; roster- and override-anchored
    # bags keep their curated human-readable label. Reverted if cleaning would
    # make two distinct bags collide.
    self_named <- TYPES$self_fold[ti] &
      res$bag_label == sub(paste0("^", pfx, "_"), "", res$bag_id)
    res[, `:=`(cid = bag_id, clab = bag_label)]
    res[self_named, `:=`(clab = .clean_name(bag_label))]
    res[self_named, cid := paste0(pfx, "_", clab)]
    bad_cid <- res[, .(n = uniqueN(bag_id)), by = cid][n > 1L, cid]
    res[!cid %in% bad_cid, `:=`(bag_id = cid, bag_label = clab)]
    if (length(bad_cid))
      message(sprintf("[%s] %d bag names kept raw (cleaning would collide)", et, length(bad_cid)))
    res[, c("cid", "clab") := NULL]

    # one label per bag, always: the mechanically derived label of a singleton
    # override component used to compete with the roster's curated label for the
    # same bag_id, which put one bag on two rows of the cluster audit and emitted
    # a review row asking a human to merge a bag with itself
    lb <- res[, .(bag_label = bag_label[which.max(nchar(gsub("[^ ]", "", bag_label)) * 1000L +
                                                    nchar(bag_label))]), by = bag_id]
    res[, bag_label := NULL]
    res <- merge(res, lb, by = "bag_id", all.x = TRUE)

    all_map[[et]] <- res

    # ---- 4. audit + review menus ---------------------------------------------
    cls <- res[, .(bag_label = bag_label[1], n_members = .N,
                   members = paste(.rsort(variant), collapse = " | ")),
               by = .(entity_type, bag_id)][n_members > 1L][order(-n_members, bag_id)]
    all_cl[[et]] <- cls

    if (TYPES$self_fold[ti]) {
      cp <- .find_compounds(res)
      if (nrow(cp)) all_cp[[et]] <- cp
      bags <- res[, .(bag_label = bag_label[1], n_members = .N), by = .(entity_type, bag_id)]
      sg <- .bag_review(bags, gen)
      if (nrow(rvw_extra)) sg <- rbind(sg, rvw_extra, fill = TRUE)
      rv <- nip_product("01_entity_classification",
                        sprintf("alter_bag_review_%s.csv", tolower(et)))
      fwrite(sg, rv)
      message(sprintf("[%s] review menu: %d candidate bag merges (%s) -> %s%s",
                      et, nrow(sg),
                      if (nrow(sg)) paste(sprintf("%s=%d", names(table(sg$method)),
                                                  as.integer(table(sg$method))), collapse = ", ")
                      else "none", basename(rv),
                      if (nrow(cp)) sprintf("; %d multi-org mentions", nrow(cp)) else ""))
    }
  }

  map <- rbindlist(all_map, fill = TRUE)
  if (anyDuplicated(map$variant)) stop("duplicate variant in alter_bag_map")
  # namespaces must be disjoint: a bag_id may only belong to one type
  chk <- map[, uniqueN(entity_type), by = bag_id][V1 > 1L]
  if (nrow(chk)) stop("bag_id spans >1 entity_type (namespace leak): ",
                      paste(head(chk$bag_id, 5), collapse = ", "))
  # one bag, one label
  lbc <- map[, uniqueN(bag_label), by = bag_id][V1 > 1L]
  if (nrow(lbc)) stop("bag_id carries >1 bag_label: ",
                      paste(head(lbc$bag_id, 5), collapse = ", "))
  if (anyNA(map$method) || any(!nzchar(map$method))) stop("NA/empty method in alter_bag_map")
  map[, bag_type := sub("_.*$", "", bag_id)]
  bt <- map[, uniqueN(bag_type), by = entity_type][V1 > 1L]
  if (nrow(bt)) stop("entity_type spans >1 namespace prefix: ",
                     paste(bt$entity_type, collapse = ", "))

  setcolorder(map, c("variant", "entity_type", "bag_id", "bag_label",
                     "method", "joined", "score", "bag_type"))
  fwrite(map, out_map)
  fwrite(rbindlist(all_cl, fill = TRUE), out_cl)
  cpd <- rbindlist(all_cp, fill = TRUE)
  fwrite(cpd, out_cp)

  cat(sprintf("\nwrote %s (%d rows)\n", out_map, nrow(map)))
  cat("\n== variants -> bags, by type (independent namespaces) ==\n")
  print(map[, .(variants = .N, bags = uniqueN(bag_id),
                folded = sum(!method %in% c("identity")),
                shrink = sprintf("%.1f%%", 100 * (1 - uniqueN(bag_id) / .N))),
            by = entity_type][order(entity_type)])
  cat("\n== fold method by type ==\n")
  print(dcast(map[, .N, by = .(entity_type, method)], entity_type ~ method,
              value.var = "N", fill = 0))
  cat(sprintf("\nwrote %s (%d multi-member bags)\n", out_cl,
              nrow(rbindlist(all_cl, fill = TRUE))))
  cat(sprintf("wrote %s (%d multi-organization mentions)\n", out_cp, nrow(cpd)))
  invisible(map)
}

if (sys.nframe() == 0) {
  message("== build_alter_bag_map.R (CLOBBER=", CLOBBER, ") ==")
  build_alter_bag_map()
  message("== alter_bag_map complete ==")
}
