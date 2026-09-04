#' LLM entity-type classifier (Claude API)
#'
#' The core NER pipeline tags entities with spaCy categories (ORG/LOC/GPE/...),
#' which are NOT the semantic entity types the modeling scripts need. Downstream
#' (see _entity_groups.R) only ever asks two things of a label: (1) is this a real
#' institutional actor at all (the noise gate), and (2) is it one of the three
#' focal org types (Consultant / Research / NGO) or a GSA. So the vocabulary is
#' exactly those distinctions -- six mutually-exclusive leaves -- and nothing
#' finer. Historically the semantic labels came from an early, ad hoc LLM pass with
#' no reproducible generator; this module regenerates them from current core naming
#' by classifying each unique entity name with Claude, few-shot-prompted from a small
#' set of curated in-code exemplars (.FEWSHOT_EXAMPLES) -- maintained here as code,
#' NOT sampled from any prior label file. The only authoritative label source is the
#' deterministic gazetteer built from core_code/dicts (build_overrides_from_dicts.R),
#' which pins the known, enumerable cast; the LLM handles the long tail.
#'
#' Design:
#'   - Deterministic re-runs: every (name -> type) is cached; only unseen names
#'     hit the API. Delete the cache to force a full re-classification.
#'   - Batched: names are sent in index-keyed batches to avoid name-mangling.
#'   - temperature = 0 for stability.
#'
#' Public entry point:
#'   classify_entities(names, hints = NULL) -> data.table(name, entity_type)
#'
#' Requires: httr2, jsonlite, data.table, stringr. Source _paths.R first.

suppressMessages({
  library(data.table); library(stringr); library(httr2); library(jsonlite)
})

CLASSIFIER_CONFIG <- list(
  # Any current Anthropic model id. Haiku is the default: this is a bulk 6-way
  # classification of short names against a controlled vocabulary (few-shot +
  # spaCy/frequency hints), not a reasoning task, so Haiku's quality is ample at
  # ~1/4 the cost of Sonnet for ~90k names. Override with NIP_CLASSIFIER_MODEL
  # (e.g. claude-sonnet-5) to spend more on the ambiguous label distinctions.
  # NOTE ON CACHING: the system prompt is cache-tagged below, but Haiku 4.5's
  # minimum cacheable prefix is 4096 tokens and this prompt is ~1.9k, so the
  # cache silently no-ops on Haiku (verified). It DOES fire on Sonnet/Opus,
  # which have a lower (1024/512) floor -- so the tag pays off only if you
  # override the model to one of those.
  model         = Sys.getenv("NIP_CLASSIFIER_MODEL", "claude-haiku-4-5"),
  api_url       = "https://api.anthropic.com/v1/messages",
  api_version   = "2023-06-01",
  key_env       = "ANTHROPIC_API_KEY",
  key_file      = path.expand("~/Documents/Github/anthropic_key_kings"),
  batch_size    = 60,
  max_tokens    = 4096,
  max_retries   = 6,       # network/5xx/429 backoff retries per request
  req_timeout   = 120,     # hard per-request cap (s): a hung/slow connection
                           # fails fast and is retried, instead of stalling ~600s
                           # on curl's low-speed timeout and killing the whole run
  cache_path    = NULL,    # set by 00_ingest_core.R; defaults below if NULL
  overrides_path = NULL    # paper-local name->type gazetteer; defaults below if NULL
)

# The controlled vocabulary. Any label outside this set is coerced to
# "Non_institutional". v3 (2026-08-12): collapsed from the legacy 23-way taxonomy
# to the six leaves downstream actually consumes (see _entity_groups.R). Two axes,
# one label: the NOISE GATE (institutional vs Non_institutional) and, within
# institutional, the FOCAL type (GSA / Consultant / Research / NGO) or the generic
# residual (Institutional_other). "Institutional" is the SUPERSET of the first
# five leaves -- it is a grouping in _entity_groups.R, never a label here.
# Institutional_other is the leftover institutional leaf (city, county, district,
# state/federal/local government body, company, stakeholder committee), i.e. a
# real actor that is not a GSA, consulting firm, research institution, or advocacy
# nonprofit. The prior v2 finding still holds: "is this the plan's own GSA" is a
# name-join against the GSA roster, not an LLM guess. See eval_classifier.R.
ENTITY_TYPES <- c(
  "GSA", "Consultant", "Research", "NGO", "Institutional_other", "Non_institutional"
)

# Six leaves on two axes: the noise gate (institutional vs Non_institutional) and,
# within institutional, the focal type or the generic residual. Definitions carry
# an explicit decision rule and, for the org types that historically leaked, a
# negative boundary (what to route elsewhere). The live trap pairs are now only
# Consultant vs Institutional_other(company) and Research vs NGO vs Consultant --
# the many fine non-institutional splits collapse into one leaf, so most of the
# old confusion surface is gone by construction.
.type_guidance <- paste(
  "GSA: a Groundwater Sustainability Agency -- named or generic. The name ends in _gsa or contains groundwater_sustainability_agency. Do NOT sub-split by how specific it is. (Institutional.)",
  "Consultant: a private engineering, hydrogeology, environmental, or technical-services CONSULTING firm -- the contractors hired to study or write GSPs (woodard_curran, dudek, luhdorff_and_scalmanini_consulting_engineers, gsi_water_solutions, montgomery_associates, provost_pritchard). Names often contain _consultants, _engineers, _associates, or _consulting. A university or research lab -> Research; any OTHER institution (a non-consulting company, agency, district, city, county, or committee) -> Institutional_other. (Institutional.)",
  "Research: an organization whose primary purpose is producing KNOWLEDGE -- studies, data, analysis, teaching: a university, college, cooperative-extension program, research laboratory, research center, or policy research institute / think tank (uc_davis, stanford_university, cal_poly, public_policy_institute, desert_research_institute). The demarcation from NGO is OUTPUT vs ADVOCACY: an entity that produces knowledge is Research; one whose purpose is advocacy, organizing, or representing members is NGO -- even a science-oriented nonprofit is Research if it mainly produces studies/data. A government research agency (usgs, usbr) -> Institutional_other; a private consulting firm -> Consultant; an advocacy nonprofit -> NGO; a community-college or school DISTRICT (name ends in ..._college_district) is a governance body -> Institutional_other; a university root with OCR garbage or stray tokens appended -> Non_institutional. A JOURNAL, periodical, or academic publication is NOT the research organization -- it is a citation/reference -> Non_institutional. (Institutional.)",
  "NGO: a non-governmental, non-profit ADVOCACY, conservation, or membership organization (nature_conservancy, environmental_defense_fund, sierra_club, audubon, trout_unlimited). The demarcation from Research is ADVOCACY vs KNOWLEDGE-OUTPUT: if the organization primarily advocates, organizes, litigates, or represents members it is NGO; if it primarily produces studies, data, or analysis (a research institute or think tank, even a nonprofit one) it is Research. A university/research lab/policy institute -> Research; a government body or a stakeholder committee -> Institutional_other. (Institutional.)",
  "Institutional_other: any real organization, government body, or place-acting-as-government that is NOT one of the four types above -- named cities and counties, special/water/irrigation/flood-control districts, California state agencies/boards/commissions, United States federal agencies, city/county government bodies (city_council, board_of_supervisors, county departments), private non-consulting companies (agricultural operations, land/water companies, investor-owned utilities, data/tech vendors), and stakeholder committees/coalitions/advisory boards. Use this for every institutional actor that is not a GSA, consulting firm, research institution, or advocacy nonprofit.",
  "Non_institutional: NOT an institution. Includes an individual person or bare surname; a groundwater basin or subbasin; a natural feature (river, creek, lake, aquifer, watershed); an administrative/management region that is not a city or county (management_zone_6, planning_area, subregion); physical infrastructure (dam, canal, well, pipeline, treatment plant); a named project/program/management action; a database, monitoring network, or numerical model (modflow, iwfm, cvhm); a law, code, regulation, ordinance, agreement/MOU, or citation to one; a document/report/memo/table/figure/appendix reference; a journal, periodical, or academic publication (american_journal_of_science, journal_of_hydrology) -- the publication itself is a citation, not the organization that produced it; a bare technical concept, measurement, or parameter (specific_yield, hydraulic_conductivity); and OCR garbage or meaningless fragments. A named city or county is NOT here -> Institutional_other.",
  sep = "\n"
)

# ---- API key -----------------------------------------------------------------
.get_api_key <- function() {
  k <- Sys.getenv(CLASSIFIER_CONFIG$key_env, "")
  if (nzchar(k)) return(trimws(k))
  kf <- CLASSIFIER_CONFIG$key_file
  if (file.exists(kf)) {
    k <- trimws(readLines(kf, warn = FALSE)[1])
    if (nzchar(k)) return(k)
  }
  stop("No Anthropic API key: set ", CLASSIFIER_CONFIG$key_env,
       " or place it at ", CLASSIFIER_CONFIG$key_file)
}

# ---- Curated few-shot examples (in-code, maintained by hand) ------------------
# A small, fixed exemplar set per leaf, written here as code -- NOT sampled from any
# label file. Earlier versions drew few-shot examples from a frozen "seed"
# dictionary, but that seed was itself the output of an early, ad hoc LLM pass, was
# noisier than the current model, and is not a trustworthy authority -- so it was
# retired. The only authoritative label source is the deterministic gazetteer built
# from core_code/dicts (build_overrides_from_dicts.R), which pins the known,
# enumerable cast; these exemplars just teach the model the PATTERN for generalizing
# to firms/orgs/features not on that list, with emphasis on the trap pairs:
# Consultant vs non-consulting company, Research vs NGO vs government agency.
.FEWSHOT_EXAMPLES <- paste(
  # GSA -- named or generic groundwater sustainability agencies
  '  "aliso_water_district_gsa" -> GSA',
  '  "kern_groundwater_authority_gsa" -> GSA',
  '  "east_kaweah_gsa" -> GSA',
  # Consultant -- private engineering/hydrogeology/technical consulting firms
  '  "woodard_curran" -> Consultant',
  '  "luhdorff_and_scalmanini_consulting_engineers" -> Consultant',
  '  "gsi_water_solutions" -> Consultant',
  '  "provost_pritchard" -> Consultant',
  '  "kennedy_jenks_consultants" -> Consultant',
  # Research -- knowledge-producing universities/labs/institutes (NON-government)
  '  "uc_davis" -> Research',
  '  "stanford_university" -> Research',
  '  "public_policy_institute_of_california" -> Research',
  '  "desert_research_institute" -> Research',
  # NGO -- advocacy/conservation/membership nonprofits
  '  "nature_conservancy" -> NGO',
  '  "environmental_defense_fund" -> NGO',
  '  "sierra_club" -> NGO',
  '  "community_water_center" -> NGO',
  # Institutional_other -- real actors that are none of the above: cities, counties,
  # districts, state/federal/local government bodies (incl. government research/water
  # agencies like usgs/usbr), non-consulting companies (ag/land/utility firms that are
  # NOT technical consultants), college districts, stakeholder committees
  '  "city_of_fresno" -> Institutional_other',
  '  "county_of_kern" -> Institutional_other',
  '  "department_of_water_resources" -> Institutional_other',
  '  "kern_county_water_agency" -> Institutional_other',
  '  "usgs" -> Institutional_other',
  '  "usbr" -> Institutional_other',
  '  "board_of_supervisors" -> Institutional_other',
  # a non-consulting company: a corporate actor, but NOT a technical consulting firm
  '  "j_g_boswell_company" -> Institutional_other',
  # a college/community-college district is a government body, not a Research institute
  '  "west_hills_community_college_district" -> Institutional_other',
  # with the parenthetical hint: an ORG place-name -> an institution
  '  "westlands_water_district (spaCy=ORG, n=57)" -> Institutional_other',
  # Non_institutional -- the reject bucket: persons, basins/features/regions,
  # infrastructure, projects, models/data systems, laws/citations, journals, bare
  # technical concepts, document references, OCR junk
  '  "kern_river" -> Non_institutional',
  '  "san_joaquin_valley" -> Non_institutional',
  '  "groundwater_sustainability_plan" -> Non_institutional',
  '  "modflow" -> Non_institutional',
  '  "specific_yield" -> Non_institutional',
  '  "journal_of_hydrology" -> Non_institutional',
  # with the parenthetical hint: a basin/natural-feature place (GPE) -> reject bucket
  '  "tulare_lake_basin (spaCy=GPE, n=41)" -> Non_institutional',
  # a university root wrapped in OCR garbage is not the institution -> reject bucket
  '  "univ_of_califoria_xii_3" -> Non_institutional',
  sep = "\n"
)

.system_prompt <- function() {
  paste0(
    "You classify governance-network entity names extracted from California ",
    "Groundwater Sustainability Plans into a fixed controlled vocabulary.\n\n",
    "Allowed types (choose exactly one per entity):\n", .type_guidance, "\n\n",
    "Rules:\n",
    "- Names are lowercased with underscores for spaces (e.g. aliso_water_district_gsa).\n",
    "- An entity may carry a parenthetical hint: its spaCy NER tag and frequency, ",
    "e.g. (GPE, n=12). Use the tag as a strong prior -- a named city/county place ",
    "(GPE) -> Institutional_other, but a basin/natural-feature/region place -> ",
    "Non_institutional; PERSON -> Non_institutional; ORG -> an institution ",
    "(GSA/Consultant/Research/NGO/Institutional_other) -- but override it when the ",
    "name clearly says otherwise.\n",
    "- Return the SINGLE best-fitting type from the allowed list, verbatim.\n",
    "- If a string is not a meaningful entity (fragment, OCR error), use Non_institutional.\n\n",
    "Example labels:\n", .FEWSHOT_EXAMPLES
  )
}

# ---- One batch ---------------------------------------------------------------
.extract_json <- function(txt) {
  if (is.null(txt) || !nzchar(txt)) return("{}")
  txt <- gsub("^```[a-zA-Z]*", "", trimws(txt))
  txt <- gsub("```$", "", txt)
  a <- regexpr("\\{", txt); b <- max(gregexpr("\\}", txt)[[1]])
  if (a > 0 && b > a) substr(txt, a, b) else "{}"
}

.classify_batch <- function(names_batch, hints_batch, key) {
  n <- length(names_batch)
  lines <- vapply(seq_len(n), function(i) {
    h <- if (!is.null(hints_batch)) paste0("  (", hints_batch[i], ")") else ""
    sprintf("%d. %s%s", i, names_batch[i], h)
  }, character(1))
  user <- paste0(
    "Classify each numbered entity. Respond with ONLY a JSON object mapping ",
    "each number (as a string) to one allowed type, e.g. {\"1\":\"GSA\"}.\n\n",
    paste(lines, collapse = "\n")
  )
  body <- list(
    model = CLASSIFIER_CONFIG$model,
    max_tokens = CLASSIFIER_CONFIG$max_tokens,
    # Structured system block with an ephemeral cache breakpoint: the prompt is
    # byte-identical across all batches, so on caching-capable models (Sonnet/
    # Opus) every batch after the first reads it at ~0.1x instead of full price
    # (~60% of input tokens). Inert on Haiku 4.5 (below its 4096-token floor).
    system = list(list(
      type = "text",
      text = .system_prompt(),
      cache_control = list(type = "ephemeral")
    )),
    messages = list(list(role = "user", content = user))
  )
  resp <- request(CLASSIFIER_CONFIG$api_url) |>
    req_headers(`x-api-key` = key,
                `anthropic-version` = CLASSIFIER_CONFIG$api_version,
                `content-type` = "application/json") |>
    req_body_json(body) |>
    # Hard per-request cap so a stalled connection errors quickly and is retried,
    # rather than hanging on curl's ~600s low-speed timeout.
    req_timeout(CLASSIFIER_CONFIG$req_timeout) |>
    # Retry transient HTTP statuses AND transport failures (timeouts/resets): a
    # single slow request should never abort a 1500-batch run.
    req_retry(max_tries = CLASSIFIER_CONFIG$max_retries,
              is_transient = function(r) resp_status(r) %in% c(429, 500, 502, 503, 529),
              retry_on_failure = TRUE) |>
    req_perform()
  # Concatenate all text-type content blocks (models may emit a leading
  # non-text block, e.g. extended "thinking", before the answer).
  blocks <- resp_body_json(resp)$content
  txt <- paste(vapply(blocks, function(b) if (!is.null(b$text)) b$text else "",
                      character(1)), collapse = "")
  parsed <- tryCatch(fromJSON(.extract_json(txt)), error = function(e) NULL)
  out <- rep(NA_character_, n)
  if (!is.null(parsed)) {
    for (i in seq_len(n)) {
      v <- parsed[[as.character(i)]]
      if (!is.null(v) && length(v) == 1) out[i] <- as.character(v)
    }
  }
  out[!out %in% ENTITY_TYPES] <- NA_character_  # reject off-vocabulary
  out
}

# ---- Deterministic gazetteer overrides ---------------------------------------
# A paper-local, curated name->type table (inputs/entity_type_overrides.csv) that
# is AUTHORITATIVE over both the cache and the LLM. It exists because the fine
# distinctions this paper cares about -- consulting firm vs generic company,
# advocacy NGO vs research institute -- are identity facts, not string facts:
# nothing in "luhdorff_and_scalmanini" or "pacific_institute" tells a classifier
# what kind of org it is. The GSP world has a small, enumerable cast of these, so
# a lookup beats guessing. Overrides are applied at read time and never written to
# the cache, so editing the CSV takes effect on the next run without a cache bust.
#
# Schema: pattern,entity_type,match[,notes]
#   match = "exact"  -> name == pattern (safe; use for short/place-colliding tokens)
#   match = "regex"  -> grepl(pattern, name) (use for distinctive multi-word roots;
#                       also folds in OCR fragments like nature_conservancy_notes)
# Precedence: regex rows apply in file order (first match wins); exact rows then
# apply on top (always win). entity_type must be in ENTITY_TYPES or the row is
# dropped with a warning.
.load_overrides <- function() {
  path <- CLASSIFIER_CONFIG$overrides_path %||% nip_input("entity_type_overrides.csv")
  if (!file.exists(path)) return(NULL)
  ov <- fread(path, colClasses = "character")
  if (!all(c("pattern", "entity_type") %in% names(ov))) {
    warning("entity_type_overrides.csv missing pattern/entity_type columns; ignoring")
    return(NULL)
  }
  if (!"match" %in% names(ov)) ov[, match := "exact"]
  ov[is.na(match) | !nzchar(match), match := "exact"]
  ov <- ov[!is.na(pattern) & nzchar(pattern)]
  bad <- ov[!entity_type %in% ENTITY_TYPES]
  if (nrow(bad)) {
    warning("Dropping ", nrow(bad), " override row(s) with off-vocabulary type(s): ",
            paste(unique(bad$entity_type), collapse = ", "))
    ov <- ov[entity_type %in% ENTITY_TYPES]
  }
  if (!nrow(ov)) return(NULL)
  ov[]
}

# Returns a character vector (same length as `names`) of the override type, or NA
# where no override matches.
.match_overrides <- function(names, ov) {
  out <- rep(NA_character_, length(names))
  if (is.null(ov) || !nrow(ov)) return(out)
  rx <- ov[match == "regex"]
  for (i in seq_len(nrow(rx))) {
    hit <- is.na(out) & grepl(rx$pattern[i], names, perl = TRUE)
    out[hit] <- rx$entity_type[i]
  }
  ex <- ov[match == "exact"]
  if (nrow(ex)) {
    m <- ex$entity_type[match(names, ex$pattern)]
    out[!is.na(m)] <- m[!is.na(m)]
  }
  out
}

# ---- Public entry point ------------------------------------------------------
#' @param names   character vector of unique entity names to classify
#' @param hints   optional character vector (same length) of hints shown to the
#'                model, e.g. paste0(spacy_type, ", n=", num_appearances)
#' @return data.table(name, entity_type) for every input name
classify_entities <- function(names, hints = NULL) {
  names <- as.character(names)
  stopifnot(is.null(hints) || length(hints) == length(names))
  cache_path <- CLASSIFIER_CONFIG$cache_path %||% nip_product("01_entity_classification", "entity_type_cache.csv")

  # Deterministic gazetteer: names it matches never hit the API (and its verdict
  # overrides the cache at the end), so exclude them from the todo set here.
  ov <- .load_overrides()
  ov_type <- .match_overrides(names, ov)

  # Load cache
  cache <- if (file.exists(cache_path)) fread(cache_path, colClasses = "character") else
    data.table(name = character(), entity_type = character())
  known <- cache$name

  todo_idx <- which(!names %in% known & is.na(ov_type))
  todo <- unique(names[todo_idx])
  if (length(todo)) {
    key <- .get_api_key()
    hint_map <- if (!is.null(hints)) setNames(hints, names) else NULL
    batches <- split(todo, ceiling(seq_along(todo) / CLASSIFIER_CONFIG$batch_size))
    message(sprintf("Classifying %d new entities in %d batches (model=%s)...",
                    length(todo), length(batches), CLASSIFIER_CONFIG$model))
    for (b in seq_along(batches)) {
      nb <- batches[[b]]
      hb <- if (!is.null(hint_map)) unname(hint_map[nb]) else NULL
      res <- .classify_batch(nb, hb, key)
      res[is.na(res)] <- "Non_institutional"  # unreturned/parse-failures default
      cache <- rbind(cache, data.table(name = nb, entity_type = res))
      # persist incrementally so a mid-run failure loses nothing
      fwrite(unique(cache, by = "name"), cache_path)
      if (b %% 10 == 0 || b == length(batches))
        message(sprintf("  batch %d/%d done", b, length(batches)))
    }
    cache <- unique(cache, by = "name")
  }

  out <- merge(data.table(name = names), cache, by = "name", all.x = TRUE, sort = FALSE)
  out[is.na(entity_type), entity_type := "Non_institutional"]

  # Apply gazetteer LAST so it wins over both the cache and the LLM. Recomputed on
  # out$name (order-independent); overrides are never persisted to the cache.
  ov_out <- .match_overrides(out$name, ov)
  hit <- !is.na(ov_out)
  if (any(hit)) {
    out[hit, entity_type := ov_out[hit]]
    message(sprintf("Applied %d gazetteer override(s) across %d unique type(s).",
                    sum(hit), length(unique(ov_out[hit]))))
  }
  out[]
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
