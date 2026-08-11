#' LLM entity-type classifier (Claude API)
#'
#' The core NER pipeline tags entities with spaCy categories (ORG/LOC/GPE/...),
#' which are NOT the semantic entity types the modeling scripts need
#' (Local_GSA / Company / NGO / Group / District / ...). Historically those
#' semantic labels were a hand-coded spreadsheet with no reproducible generator.
#' This module regenerates them from current core naming by classifying each
#' unique entity name with Claude, few-shot-prompted from the prior hand labels.
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
  # Any current Anthropic model id. Haiku is the default: this is a bulk 21-way
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
  max_retries   = 4,       # network/5xx/429 backoff retries per request
  cache_path    = NULL,    # set by 00_ingest_core.R; defaults below if NULL
  overrides_path = NULL,   # paper-local name->type gazetteer; defaults below if NULL
  examples_per_category = 4
)

# The controlled vocabulary. Any label outside this set is coerced to "Nonsense".
# v2 (2026-08-10): Local_GSA + Other_GSA merged to GSA. Evaluation showed the
# split was unlearnable from the string (Other_GSA recall 0.03 -- the model sent
# 24/30 to Local_GSA because they ARE specific named GSAs; the human split was
# noise). "Is this the plan's own GSA" is a name-join against the GSA roster,
# not an LLM guess. See eval_classifier.R.
ENTITY_TYPES <- c(
  "GSA","Local_Gov","State_Gov","Federal_Gov",
  "City","County","District","Company","Consultant","NGO","Research","Group","Person",
  "Basin","Natural_Feature","Geographic_Unit","Infrastructure",
  "Water_Project","Data_System","Legal","Reference","Technical","Nonsense"
)

# Legacy human labels -> current vocabulary. Applied when reading the prior-label
# dictionary (few-shot source and eval gold) so historical splits fold in.
LABEL_REMAP <- c(Local_GSA = "GSA", Other_GSA = "GSA")
.remap_labels <- function(x) {
  x <- as.character(x)
  hit <- x %in% names(LABEL_REMAP)
  x[hit] <- LABEL_REMAP[x[hit]]
  x
}

# Definitions carry an explicit decision rule and, for the categories that
# evaluation showed leak, a negative boundary (what to route elsewhere). The
# trap pairs surfaced by the confusion matrix: City vs Local_Gov, Legal vs
# Water_Project, Technical vs Data_System/Reference, Group vs Local_Gov/Legal.
.type_guidance <- paste(
  "GSA: a Groundwater Sustainability Agency -- named or generic. The name ends in gsa or contains groundwater_sustainability_agency. Do NOT sub-split by how specific it is.",
  "Local_Gov: a city/county GOVERNMENT BODY or concept that is not a bare place name (city_council, board_of_supervisors, county departments, 'local_agencies'). A bare city name -> City; a bare county name -> County.",
  "State_Gov: California state agencies, departments, boards, commissions.",
  "Federal_Gov: United States federal agencies or bodies.",
  "City: a named city or municipality (paso_robles, city_of_san_luis_obispo). Use City even when the city acts as a government.",
  "County: a named county.",
  "District: a special district -- water/irrigation/flood-control/conservation/utility. Names often end in _district, _wd, _id, _cwd, or are district acronyms.",
  "Company: a private for-profit business that is NOT primarily an engineering/consulting/technical-services firm -- agricultural operations, land or water companies, investor-owned utilities organized as companies, technology or data vendors. A firm hired to study or write the plan (engineering, hydrogeology, consulting) -> Consultant.",
  "Consultant: a private engineering, hydrogeology, environmental, or technical-services CONSULTING firm -- the contractors that prepare GSPs (woodard_curran, dudek, luhdorff_and_scalmanini_consulting_engineers, gsi_water_solutions, montgomery_associates, provost_pritchard). Names often contain _consultants, _engineers, _associates, or _consulting. A non-consulting for-profit business -> Company; a university or research lab -> Research.",
  "NGO: a non-governmental, non-profit ADVOCACY, conservation, or membership organization (nature_conservancy, environmental_defense_fund, sierra_club, audubon, trout_unlimited). A university/research lab/policy institute -> Research; a stakeholder committee -> Group; a government body -> a *_Gov type.",
  "Research: a university, college, university cooperative-extension program, research laboratory, research center, or policy research institute / think tank (uc_davis, stanford_university, cal_poly, public_policy_institute, desert_research_institute). A government research agency (usgs, usbr) -> its *_Gov type; a private consulting firm -> Consultant; an advocacy nonprofit -> NGO; a community-college or school DISTRICT (name ends in ..._college_district) is a governance body, not a research institution -> District; a college/university root with OCR garbage or stray tokens appended (e.g. ..._max_expansion_fixed) -> Nonsense.",
  "Group: a stakeholder/advisory COMMITTEE, coalition, working group, or advisory board -- not an operating agency and not a legal instrument. An operating agency -> District/State_Gov/GSA; an agreement/MOU -> Legal; a board_of_supervisors -> Local_Gov.",
  "Person: an individual human, including a bare surname (trussell, chung).",
  "Basin: a groundwater basin or subbasin.",
  "Natural_Feature: rivers, creeks, lakes, aquifers, watersheds, natural features.",
  "Geographic_Unit: an administrative/management area or region that is not a basin, city, county, or natural feature (management_zone_6, planning_area, subregion).",
  "Infrastructure: dams, canals, wells, pipelines, treatment plants, physical works.",
  "Water_Project: a named on-the-ground project, program, or management ACTION (recharge project, restoration program). Not a law -> Legal; not a model/database -> Data_System.",
  "Data_System: a database, monitoring network/system, or NUMERICAL MODEL (names with _model, modflow, iwfm, cvhm, monitoring_network, _database, information_system).",
  "Legal: a law, statute, code, regulation, ordinance, legal agreement/MOU, settlement, or citation to one. A physical project or program -> Water_Project.",
  "Reference: a document/citation reference -- reports, memoranda, plan sections, tables, figures, appendices (technical_memorandum, chapter_3, table_5).",
  "Technical: a technical concept, measurement, parameter, or jargon that is not an organization, place, document, model, or project (specific_yield, hydraulic_conductivity). A model -> Data_System; a document -> Reference; an action/program -> Water_Project.",
  "Nonsense: OCR garbage, fragments, or strings that are not meaningful entities. Do NOT use Nonsense for a plausible surname (-> Person) or a real short acronym.",
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

# ---- Few-shot examples from prior hand labels --------------------------------
# Sampled deterministically (fixed seed) so the prompt is stable across runs.
.build_examples <- function(per_cat = CLASSIFIER_CONFIG$examples_per_category) {
  f <- nip_product("node_dictionary.csv")
  if (!file.exists(f)) return(character(0))
  d <- fread(f)
  d[, entity_type := .remap_labels(entity_type)]
  d <- d[entity_type %in% ENTITY_TYPES & !is.na(name) & nzchar(name)]
  if (!nrow(d)) return(character(0))
  set.seed(20260806)
  ex <- d[, .SD[sample(.N, min(.N, per_cat))], by = entity_type]
  ex <- ex[order(entity_type)]
  paste0(sprintf('  "%s" -> %s', ex$name, ex$entity_type), collapse = "\n")
}

# Hand-seeded examples for Consultant and Research: these categories are new in
# v3 (2026-08-10) and have NO instances in node_dictionary.csv, so .build_examples
# (which samples from the human seed) can't produce any. Known members are pinned
# deterministically by inputs/entity_type_overrides.csv; these examples just give
# the model a pattern for generalizing to firms/institutes not on that list.
.NEW_CATEGORY_EXAMPLES <- paste(
  '  "woodard_curran" -> Consultant',
  '  "gsi_water_solutions" -> Consultant',
  '  "provost_pritchard" -> Consultant',
  '  "kennedy_jenks_consultants" -> Consultant',
  '  "uc_davis" -> Research',
  '  "stanford_university" -> Research',
  '  "public_policy_institute" -> Research',
  '  "desert_research_institute" -> Research',
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
    "e.g. (GPE, n=12). Use the tag as a strong prior -- GPE/LOC -> a place type ",
    "(City/County/Basin/Natural_Feature/Geographic_Unit); PERSON -> Person; ORG -> an ",
    "organization type (GSA/District/Company/NGO/State_Gov/Federal_Gov/Local_Gov/Group) ",
    "-- but override it when the name clearly says otherwise.\n",
    "- Return the SINGLE best-fitting type from the allowed list, verbatim.\n",
    "- If a string is not a meaningful entity (fragment, OCR error), use Nonsense.\n\n",
    "Example labels (from prior human coding):\n", .build_examples(), "\n",
    .NEW_CATEGORY_EXAMPLES
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
    "each number (as a string) to one allowed type, e.g. {\"1\":\"City\"}.\n\n",
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
    req_retry(max_tries = CLASSIFIER_CONFIG$max_retries,
              is_transient = function(r) resp_status(r) %in% c(429, 500, 502, 503, 529)) |>
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
  cache_path <- CLASSIFIER_CONFIG$cache_path %||% nip_product("entity_type_cache.csv")

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
      res[is.na(res)] <- "Nonsense"           # unreturned/parse-failures default
      cache <- rbind(cache, data.table(name = nb, entity_type = res))
      # persist incrementally so a mid-run failure loses nothing
      fwrite(unique(cache, by = "name"), cache_path)
      if (b %% 10 == 0 || b == length(batches))
        message(sprintf("  batch %d/%d done", b, length(batches)))
    }
    cache <- unique(cache, by = "name")
  }

  out <- merge(data.table(name = names), cache, by = "name", all.x = TRUE, sort = FALSE)
  out[is.na(entity_type), entity_type := "Nonsense"]

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
