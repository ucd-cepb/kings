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
  # Any current Anthropic model id. Sonnet balances quality/cost for ~90k names;
  # switch to a Haiku id for cheaper bulk runs.
  model         = Sys.getenv("NIP_CLASSIFIER_MODEL", "claude-sonnet-5"),
  api_url       = "https://api.anthropic.com/v1/messages",
  api_version   = "2023-06-01",
  key_env       = "ANTHROPIC_API_KEY",
  key_file      = path.expand("~/Documents/Github/anthropic_key_kings"),
  batch_size    = 60,
  max_tokens    = 4096,
  max_retries   = 4,       # network/5xx/429 backoff retries per request
  cache_path    = NULL,    # set by 00_ingest_core.R; defaults below if NULL
  examples_per_category = 4
)

# The controlled vocabulary. Any label outside this set is coerced to "Nonsense".
ENTITY_TYPES <- c(
  "Local_GSA","Other_GSA","Local_Gov","State_Gov","Federal_Gov",
  "City","County","District","Company","NGO","Group","Person",
  "Basin","Natural_Feature","Geographic_Unit","Infrastructure",
  "Water_Project","Data_System","Legal","Reference","Technical","Nonsense"
)

.type_guidance <- paste(
  "Local_GSA: a Groundwater Sustainability Agency (GSA) for a specific place/basin.",
  "Other_GSA: generic/umbrella GSA references, not one specific local GSA.",
  "Local_Gov: city/county/local government body or generic local-gov concept (non-GSA).",
  "State_Gov: California state agencies, departments, boards, commissions.",
  "Federal_Gov: United States federal agencies or bodies.",
  "City: a named city or municipality.",
  "County: a named county.",
  "District: special districts (water, irrigation, flood-control, conservation, etc.).",
  "Company: private for-profit firms, consultants, engineering companies.",
  "NGO: non-governmental / nonprofit organizations.",
  "Group: committees, coalitions, boards, stakeholder or advisory groups.",
  "Person: an individual human.",
  "Basin: a groundwater basin or subbasin.",
  "Natural_Feature: rivers, creeks, lakes, aquifers, watersheds, natural features.",
  "Geographic_Unit: other geographic areas/regions/places (not basins).",
  "Infrastructure: dams, canals, wells, pipelines, treatment plants, physical works.",
  "Water_Project: named projects, programs, or management actions.",
  "Data_System: databases, monitoring systems, numerical models, data-management systems.",
  "Legal: laws, statutes, codes, regulations, legal citations.",
  "Reference: document/citation references.",
  "Technical: technical terms, concepts, measurements, non-organizational jargon.",
  "Nonsense: OCR garbage, fragments, or strings that are not meaningful entities.",
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
  d <- d[entity_type %in% ENTITY_TYPES & !is.na(name) & nzchar(name)]
  if (!nrow(d)) return(character(0))
  set.seed(20260806)
  ex <- d[, .SD[sample(.N, min(.N, per_cat))], by = entity_type]
  ex <- ex[order(entity_type)]
  paste0(sprintf('  "%s" -> %s', ex$name, ex$entity_type), collapse = "\n")
}

.system_prompt <- function() {
  paste0(
    "You classify governance-network entity names extracted from California ",
    "Groundwater Sustainability Plans into a fixed controlled vocabulary.\n\n",
    "Allowed types (choose exactly one per entity):\n", .type_guidance, "\n\n",
    "Rules:\n",
    "- Names are lowercased with underscores for spaces (e.g. aliso_water_district_gsa).\n",
    "- Return the SINGLE best-fitting type from the allowed list, verbatim.\n",
    "- If a string is not a meaningful entity (fragment, OCR error), use Nonsense.\n\n",
    "Example labels (from prior human coding):\n", .build_examples()
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
    system = .system_prompt(),
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

# ---- Public entry point ------------------------------------------------------
#' @param names   character vector of unique entity names to classify
#' @param hints   optional character vector (same length) of hints shown to the
#'                model, e.g. paste0(spacy_type, ", n=", num_appearances)
#' @return data.table(name, entity_type) for every input name
classify_entities <- function(names, hints = NULL) {
  names <- as.character(names)
  stopifnot(is.null(hints) || length(hints) == length(names))
  cache_path <- CLASSIFIER_CONFIG$cache_path %||% nip_product("entity_type_cache.csv")

  # Load cache
  cache <- if (file.exists(cache_path)) fread(cache_path, colClasses = "character") else
    data.table(name = character(), entity_type = character())
  known <- cache$name

  todo_idx <- which(!names %in% known)
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
  out[]
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
