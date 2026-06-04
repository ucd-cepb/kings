# step0_download_from_sgma.R
# One-way refresh from the DWR SGMA portal. Scrapes every preview page,
# writes plan_family_manifest.csv (docId -> gspId -> plan family), and
# downloads any plan PDF or Elements Guide xlsx not already on disk.
#
# Filename conventions:
#   gsp_doc_id_<gspDocId>.pdf                in core_data_source_pdfs/
#   gsp_doc_id_<plan_doc_id>_elements.xlsx   in core_data_source_elements/
#
# Manifest (one row per plan PDF, additive across runs) at
#   core_data_source_pdfs/plan_family_manifest.csv
# Columns:
#   out_basename, gspDocId, gspId, canonical_gspId, version, fromGspId,
#   plan_section, submitted_date, posted_date, comment_end_date,
#   gsa_ids, gsa_names, basin, localId, displayStatus, url,
#   elements_url, elements_text, first_seen, last_seen
#
# Merge policy on rerun:
#   - Rows present in the current scrape: refreshed; last_seen updated.
#   - Rows in the existing manifest but missing from the current scrape
#     (the portal de-listed them): carried forward unchanged. last_seen
#     stays at its previous value so a stale row is visible as such.
#   - first_seen is preserved from the existing manifest when a row
#     reappears; it's set to now for genuinely new rows.
#
# How it works:
#   1. JSON enumerators (two feeds). GET the active and archived gspId
#      lists from /portal/service/gsp/submittedgsps and
#      /portal/service/gsp/archivedgsps, union them, dedupe by gspId.
#      Each row carries the fromGspId chain (used to compute version +
#      canonical_gspId per row).
#   2. Preview-page scrape. For each gspId, fetch /portal/gsp/preview/<gspId>
#      and parse out every "Groundwater Sustainability Plan" PDF (rejecting
#      Redline + Elements Guide variants from the plan-PDF list), their
#      submitted / posted / comment-end dates, the Elements Guide URL, and
#      the GSA list. ~5s rate-limited between fetches.
#   3. Manifest. Merge with the existing plan_family_manifest.csv (if any):
#      rows currently on the portal get refreshed, rows we've seen before
#      but the portal has since de-listed are carried forward unchanged so
#      historical records aren't lost. The manifest is the canonical record
#      of what should exist; every downstream piece of the pipeline keys on it.
#   4. Download. For each manifest row, fetch the plan PDF (if missing).
#      Then for each row with an elements_url, fetch the xlsx (if missing).
#      Atomic .tmp→rename so Ctrl-C never leaves a half-written file.
#      30-min timeout per attempt with one retry on failure.
#
# Run cadence: anytime you want to refresh. Preview scrape is the slow part
# (~5s × ~122 pages ≈ 10 min); downloads add another 10-30 min depending on
# how many PDFs are missing. CLOBBER=TRUE re-fetches everything.

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(rvest)
  library(xml2)
  library(data.table)
  library(stringr)
})

source("core_code/_config.R")   # provides CLOBBER, filekey, fk()

# === Constants =============================================================
# Portal feeds we pull gspId enumerations from. submittedgsps backs the
# /portal/gsp/all page (active submissions); archivedgsps backs the
# /portal/gsp/archives page (older / superseded / not-accepted plans).
# We iterate both and dedupe by gspId so every preview page on the portal
# gets scraped, regardless of which list serves it.
PORTAL_LIST_URLS   <- c(
  "https://sgma.water.ca.gov/portal/service/gsp/submittedgsps",
  "https://sgma.water.ca.gov/portal/service/gsp/archives"
)
PORTAL_PREVIEW_URL <- "https://sgma.water.ca.gov/portal/gsp/preview/"
PORTAL_DL_URL      <- "https://sgma.water.ca.gov/portal/service/gspdocument/download/"
REQUEST_DELAY      <- 5L      # seconds between portal requests (robots.txt crawl-delay)
DOWNLOAD_TIMEOUT   <- 1800L   # 30 min per attempt (some PDFs are 300-500 MB at slow rates)
DOWNLOAD_RETRIES   <- 1L
RETRY_BACKOFF      <- 10L
USER_AGENT         <- "UCD-kings-pipeline (contact: tascott@ucdavis.edu)"

TEST_GSP_ID <- Sys.getenv("STEP0_TEST_GSP_ID", "")  # see TEST MODE below
SKIP_SCRAPE <- tolower(Sys.getenv("STEP0_SKIP_SCRAPE", "")) %in%
                 c("1", "true", "t", "yes", "y")

pdf_dir      <- fk("core_data_source_pdfs")
elements_dir <- fk("core_data_source_elements")
dir.create(pdf_dir,      recursive = TRUE, showWarnings = FALSE)
dir.create(elements_dir, recursive = TRUE, showWarnings = FALSE)

# === Helpers ===============================================================
pad4 <- function(x) ifelse(is.na(x) | !nzchar(x), NA_character_,
                           formatC(as.integer(x), width = 4L, format = "d", flag = "0"))

# strip_saveas: drop ?saveas=... query parameter. The portal embeds it as a
# Save-As filename hint; raw spaces/parens in the hint make curl reject the
# URL as "URL using bad/illegal format". We name files ourselves.
strip_saveas <- function(u) {
  if (is.na(u) || !nzchar(u)) return(u)
  u <- gsub("([?&])saveas=[^&]*", "\\1", u, perl = TRUE)
  u <- gsub("\\?&",       "?", u, perl = TRUE)
  u <- gsub("[?&]$",      "",  u, perl = TRUE)
  u
}

# Walk fromGspId chain. Each "v1" row is a root; an amendment whose parent
# is a v1 row is v2; chain back to the root yields canonical_gspId.
compute_chain <- function(gsp_id, from_gsp_id) {
  parent <- setNames(from_gsp_id, gsp_id)
  walk <- function(g, seen) {
    p <- parent[[g]]
    if (is.null(p) || is.na(p) || !nzchar(p)) return(list(version = 1L, root = g))
    if (p %in% seen)                          return(list(version = length(seen) + 1L, root = seen[1L]))
    if (!(p %in% names(parent)))              return(list(version = length(seen) + 2L, root = p))
    up <- walk(p, c(seen, g))
    list(version = up$version + 1L, root = up$root)
  }
  r <- lapply(gsp_id, walk, seen = character(0))
  list(version          = vapply(r, function(x) x$version, integer(1)),
       canonical_gspId  = vapply(r, function(x) as.character(x$root), character(1)))
}

# Atomic download with retry. Stages to <out>.tmp, renames on success.
safe_download <- function(url, out_path) {
  tmp <- paste0(out_path, ".tmp")
  on.exit(if (file.exists(tmp)) try(file.remove(tmp), silent = TRUE), add = TRUE)
  attempts <- DOWNLOAD_RETRIES + 1L
  for (attempt in seq_len(attempts)) {
    res <- tryCatch(
      httr::GET(url,
                httr::user_agent(USER_AGENT),
                httr::write_disk(tmp, overwrite = TRUE),
                httr::timeout(DOWNLOAD_TIMEOUT)),
      error = function(e) {
        message("    download error (attempt ", attempt, "/", attempts, "): ",
                conditionMessage(e))
        NULL
      }
    )
    if (!is.null(res) && httr::status_code(res) < 400L &&
        file.exists(tmp) && file.size(tmp) > 0L) {
      file.rename(tmp, out_path)
      return(TRUE)
    }
    if (!is.null(res) && httr::status_code(res) >= 400L) {
      message("    HTTP ", httr::status_code(res),
              " (attempt ", attempt, "/", attempts, ")")
    }
    if (attempt < attempts) {
      message("    retrying in ", RETRY_BACKOFF, "s ...")
      Sys.sleep(RETRY_BACKOFF)
      if (file.exists(tmp)) try(file.remove(tmp), silent = TRUE)
    }
  }
  FALSE
}

# ── preview-page scraper ───────────────────────────────────────────────────
# Scrape /portal/gsp/preview/<gsp_id> for every "Groundwater Sustainability
# Plan" PDF the page links to (excluding Redline + Elements Guide variants),
# plus per-version submission dates, the Elements Guide URL, and the GSA
# list. Returns a data.table with one row per plan PDF found (0 rows if
# the page yields nothing). gspId is zero-padded to 4 chars.
scrape_preview_page <- function(gsp_id_raw) {
  gsp_id_padded <- pad4(gsp_id_raw)
  url <- paste0(PORTAL_PREVIEW_URL, gsp_id_raw)
  res <- tryCatch(
    httr::GET(url, httr::user_agent(USER_AGENT), httr::timeout(60)),
    error = function(e) {
      message("    preview fetch error for gspId ", gsp_id_raw, ": ",
              conditionMessage(e))
      NULL
    }
  )
  empty <- data.table(gspDocId        = character(0),
                      gspId           = character(0),
                      plan_section    = character(0),
                      link_text       = character(0),
                      submitted_date  = character(0),
                      posted_date     = character(0),
                      comment_end_date = character(0),
                      gsa_ids         = character(0),
                      gsa_names       = character(0),
                      elements_url    = character(0),
                      elements_text   = character(0))
  if (is.null(res) || httr::status_code(res) >= 400L) return(empty)
  html <- tryCatch(read_html(httr::content(res, "text", encoding = "UTF-8")),
                   error = function(e) NULL)
  if (is.null(html)) return(empty)

  # Plan PDFs per section. Within #plan-content or #incomplete-plan-content,
  # every /gspdocument/download/ anchor is plan-related — we just drop the
  # Redline diff and the Elements Guide xlsx to land on the canonical plan
  # PDF for that submission.
  extract_pdfs <- function(section_selector, section_label) {
    anchors <- html_elements(html, paste0(section_selector, " a"))
    href <- html_attr(anchors, "href")
    is_dl <- !is.na(href) & grepl("/gspdocument/download/", href)
    anchors <- anchors[is_dl]
    href    <- href[is_dl]
    if (length(anchors) == 0L) return(empty[0L])
    txt  <- html_text2(anchors)
    keep <- !grepl("Redline|Elements", txt, ignore.case = TRUE)
    if (!any(keep)) return(empty[0L])
    doc_ids <- str_match(href[keep], "/gspdocument/download/(\\d+)")[, 2L]
    data.table(gspDocId       = doc_ids,
               gspId          = gsp_id_padded,
               plan_section   = section_label,
               link_text      = txt[keep],
               submitted_date = NA_character_,
               posted_date    = NA_character_,
               comment_end_date = NA_character_,
               gsa_ids        = NA_character_,
               gsa_names      = NA_character_,
               elements_url   = NA_character_,
               elements_text  = NA_character_)
  }

  # Elements Guide anchor for a section (URL may be /gspdocument/download/<id>
  # or the dynamic /gsp/exportdata?gspId=N — strip saveas regardless).
  extract_elements <- function(section_selector) {
    anchors <- html_elements(html, paste0(section_selector, " a"))
    if (length(anchors) == 0L) return(list(url = NA_character_, text = NA_character_))
    txt  <- html_text2(anchors)
    href <- html_attr(anchors, "href")
    keep <- grepl("Elements Guide", txt, ignore.case = TRUE) &
            !grepl("Redline",       txt, ignore.case = TRUE)
    if (!any(keep)) return(list(url = NA_character_, text = NA_character_))
    idx <- which(keep)[1L]
    u   <- href[idx]
    if (!is.na(u) && startsWith(u, "/")) {
      u <- paste0("https://sgma.water.ca.gov", u)
    }
    list(url = strip_saveas(u), text = txt[idx])
  }

  # Dates live in <div> blocks ("Date Submitted", "Date Posted", "End of
  # Public Comment Period Date"), each containing the resubmitted date
  # tagged "Resubmission" plus the original date untagged.
  extract_dates <- function(label_regex) {
    divs <- html_elements(html, "div")
    if (length(divs) == 0L) return(list(original = NA_character_, resubmitted = NA_character_))
    texts <- html_text2(divs)
    hits  <- which(grepl(label_regex, texts, ignore.case = TRUE))
    if (length(hits) == 0L) return(list(original = NA_character_, resubmitted = NA_character_))
    best  <- divs[[hits[which.min(nchar(texts[hits]))]]]
    body  <- strsplit(html_text2(best), "\n")[[1]]
    body  <- trimws(body)
    body  <- body[grepl("[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}", body)]
    if (length(body) == 0L) return(list(original = NA_character_, resubmitted = NA_character_))
    dates <- str_extract(body, "[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}")
    resub <- grepl("Resubmission", body, ignore.case = TRUE)
    list(original    = if (any(!resub)) dates[!resub][1L] else NA_character_,
         resubmitted = if (any(resub))  dates[resub][1L]  else NA_character_)
  }

  pdfs <- rbind(extract_pdfs("#plan-content",            "original"),
                extract_pdfs("#incomplete-plan-content", "resubmitted"),
                fill = TRUE)
  if (nrow(pdfs) == 0L) return(empty)

  submitted <- extract_dates("Date Submitted")
  posted    <- extract_dates("Date Posted")
  end_cmt   <- extract_dates("End of Public Comment Period")
  orig_el   <- extract_elements("#plan-content")
  resub_el  <- extract_elements("#incomplete-plan-content")

  pdfs[plan_section == "original", `:=`(
    submitted_date   = submitted$original,
    posted_date      = posted$original,
    comment_end_date = end_cmt$original,
    elements_url     = orig_el$url,
    elements_text    = orig_el$text
  )]
  pdfs[plan_section == "resubmitted", `:=`(
    submitted_date   = submitted$resubmitted,
    posted_date      = posted$resubmitted,
    comment_end_date = end_cmt$resubmitted,
    elements_url     = resub_el$url,
    elements_text    = resub_el$text
  )]

  # GSA list (same list applies to every PDF on this preview page).
  gsa_anchors <- html_elements(html, "a[href*='/portal/gsa/print/']")
  if (length(gsa_anchors) > 0L) {
    gids   <- unique(str_extract(html_attr(gsa_anchors, "href"), "[0-9]+$"))
    gnames <- unique(html_text2(gsa_anchors))
    pdfs[, gsa_ids   := paste(gids,   collapse = ",")]
    pdfs[, gsa_names := paste(gnames, collapse = ";")]
  }
  pdfs[]
}

# === Main flow =============================================================
cat("=== STEP 0: REFRESH FROM SGMA PORTAL ===\n")
cat("Source PDFs dir:     ", pdf_dir,      "\n")
cat("Source elements dir: ", elements_dir, "\n")
cat("CLOBBER:             ", CLOBBER,      "\n\n")

manifest_path <- file.path(pdf_dir, "plan_family_manifest.csv")

# ── SKIP-SCRAPE shortcut ───────────────────────────────────────────────────
# STEP0_SKIP_SCRAPE=1: read the existing manifest, jump straight to the
# download phase. Useful for resuming an interrupted download run, or for
# a quick "did anything go missing on disk" check, without paying the
# ~10 min portal-scrape cost. Manifest must already exist.
if (SKIP_SCRAPE) {
  cat("STEP0_SKIP_SCRAPE=1 — skipping portal fetch + preview-page scrape.\n")
  if (!file.exists(manifest_path) || file.info(manifest_path)$size == 0L) {
    stop("Cannot skip scrape: ", manifest_path, " is missing or empty. ",
         "Run step0 without STEP0_SKIP_SCRAPE first to bootstrap the manifest.")
  }
  manifest <- data.table::fread(manifest_path,
                                colClasses = list(character = c("gspDocId", "gspId",
                                                                "canonical_gspId",
                                                                "fromGspId")))
  cat(sprintf("Loaded manifest: %s  (%d row(s))\n",
              manifest_path, nrow(manifest)))
} else {

# 1. JSON enumerators (active + archived) → unioned gspId list + fromGspId chain.
fetch_feed <- function(url) {
  res <- tryCatch(
    httr::GET(url, httr::user_agent(USER_AGENT), httr::timeout(60)),
    error = function(e) {
      message("    feed fetch error for ", url, ": ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(res) || httr::status_code(res) >= 400L) {
    if (!is.null(res)) message("    HTTP ", httr::status_code(res), " from ", url)
    return(data.table())
  }
  payload <- tryCatch(
    jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"),
                       simplifyVector = TRUE),
    error = function(e) {
      message("    JSON parse error from ", url, ": ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(payload) || !isTRUE(payload$success) || is.null(payload$data)) {
    message("    feed at ", url, " returned no usable data")
    return(data.table())
  }
  d <- as.data.table(payload$data)
  d[, .feed_source := basename(url)]
  d[]
}

cat("Fetching portal GSP feeds (active + archived) ...\n")
feed_tables <- list()
for (u in PORTAL_LIST_URLS) {
  cat("  ", u, " ...\n", sep = "")
  d <- fetch_feed(u)
  cat("    -> ", nrow(d), " row(s)\n", sep = "")
  if (nrow(d) > 0L) feed_tables[[length(feed_tables) + 1L]] <- d
}
if (length(feed_tables) == 0L) {
  stop("Both portal feeds returned 0 rows — refusing to proceed.")
}
json_tbl <- rbindlist(feed_tables, fill = TRUE)
json_tbl <- json_tbl[!is.na(gspId) & nzchar(as.character(gspId))]
# Dedupe by gspId — a gspId can in principle appear in both feeds during
# the transition; we keep the first occurrence (submittedgsps comes first
# in PORTAL_LIST_URLS, so active beats archived on tie).
json_tbl <- unique(json_tbl, by = "gspId")
cat("Portal lists ", nrow(json_tbl), " unique gspId(s) across feeds.\n", sep = "")

# Compute (version, canonical_gspId) over the JSON's fromGspId chain. We
# attach this to each scraped row by gspId join below.
chain <- compute_chain(json_tbl$gspId, json_tbl$fromGspId)
json_tbl[, version_json     := chain$version]
json_tbl[, canonical_gspId  := chain$canonical_gspId]
json_tbl[, gspId_padded     := pad4(gspId)]
json_tbl[, fromGspId_padded := pad4(fromGspId)]
json_tbl[, canonical_gspId  := pad4(canonical_gspId)]
for (col in c("basin", "localId", "displayStatus")) {
  if (!(col %in% names(json_tbl))) json_tbl[, (col) := NA_character_]
}
lineage <- unique(json_tbl[, .(gspId        = gspId_padded,
                               canonical_gspId,
                               fromGspId    = fromGspId_padded,
                               version_json,
                               basin, localId, displayStatus)])

# ── TEST MODE: scrape one preview page and dump results ────────────────────
# STEP0_TEST_GSP_ID=7 Rscript core_code/step0_download_from_sgma.R
# Prints the scraped rows for one gspId, then exits. Use to verify the
# preview-page parser before committing to the full scrape.
if (nzchar(TEST_GSP_ID)) {
  cat("\nTEST MODE: scraping preview page for gspId ", TEST_GSP_ID, " ...\n", sep = "")
  test_rows <- scrape_preview_page(TEST_GSP_ID)
  cat("\n--- scraped rows ---\n")
  if (nrow(test_rows) == 0L) {
    cat("  (no plan PDFs found)\n")
  } else {
    print(test_rows, row.names = FALSE)
  }
  quit(save = "no")
}

# 2. Scrape every preview page.
unique_gsp_ids <- unique(json_tbl$gspId)
cat(sprintf("\nScraping %d preview page(s) at %ds/request (~%d min) ...\n",
            length(unique_gsp_ids), REQUEST_DELAY,
            ceiling(length(unique_gsp_ids) * REQUEST_DELAY / 60)))
scraped_rows <- vector("list", length(unique_gsp_ids))
for (i in seq_along(unique_gsp_ids)) {
  gid <- unique_gsp_ids[i]
  cat(sprintf("  [%d/%d] gspId %s ...\n", i, length(unique_gsp_ids), gid))
  scraped_rows[[i]] <- scrape_preview_page(gid)
  Sys.sleep(REQUEST_DELAY)
}
scraped <- rbindlist(scraped_rows, fill = TRUE)
scraped <- scraped[!is.na(gspDocId) & nzchar(gspDocId)]
cat(sprintf("Scraped %d plan PDF row(s) across %d gspId(s).\n",
            nrow(scraped), length(unique(scraped$gspId))))

# 3. Build the current-scrape rows, then MERGE with the existing manifest.
# The manifest is additive: a row that disappears from the portal between
# runs (a doc de-listed, a gspId removed) stays in the manifest with its
# last-known metadata, marked by an unchanged `last_seen` timestamp. Only
# rows present in the current scrape get refreshed.
ts_now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
fresh <- merge(scraped, lineage, by = "gspId", all.x = TRUE, sort = FALSE)
setnames(fresh, "version_json", "version", skip_absent = TRUE)
fresh[, out_basename := sprintf("gsp_doc_id_%s.pdf", gspDocId)]
fresh[, url          := paste0(PORTAL_DL_URL, gspDocId)]
fresh[, first_seen   := ts_now]
fresh[, last_seen    := ts_now]
fresh <- fresh[, .(out_basename, gspDocId, gspId, canonical_gspId,
                   version, fromGspId, plan_section,
                   submitted_date, posted_date, comment_end_date,
                   gsa_ids, gsa_names, basin, localId, displayStatus,
                   url, elements_url, elements_text,
                   first_seen, last_seen)]

if (file.exists(manifest_path) && file.info(manifest_path)$size > 0L) {
  old <- data.table::fread(manifest_path,
                           colClasses = list(character = c("gspDocId", "gspId",
                                                           "canonical_gspId",
                                                           "fromGspId")))
  # Backwards-compat: older manifests may lack first_seen/last_seen — fill
  # with the current timestamp so they participate in the merge.
  for (col in c("first_seen", "last_seen")) {
    if (!(col %in% names(old))) old[, (col) := ts_now]
  }

  # Preserve first_seen from the existing manifest for rows we've seen
  # before; new rows get first_seen = now (already set in `fresh`).
  prior_first_seen <- setNames(old$first_seen, as.character(old$gspDocId))
  fresh[, first_seen := ifelse(gspDocId %in% names(prior_first_seen),
                               prior_first_seen[gspDocId],
                               first_seen)]

  # Carry forward rows that were in the manifest but aren't in this scrape
  # (portal de-listed them). Their last_seen is whatever it was — we don't
  # claim to have seen them now.
  carry <- old[!(as.character(gspDocId) %in% as.character(fresh$gspDocId))]
  manifest <- rbindlist(list(fresh, carry), use.names = TRUE, fill = TRUE)
  cat(sprintf("Manifest merge: %d refreshed, %d new, %d carried forward (not in current scrape)\n",
              sum(fresh$gspDocId %in% old$gspDocId),
              sum(!(fresh$gspDocId %in% old$gspDocId)),
              nrow(carry)))
} else {
  manifest <- fresh
  cat(sprintf("Manifest bootstrap: %d new row(s)\n", nrow(manifest)))
}

setorder(manifest, canonical_gspId, version, gspId, gspDocId)
fwrite(manifest, manifest_path)
cat(sprintf("Wrote manifest: %s  (%d row(s))\n", manifest_path, nrow(manifest)))

}   # end of !SKIP_SCRAPE branch

# 4. Download missing plan PDFs.
existing_pdfs <- list.files(pdf_dir, pattern = "\\.pdf$")
to_fetch <- if (CLOBBER) manifest else manifest[!(out_basename %in% existing_pdfs)]
cat(sprintf("\nPlan PDFs:  %d in manifest, %d to download\n",
            nrow(manifest), nrow(to_fetch)))
d_ok <- d_fail <- 0L
for (i in seq_len(nrow(to_fetch))) {
  row <- to_fetch[i]
  out_path <- file.path(pdf_dir, row$out_basename)
  message(sprintf("[%d/%d] %s  <-  gspDocId %s",
                  i, nrow(to_fetch), row$out_basename, row$gspDocId))
  ok <- safe_download(row$url, out_path)
  if (ok) d_ok <- d_ok + 1L else d_fail <- d_fail + 1L
  Sys.sleep(REQUEST_DELAY)
}
cat(sprintf("PDFs done. Downloaded: %d  Failed: %d  Already-present: %d\n",
            d_ok, d_fail, nrow(manifest) - nrow(to_fetch)))

# 5. Download missing Elements Guide xlsx files.
has_elem <- manifest[!is.na(elements_url) & nzchar(elements_url)]
if (nrow(has_elem) > 0L) {
  has_elem[, elements_basename := sprintf("gsp_doc_id_%s_elements.xlsx", gspDocId)]
  existing_elem <- list.files(elements_dir, pattern = "\\.xlsx$")
  to_fetch_elem <- if (CLOBBER) has_elem
                   else has_elem[!(elements_basename %in% existing_elem)]
  cat(sprintf("\nElements Guides: %d in manifest, %d to download\n",
              nrow(has_elem), nrow(to_fetch_elem)))
  e_ok <- e_fail <- 0L
  for (i in seq_len(nrow(to_fetch_elem))) {
    row <- to_fetch_elem[i]
    out_path <- file.path(elements_dir, row$elements_basename)
    message(sprintf("  [%d/%d] %s",
                    i, nrow(to_fetch_elem), row$elements_basename))
    ok <- safe_download(row$elements_url, out_path)
    if (ok) e_ok <- e_ok + 1L else e_fail <- e_fail + 1L
    Sys.sleep(REQUEST_DELAY)
  }
  cat(sprintf("Elements done. Downloaded: %d  Failed: %d  Already-present: %d\n",
              e_ok, e_fail, nrow(has_elem) - nrow(to_fetch_elem)))
} else {
  cat("\nElements Guides: 0 in manifest.\n")
}

cat("\nRefresh complete.\n")
