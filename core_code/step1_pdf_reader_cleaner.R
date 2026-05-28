# step1_pdf_reader_cleaner.R
# Converts each PDF in core_data_source_pdfs to two per-GSP artifacts:
#   plan_txts_raw_core/<stem>.txt        : TSV (page<TAB>text) with tabs and
#                                          newlines stripped from page text.
#                                          Primary input for step2.
#   plan_txts_raw_pages_core/<stem>.RDS  : raw pdftools::pdf_text() output as
#                                          a character vector (one element per
#                                          page, newlines and multi-space gaps
#                                          PRESERVED). Used by step4's
#                                          find_acronyms(table_text=) so the
#                                          acronym-table front-matter pages
#                                          parse correctly.
#
# Uses pdftools (poppler) for text extraction with tesseract OCR fallback
# for scanned/image-only PDFs. Replaces the legacy monolithic
# cleaned_pdfs RDS with per-file TSVs keyed on the plan-version-prefixed stem.
#
# The non-prose page filtering (TOCs, figures, references, etc.) is now
# handled by step2_clean_text_pages.R via density heuristics, so the
# gsp_docs_lang is_comment/is_reference dependency is dropped.

library(pdftools)
library(stringr)
# callr is used to isolate OCR calls in a subprocess so that poppler
# segfaults (which can't be caught by tryCatch — they kill R at the C
# level) don't take down step1. Hard-required.
if (!requireNamespace("callr", quietly = TRUE)) {
  stop("Package 'callr' is required for segfault-safe OCR. ",
       "Install with: install.packages('callr')")
}

source("core_code/_config.R")   # provides CLOBBER, filekey, fk()

# === Step1-local tunables (garbage-page OCR repair) ===
ENG_RATIO_MIN    <- 0.30    # below this fraction of alpha tokens in eng_words,
                            # a page is treated as garbage and re-OCR'd
MIN_ALPHA_TOKENS <- 20      # don't judge a page with fewer alpha tokens
                            # (figure/map pages have legitimately sparse text)
OCR_BATCH_SIZE   <- 50L     # OCR is batched to amortize the per-call PDF
                            # open + tesseract init. Smaller batches use less
                            # peak memory; larger batches go faster.

# English-word dictionary used to detect garbage extraction from PDFs with
# broken ToUnicode CMaps (subsetted CID fonts from macOS Quartz, etc.).
# Known affected plans include 0012, 0021, 0048, 0089, 0129, 0134.
load("eng_words.rda")  # provides `eng_words`, a character vector

pdf_dir       <- fk("core_data_source_pdfs")
raw_dir       <- fk("plan_txts_raw_core")
raw_pages_dir <- fk("plan_txts_raw_pages_core")
dir.create(raw_dir,       recursive = TRUE, showWarnings = FALSE)
dir.create(raw_pages_dir, recursive = TRUE, showWarnings = FALSE)

pdfs <- list.files(path = pdf_dir, pattern = "\\.pdf$", full.names = TRUE)

# Returns the fraction of alpha-containing tokens on a page that are real
# English words. NA when the page has too few alpha tokens to judge (figures,
# numeric tables) -- those pages are left alone.
eng_word_fraction <- function(page_text) {
  toks <- unlist(strsplit(page_text, "\\s+", perl = TRUE))
  toks <- toks[nzchar(toks)]
  alpha <- toks[grepl("[A-Za-z]", toks)]
  if (length(alpha) < MIN_ALPHA_TOKENS) return(NA_real_)
  # Strip leading/trailing punctuation before dictionary lookup
  alpha_clean <- tolower(gsub("^[^A-Za-z]+|[^A-Za-z]+$", "", alpha))
  sum(alpha_clean %in% eng_words) / length(alpha_clean)
}

# Run pdf_ocr_text in a fresh R subprocess so poppler segfaults stay
# isolated. tryCatch in the parent can't catch C-level crashes; callr can.
# Returns the OCR text vector on success, NULL on any failure (timeout,
# segfault, R-level error).
#
# `wd = tempdir()` keeps pdftools' intermediate page PNGs out of the repo
# root. pdf_ocr_text -> pdf_convert writes one .png per page to the CWD
# and deletes it after OCR; on a mid-conversion crash, the PNGs are left
# behind. Tempdir gets cleaned up by the OS / next R session.
safe_ocr <- function(pdf_path, pages, timeout = 1200) {
  pdf_abs <- normalizePath(pdf_path, mustWork = TRUE)
  tryCatch(
    callr::r(
      function(p, pgs) pdftools::pdf_ocr_text(p, pages = pgs),
      args    = list(p = pdf_abs, pgs = pages),
      timeout = timeout,
      wd      = tempdir()
    ),
    error = function(e) {
      message(sprintf("      subprocess OCR error: %s", conditionMessage(e)))
      NULL
    }
  )
}

# OCR a set of page indices in chunks of OCR_BATCH_SIZE. Each chunk runs
# in one subprocess (amortizes poppler open + tesseract init). If a chunk
# crashes the subprocess (segfault) or errors, fall back to per-page calls
# -- each also in its own subprocess so one bad page can't poison the rest.
# Returns a character() the same length as `page_idx`, aligned 1-to-1.
batched_ocr <- function(pdf_path, page_idx) {
  out <- character(length(page_idx))
  chunks <- split(seq_along(page_idx),
                  ceiling(seq_along(page_idx) / OCR_BATCH_SIZE))
  n_chunks <- length(chunks)
  for (i in seq_along(chunks)) {
    ch <- chunks[[i]]
    chunk_pages <- page_idx[ch]
    message(sprintf("    OCR batch %d/%d (pages %d-%d, %d page(s))",
                    i, n_chunks,
                    min(chunk_pages), max(chunk_pages), length(chunk_pages)))
    res <- safe_ocr(pdf_path, chunk_pages)
    if (!is.null(res) && length(res) == length(chunk_pages)) {
      out[ch] <- res
    } else {
      message(sprintf("    OCR batch (pages %d-%d) failed -- retrying per-page in isolated subprocesses",
                      min(chunk_pages), max(chunk_pages)))
      for (j in seq_along(chunk_pages)) {
        pg <- chunk_pages[j]
        single <- safe_ocr(pdf_path, pg, timeout = 120)
        if (is.null(single) || !nzchar(trimws(single))) {
          message(sprintf("    OCR failed on page %d -- leaving as NA", pg))
          out[ch[j]] <- NA_character_
        } else {
          out[ch[j]] <- single
        }
      }
    }
  }
  out
}

# Identify pages where poppler returned constant-shifted glyph indices (or
# other unreadable output) and replace them with OCR text. OCR is batched
# via batched_ocr() so we don't pay the PDF-open + tesseract-init cost on
# every bad page.
repair_garbage_pages <- function(pdf_path, pages) {
  if (length(pages) == 0) return(pages)
  ratios <- vapply(pages, eng_word_fraction, numeric(1))
  bad <- which(!is.na(ratios) & ratios < ENG_RATIO_MIN)
  if (length(bad) == 0) return(pages)
  message(sprintf("    %d/%d pages look like garbage extraction -- re-OCR'ing in batches of %d",
                  length(bad), length(pages), OCR_BATCH_SIZE))
  ocr_results <- batched_ocr(pdf_path, bad)
  for (j in seq_along(bad)) {
    ocr <- ocr_results[j]
    if (is.na(ocr) || !nzchar(trimws(ocr))) {
      # OCR genuinely failed for this page (subprocess crashed or returned
      # empty). Blank it so downstream sees a clearly-missing page rather
      # than the original garbage text we tried to repair.
      pages[bad[j]] <- ""
    } else {
      pages[bad[j]] <- ocr
    }
  }
  pages
}

extract_pages <- function(pdf_path) {
  pages <- tryCatch(pdftools::pdf_text(pdf_path),
                    error = function(e) {
                      message("    pdf_text failed: ", conditionMessage(e))
                      character(0)
                    })
  # OCR fallback when poppler returns nothing useful (subprocess-isolated
  # for the same segfault-safety reason as the per-page repair below).
  if (length(pages) == 0 || all(!nzchar(trimws(pages)))) {
    message("    empty text -- trying OCR for whole document")
    pdf_abs <- normalizePath(pdf_path, mustWork = TRUE)
    pages <- tryCatch(
      callr::r(
        function(p) pdftools::pdf_ocr_text(p),
        args    = list(p = pdf_abs),
        timeout = 7200,   # 2h ceiling for OCR'ing an entire scanned PDF
        wd      = tempdir()
      ),
      error = function(e) {
        message("    whole-document OCR failed: ", conditionMessage(e))
        character(0)
      }
    )
  } else {
    # OCR per-page repair for PDFs with broken font encodings (garbage text)
    pages <- repair_garbage_pages(pdf_path, pages)
  }
  pages
}

write_tsv <- function(pages, out_path) {
  df <- data.frame(page = seq_along(pages),
                   text = pages,
                   stringsAsFactors = FALSE)
  # Strip embedded tabs and newlines so the page\ttext format stays parsable
  df$text <- str_replace_all(df$text, "[\t\r\n]+", " ")
  atomic_write(out_path, function(p) {
    write.table(df, file = p,
                sep = "\t", quote = FALSE,
                row.names = FALSE, col.names = TRUE,
                fileEncoding = "UTF-8")
  })
}

converted <- skipped <- failed <- 0L

for (pdf_path in pdfs) {
  fname    <- basename(pdf_path)
  # Preserve the plan-version prefix (v1, v2, ... refer to successive
  # submissions/revisions of the GSP itself — NOT to pipeline versions) and
  # the zero-padded numeric GSP id.
  # e.g. v1_gsp_num_id_0007.pdf -> v1_0007
  vsn      <- stringr::str_extract(fname, "^v[0-9]+")
  gsp_id   <- stringr::str_extract(fname, "[0-9]+(?=\\.pdf$)")
  if (is.na(gsp_id)) {
    message("Skipping (no numeric id): ", fname)
    skipped <- skipped + 1L
    next
  }
  stem           <- if (!is.na(vsn)) paste(vsn, gsp_id, sep = "_") else gsp_id
  out_path       <- file.path(raw_dir,       paste0(stem, ".txt"))
  raw_pages_path <- file.path(raw_pages_dir, paste0(stem, ".RDS"))

  # Skip only if BOTH outputs already exist. If only the TSV exists (from
  # a pre-raw-pages-output run), re-process so the RDS gets written too.
  if (file.exists(out_path) && file.exists(raw_pages_path) && !CLOBBER) {
    skipped <- skipped + 1L
    next
  }

  message("Converting: ", basename(pdf_path))
  pages <- extract_pages(pdf_path)
  if (length(pages) == 0) {
    failed <- failed + 1L
    next
  }
  write_tsv(pages, out_path)
  # Also save the raw per-page text (newlines / multi-space gaps preserved)
  # for step4's find_acronyms(table_text=) path.
  atomic_saveRDS(pages, raw_pages_path)
  message(sprintf("    -> %d pages -> %s (+ raw RDS)",
                  length(pages), basename(out_path)))
  converted <- converted + 1L
}

cat(sprintf("\nDone. Converted: %d  Skipped: %d  Failed: %d\n",
            converted, skipped, failed))
