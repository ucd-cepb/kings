# step1_pdf_reader_cleaner.R
# Converts each PDF in core_data_source_pdfs to a per-GSP TSV
# (page\ttext) at plan_txts_raw_core/<stem>.txt.
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

# === Flags ===
CLOBBER <- FALSE   # set TRUE to re-process PDFs even if a TSV exists

filekey <- read.csv("filekey.csv")

pdf_dir <- filekey[filekey$var_name == "core_data_source_pdfs", ]$filepath
raw_dir <- filekey[filekey$var_name == "plan_txts_raw_core", ]$filepath
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

pdfs <- list.files(path = pdf_dir, pattern = "\\.pdf$", full.names = TRUE)

extract_pages <- function(pdf_path) {
  pages <- tryCatch(pdftools::pdf_text(pdf_path),
                    error = function(e) {
                      message("    pdf_text failed: ", conditionMessage(e))
                      character(0)
                    })
  # OCR fallback when poppler returns nothing useful
  if (length(pages) == 0 || all(!nzchar(trimws(pages)))) {
    message("    empty text — trying OCR")
    pages <- tryCatch(pdftools::pdf_ocr_text(pdf_path),
                      error = function(e) {
                        message("    OCR failed: ", conditionMessage(e))
                        character(0)
                      })
  }
  pages
}

write_tsv <- function(pages, out_path) {
  df <- data.frame(page = seq_along(pages),
                   text = pages,
                   stringsAsFactors = FALSE)
  # Strip embedded tabs and newlines so the page\ttext format stays parsable
  df$text <- str_replace_all(df$text, "[\t\r\n]+", " ")
  write.table(df, file = out_path,
              sep = "\t", quote = FALSE,
              row.names = FALSE, col.names = TRUE,
              fileEncoding = "UTF-8")
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
  stem     <- if (!is.na(vsn)) paste(vsn, gsp_id, sep = "_") else gsp_id
  out_path <- file.path(raw_dir, paste0(stem, ".txt"))

  if (file.exists(out_path) && !CLOBBER) {
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
  message(sprintf("    -> %d pages -> %s", length(pages), basename(out_path)))
  converted <- converted + 1L
}

cat(sprintf("\nDone. Converted: %d  Skipped: %d  Failed: %d\n",
            converted, skipped, failed))

# === GSP 0089 salvage (kings-specific) =======================================
# 0089 is text reconstructed from plaintext repair; preserved as-is for the
# salvage table. Not used in the main pipeline. Skipped on rerun unless
# CLOBBER is TRUE — without this guard the salvage redoes its work every
# step1 invocation, even when only new PDFs need processing.
salvage_out <- filekey[filekey$var_name == "gsp0089_salvage_postprocess", ]$filepath
if (CLOBBER || !file.exists(salvage_out)) {
  pdf89   <- filekey[filekey$var_name == "gsp0089_salvage_preprocessed", ]$filepath
  maxchar <- 10000

  pdf0089repair <- function(doc) {
    texts <- pdf_text(doc)
    # If too many chars, the page is probably a map or figure, not real prose
    for (i in seq_along(texts)) {
      if (nchar(texts[i]) > maxchar) texts[i] <- NA
    }
    texts
  }
  saveRDS(pdf0089repair(pdf89), salvage_out)
  message("GSP 0089 salvage written to ", salvage_out)
} else {
  message("GSP 0089 salvage exists, skipping (set CLOBBER=TRUE to redo)")
}
