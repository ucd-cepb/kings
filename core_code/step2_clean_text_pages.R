# step2_clean_text_pages.R
# Filters non-prose pages from raw TSVs using density heuristics.
# Reads:  plan_txts_raw_core/<stem>.txt   (TSV: page\ttext)
# Writes: plan_txts_clean_core/<stem>.parquet
#
# Pages failing any threshold have their text set to "" but rows are kept,
# so downstream code can still match output back to the original PDF pages.

library(arrow)
library(stringr)

source("core_code/_config.R")   # provides CLOBBER, TESTING, TESTING_N, filekey, fk()

# === Step2-local thresholds ===
# Set any threshold above to 1 (or MAX_CHARACTERS to Inf)
# to disable that filter.
PUNCT_DENSITY_MAX      <- 0.10   # reference lists, TOCs
NUMERIC_DENSITY_MAX    <- 0.25   # tables, data appendices
WHITESPACE_DENSITY_MAX <- 0.75   # figures, maps
MAX_CHARACTERS         <- 20000L # oversized map/image pages

raw_dir   <- fk("plan_txts_raw_core")
clean_dir <- fk("plan_txts_clean_core")
dir.create(clean_dir, recursive = TRUE, showWarnings = FALSE)

clean_pages <- function(df) {
  text <- ifelse(is.na(df$text), "", df$text)
  n    <- pmax(nchar(text), 1L)   # guard against divide-by-zero

  punct_density   <- str_count(text, "[^[:alnum:][:space:]]") / n
  numeric_density <- str_count(text, "[0-9]")                 / n
  ws_density      <- str_count(text, "\\s")                   / n
  n_chars         <- nchar(text)

  mask <- (n_chars == 0L) |
          (punct_density   > PUNCT_DENSITY_MAX)      |
          (numeric_density > NUMERIC_DENSITY_MAX)    |
          (ws_density      > WHITESPACE_DENSITY_MAX) |
          (n_chars         > MAX_CHARACTERS)

  df$text[mask] <- ""
  list(df = df, n_blanked = sum(mask))
}

files <- list.files(raw_dir, pattern = "\\.txt$", full.names = TRUE)
cat(sprintf("Files in %s: %d\n", basename(raw_dir), length(files)))
if (TESTING) {
  files <- head(files, TESTING_N)
  cat(sprintf("TESTING mode: processing %d file(s)\n", length(files)))
}

cleaned <- skipped <- failed <- 0L

for (raw_path in files) {
  stem     <- str_remove(basename(raw_path), "\\.txt$")
  out_path <- file.path(clean_dir, paste0(stem, ".parquet"))

  if (file.exists(out_path) && !CLOBBER) {
    skipped <- skipped + 1L
    next
  }

  res <- tryCatch({
    df <- read.delim(raw_path, sep = "\t", quote = "",
                     colClasses = c(page = "integer", text = "character"),
                     stringsAsFactors = FALSE)
    clean_pages(df)
  }, error = function(e) {
    message("  ERROR: ", basename(raw_path), ": ", conditionMessage(e))
    NULL
  })

  if (is.null(res)) { failed <- failed + 1L; next }

  atomic_write(out_path, function(p) arrow::write_parquet(res$df, p))
  total <- nrow(res$df)
  pct   <- if (total > 0L) round(100 * res$n_blanked / total) else 0L
  message(sprintf("  %s: %d pages, %d blanked (%d%%) -> %s",
                  basename(raw_path), total, res$n_blanked, pct,
                  basename(out_path)))
  cleaned <- cleaned + 1L
}

cat(sprintf("\nDone. Cleaned: %d  Skipped: %d  Failed: %d\n",
            cleaned, skipped, failed))
