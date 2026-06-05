# migrate_pdf_naming.R
# One-time legacy cleanup. Moves every file matching the pre-docId
# naming convention into a sibling `_legacy/` subdirectory next to each
# original location. Doesn't touch the portal, doesn't write the manifest,
# doesn't download anything — that's all step0's job, run separately.
#
# Legacy patterns (matched across nine directories):
#   <dir>/(v<N>_)?gsp_num_id_<NNNN>.pdf            (source_pdfs)
#   <dir>/(v<N>_)?<NNNN>.<ext>                     (step1-5 outputs)
#   <dir>/parsed_(v<N>_)?<NNNN>.parquet            (step3 parsed parquet)
# New-convention filenames (gsp_doc_id_*.pdf, bare <NNNN>.<ext> already)
# are explicitly excluded so re-running is a no-op once migrated.
#
# Dry-run is the default. Set MIGRATE_APPLY=1 to actually move files.
# Writes data/core_data/source_pdfs/migration_log.csv (append-only) with
# every action for auditability.
#
# Run:
#   Rscript core_code/migrate_pdf_naming.R                  # dry-run
#   MIGRATE_APPLY=1 Rscript core_code/migrate_pdf_naming.R  # apply
#   # then:
#   Rscript core_code/step0_download_from_sgma.R            # refresh
#
# Once the migration is verified, the `_legacy/` directories are safe to
# delete (the new pipeline output will be under the docId convention):
#   rm -rf data/core_data/*/_legacy data/core_data/*/*/_legacy
#
# Delete this script once you've migrated and verified; it's a one-shot.

suppressPackageStartupMessages({
  library(data.table)
  library(stringr)
})

source("core_code/_config.R")   # provides filekey, fk()

APPLY <- tolower(Sys.getenv("MIGRATE_APPLY", "")) %in% c("1", "true", "t", "yes", "y")

pdf_dir <- fk("core_data_source_pdfs")
stopifnot("source PDF dir not found" = dir.exists(pdf_dir))

# Each row: filekey-key, prefix (text before <NNNN> in the basename, if any),
# extension. core_data_parsed_plans's filekey value ends with the "parsed_"
# prefix already, so we dirname() it for the directory.
artifact_specs <- list(
  list(label = "pdf",             dir = pdf_dir,                                  prefix = "gsp_num_id_", ext = "pdf"),
  list(label = "raw .txt",        dir = fk("plan_txts_raw_core"),                 prefix = "",            ext = "txt"),
  list(label = "raw_pages RDS",   dir = fk("plan_txts_raw_pages_core"),           prefix = "",            ext = "RDS"),
  list(label = "clean parquet",   dir = fk("plan_txts_clean_core"),               prefix = "",            ext = "parquet"),
  list(label = "parsed parquet",  dir = dirname(fk("core_data_parsed_plans")),    prefix = "parsed_",     ext = "parquet"),
  list(label = "extract RDS",     dir = fk("nondisambiged_extracts_core"),        prefix = "",            ext = "RDS"),
  list(label = "disambig RDS",    dir = fk("disambiged_extracts_core"),           prefix = "",            ext = "RDS"),
  list(label = "multiplex RDS",   dir = fk("multiplex_directed_graphs_core"),     prefix = "",            ext = "RDS"),
  list(label = "uniplex RDS",     dir = fk("uniplex_weighted_graphs_core"),       prefix = "",            ext = "RDS")
)

legacy_pattern <- function(prefix, ext) {
  sprintf("^(v[0-9]+_)?%s[0-9]+\\.%s$", prefix, gsub("\\.", "\\\\.", ext))
}

# ── plan ──────────────────────────────────────────────────────────────────
scrub_plan <- list()
for (spec in artifact_specs) {
  if (!dir.exists(spec$dir)) {
    cat(sprintf("  [%s] dir missing, skipping: %s\n", spec$label, spec$dir))
    next
  }
  files <- list.files(spec$dir,
                      pattern = legacy_pattern(spec$prefix, spec$ext),
                      full.names = FALSE)
  if (length(files) == 0L) next
  scrub_plan[[length(scrub_plan) + 1L]] <- data.table(
    label    = spec$label,
    dir      = spec$dir,
    old_name = files
  )
}
scrub_plan <- if (length(scrub_plan)) {
  rbindlist(scrub_plan)
} else {
  data.table(label = character(0), dir = character(0), old_name = character(0))
}

cat("\n--- scrub plan ---\n")
if (nrow(scrub_plan) == 0L) {
  cat("  (no legacy files found; nothing to do)\n")
} else {
  print(scrub_plan[, .N, by = label][order(-N)], row.names = FALSE)
  cat(sprintf("\nTotal: %d legacy file(s) to move into per-directory _legacy/ subdirs\n",
              nrow(scrub_plan)))
}

# ── apply ─────────────────────────────────────────────────────────────────
log_path <- file.path(pdf_dir, "migration_log.csv")

if (!APPLY) {
  cat("\nDRY-RUN. No files moved.\n")
  cat("Set MIGRATE_APPLY=1 to apply.\n")
  cat("After applying, run core_code/step0_download_from_sgma.R to refresh.\n")
  quit(save = "no")
}

if (nrow(scrub_plan) == 0L) {
  cat("Nothing to apply.\n")
  quit(save = "no")
}

cat("\nMoving legacy files to _legacy/ ...\n")
ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
log_rows <- list()
n_moved <- n_skipped <- n_failed <- 0L

for (i in seq_len(nrow(scrub_plan))) {
  src_dir    <- scrub_plan$dir[i]
  legacy_dir <- file.path(src_dir, "_legacy")
  dir.create(legacy_dir, recursive = TRUE, showWarnings = FALSE)

  src_path <- file.path(src_dir,    scrub_plan$old_name[i])
  dst_path <- file.path(legacy_dir, scrub_plan$old_name[i])

  if (file.exists(dst_path)) {
    n_skipped <- n_skipped + 1L
    log_rows[[length(log_rows) + 1L]] <- list(
      ts = ts, label = scrub_plan$label[i], dir = src_dir,
      old_name = scrub_plan$old_name[i],
      new_name = file.path("_legacy", scrub_plan$old_name[i]),
      action = "skip_target_exists",
      note   = "already present in _legacy/ from a previous run"
    )
    next
  }

  ok <- tryCatch(file.rename(src_path, dst_path),
                 error = function(e) {
                   message("    move failed: ", conditionMessage(e)); FALSE
                 })
  if (isTRUE(ok)) n_moved  <- n_moved  + 1L
  else            n_failed <- n_failed + 1L
  log_rows[[length(log_rows) + 1L]] <- list(
    ts = ts, label = scrub_plan$label[i], dir = src_dir,
    old_name = scrub_plan$old_name[i],
    new_name = file.path("_legacy", scrub_plan$old_name[i]),
    action = if (isTRUE(ok)) "moved_to_legacy" else "move_failed",
    note   = "legacy filename convention"
  )
}

log_df <- rbindlist(log_rows, fill = TRUE)
setcolorder(log_df, c("ts", "label", "dir", "action", "old_name", "new_name", "note"))
if (file.exists(log_path)) {
  fwrite(log_df, log_path, append = TRUE)
} else {
  fwrite(log_df, log_path)
}

cat(sprintf("\nDone. Moved: %d  Skipped (already in _legacy/): %d  Failed: %d\n  Log: %s\n",
            n_moved, n_skipped, n_failed, log_path))
cat("\nNext: Rscript core_code/step0_download_from_sgma.R\n")
