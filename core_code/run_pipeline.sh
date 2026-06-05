#!/usr/bin/env bash
# run_pipeline.sh — run the core pipeline steps in order with per-step
# stdout/stderr captured to a single timestamped log file under the
# `pipeline_run_logs` filekey path. Output is also tee'd to the terminal
# so you can watch the run.
#
# Usage (from the repo root):
#   ./core_code/run_pipeline.sh                    # all five steps + audit
#   ./core_code/run_pipeline.sh step3 step4        # subset (run in given order)
#   ./core_code/run_pipeline.sh --no-audit         # skip the audit at the end
#   ./core_code/run_pipeline.sh --clobber          # force CLOBBER=TRUE in every step
#   ./core_code/run_pipeline.sh --testing          # TESTING=TRUE (first TESTING_N files)
#   ./core_code/run_pipeline.sh --with-step0       # prepend step0 (portal refresh
#                                                  # + download) before step1..5
#
# Steps run sequentially: each one must finish before the next starts, so
# you can "queue up" later steps by listing them on the command line:
#   ./core_code/run_pipeline.sh --with-step0       # step0 -> step1..5 -> audit
#   ./core_code/run_pipeline.sh step4 step5        # step4 then step5 (skip earlier)
#
# To detach from the terminal so the queue keeps running after you log out:
#   nohup ./core_code/run_pipeline.sh --with-step0 &
#   tail -f $(ls -t data/core_data/pipeline_run_logs/run_*.log | head -1)
#
# Each step is invoked as `Rscript core_code/<step>.R` and inherits CWD
# and environment from the wrapper. Flags become env vars consumed by
# core_code/_config.R. The full env-var set (all optional, all have
# defaults — see core_code/_config.R for the values):
#   CORE_CLOBBER          (set by --clobber)
#   CORE_TESTING          (set by --testing)
#   CORE_TESTING_N        (no flag — export directly)
#   CORE_MIN_PAGE_CHARS   (no flag — export directly)
#   CORE_PARSE_WORKERS    (no flag — export directly)
#   CORE_SPACY_ENV        (no flag — export directly)
# You can export any of these before invoking the wrapper or a single Rscript.

set -euo pipefail

# Default step sequence — keep in pipeline order.
DEFAULT_STEPS=(
  step1_pdf_reader_cleaner
  step2_clean_text_pages
  step3_parse_and_extract
  step4_disambiguate_nodelists
  step5_build_igraphs
)
AUDIT_STEP="step_audit_pipeline"
RUN_AUDIT=1
WITH_STEP0=0

# === Arg parsing ===
STEPS=()
for arg in "$@"; do
  case "$arg" in
    --no-audit)   RUN_AUDIT=0 ;;
    --clobber)    export CORE_CLOBBER=1 ;;
    --testing)    export CORE_TESTING=1 ;;
    --with-step0) WITH_STEP0=1 ;;
    -h|--help)
      sed -n '2,30p' "$0" | sed 's/^# *//'
      exit 0
      ;;
    *) STEPS+=("$arg") ;;
  esac
done
if [ ${#STEPS[@]} -eq 0 ]; then
  STEPS=("${DEFAULT_STEPS[@]}")
fi
# Prepend step0 if requested. (Idempotent: only added if not already in the list.)
if [ "$WITH_STEP0" -eq 1 ] && [[ ! " ${STEPS[*]} " =~ " step0_download_from_sgma " ]]; then
  STEPS=(step0_download_from_sgma "${STEPS[@]}")
fi

# === Log file path from filekey ===
# Use python's csv module so quoted/multi-comma description fields don't
# trip up the parse (awk -F, breaks on the 3rd column when it's quoted).
LOG_DIR=$(python3 -c '
import csv, sys
for row in csv.reader(open("filekey.csv")):
    if row and row[0] == "pipeline_run_logs":
        print(row[1]); break
') || true
if [ -z "$LOG_DIR" ]; then
  echo "ERROR: 'pipeline_run_logs' row not found in filekey.csv" >&2
  exit 1
fi
mkdir -p "$LOG_DIR"
TS=$(date +%Y%m%d_%H%M%S)
LOG="${LOG_DIR}/run_${TS}.log"

# === Run ===
{
  echo "=== Pipeline run started $(date) ==="
  echo "Steps:   ${STEPS[*]}"
  [ "$RUN_AUDIT" -eq 1 ] && echo "Audit:   yes (${AUDIT_STEP})" || echo "Audit:   no"
  echo "Clobber: ${CORE_CLOBBER:-0}"
  echo "Testing: ${CORE_TESTING:-0}"
  echo "Log:     $LOG"
  echo
} | tee "$LOG"

for step in "${STEPS[@]}"; do
  script="core_code/${step}.R"
  if [ ! -f "$script" ]; then
    echo "ERROR: $script not found — skipping" | tee -a "$LOG"
    continue
  fi
  {
    echo "=== $step ==="
    echo "  started  $(date +%H:%M:%S)"
  } | tee -a "$LOG"
  # Tee stdout+stderr; pipefail surfaces any nonzero exit from Rscript
  if ! Rscript "$script" 2>&1 | tee -a "$LOG"; then
    echo "  FAILED   $(date +%H:%M:%S)" | tee -a "$LOG"
    echo "Pipeline halted on $step. See $LOG" >&2
    exit 1
  fi
  echo "  finished $(date +%H:%M:%S)" | tee -a "$LOG"
  echo | tee -a "$LOG"
done

if [ "$RUN_AUDIT" -eq 1 ]; then
  script="core_code/${AUDIT_STEP}.R"
  if [ -f "$script" ]; then
    {
      echo "=== $AUDIT_STEP ==="
      echo "  started  $(date +%H:%M:%S)"
    } | tee -a "$LOG"
    Rscript "$script" 2>&1 | tee -a "$LOG" || true   # audit failure non-fatal
    echo "  finished $(date +%H:%M:%S)" | tee -a "$LOG"
  fi
fi

{
  echo
  echo "=== Pipeline run done $(date) ==="
  echo "Log: $LOG"
} | tee -a "$LOG"
