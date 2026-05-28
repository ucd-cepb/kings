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
#
# Each step is invoked as `Rscript core_code/<step>.R` and inherits CWD
# from the wrapper. Pre-existing CLOBBER/TESTING flags in each script are
# honored (set them in-script before running).

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

# === Arg parsing ===
STEPS=()
for arg in "$@"; do
  case "$arg" in
    --no-audit) RUN_AUDIT=0 ;;
    -h|--help)
      sed -n '2,15p' "$0" | sed 's/^# *//'
      exit 0
      ;;
    *) STEPS+=("$arg") ;;
  esac
done
if [ ${#STEPS[@]} -eq 0 ]; then
  STEPS=("${DEFAULT_STEPS[@]}")
fi

# === Log file path from filekey ===
LOG_DIR=$(awk -F, '$1=="pipeline_run_logs"{print $2}' filekey.csv)
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
  echo "Steps: ${STEPS[*]}"
  [ "$RUN_AUDIT" -eq 1 ] && echo "Audit:  yes (${AUDIT_STEP})" || echo "Audit:  no"
  echo "Log:    $LOG"
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
