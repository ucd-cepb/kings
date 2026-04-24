#!/usr/bin/env bash
#
# setup_symlinks.sh
#
# Creates a symlink from this repo to the Box-synced Kings_Large_Files/data
# folder (where large files are streamed). Run once after cloning (or whenever
# the symlink needs to be recreated).
#
# Usage:
#   bash setup_symlinks.sh
#   bash setup_symlinks.sh /path/to/box/Kings_Large_Files/data   # override auto-detection
#

set -euo pipefail

REPO_DIR="$(cd "$(dirname "$0")" && pwd)"

# --- Detect or accept the Box Kings_Large_Files/data root --------------------

if [[ -n "${1:-}" ]]; then
    BOX_KINGS_DATA="$1"
else
    # Try common Box mount names on macOS
    BOX_BASE="$HOME/Library/CloudStorage"
    if [[ -d "$BOX_BASE/Box-Box/Kings_Large_Files/data" ]]; then
        BOX_KINGS_DATA="$BOX_BASE/Box-Box/Kings_Large_Files/data"
    elif [[ -d "$BOX_BASE/Box/Kings_Large_Files/data" ]]; then
        BOX_KINGS_DATA="$BOX_BASE/Box/Kings_Large_Files/data"
    elif [[ -d "$HOME/Box/Kings_Large_Files/data" ]]; then
        BOX_KINGS_DATA="$HOME/Box/Kings_Large_Files/data"
    else
        echo "ERROR: Could not find Kings_Large_Files/data in Box."
        echo "Searched:"
        echo "  $BOX_BASE/Box-Box/Kings_Large_Files/data"
        echo "  $BOX_BASE/Box/Kings_Large_Files/data"
        echo "  $HOME/Box/Kings_Large_Files/data"
        echo ""
        echo "Re-run with an explicit path:"
        echo "  bash setup_symlinks.sh /path/to/box/Kings_Large_Files/data"
        exit 1
    fi
fi

echo "Using Box Kings_Large_Files/data at: $BOX_KINGS_DATA"

# --- Create symlink -----------------------------------------------------------

LINK_PATH="$REPO_DIR/data"

# Remove existing symlink or warn if something else is in the way
if [[ -L "$LINK_PATH" ]]; then
    rm "$LINK_PATH"
elif [[ -e "$LINK_PATH" ]]; then
    echo "WARNING: $LINK_PATH exists and is not a symlink — skipping."
    exit 1
fi

if [[ -d "$BOX_KINGS_DATA" ]]; then
    ln -s "$BOX_KINGS_DATA" "$LINK_PATH"
    echo "  OK  $LINK_PATH -> $BOX_KINGS_DATA"
else
    echo "  MISSING  $BOX_KINGS_DATA  (symlink not created)"
    exit 1
fi

echo ""
echo "Done. Symlink created."
