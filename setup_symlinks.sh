#!/usr/bin/env bash
#
# setup_symlinks.sh
#
# Creates a symlink from this repo to the Box-synced Kings_Large_Files folder.
# Run once after cloning (or whenever the symlink needs to be recreated).
#
# Usage:
#   bash setup_symlinks.sh
#   bash setup_symlinks.sh /path/to/box/Kings_Large_Files   # override auto-detection
#

set -euo pipefail

REPO_DIR="$(cd "$(dirname "$0")" && pwd)"

# --- Detect or accept the Box Kings_Large_Files root --------------------------------

if [[ -n "${1:-}" ]]; then
    BOX_KINGS="$1"
else
    # Try common Box mount names on macOS
    BOX_BASE="$HOME/Library/CloudStorage"
    if [[ -d "$BOX_BASE/Box-Box/Kings_Large_Files" ]]; then
        BOX_KINGS="$BOX_BASE/Box-Box/Kings_Large_Files"
    elif [[ -d "$BOX_BASE/Box/Kings_Large_Files" ]]; then
        BOX_KINGS="$BOX_BASE/Box/Kings_Large_Files"
    elif [[ -d "$HOME/Box/Kings_Large_Files" ]]; then
        BOX_KINGS="$HOME/Box/Kings_Large_Files"
    else
        echo "ERROR: Could not find Kings_Large_Files in Box."
        echo "Searched:"
        echo "  $BOX_BASE/Box-Box/Kings_Large_Files"
        echo "  $BOX_BASE/Box/Kings_Large_Files"
        echo "  $HOME/Box/Kings_Large_Files"
        echo ""
        echo "Re-run with an explicit path:"
        echo "  bash setup_symlinks.sh /path/to/box/Kings_Large_Files"
        exit 1
    fi
fi

echo "Using Box Kings_Large_Files at: $BOX_KINGS"

# --- Create symlink -----------------------------------------------------------

LINK_PATH="$REPO_DIR/Kings_Large_Files"

# Remove existing symlink or warn if something else is in the way
if [[ -L "$LINK_PATH" ]]; then
    rm "$LINK_PATH"
elif [[ -e "$LINK_PATH" ]]; then
    echo "WARNING: $LINK_PATH exists and is not a symlink — skipping."
    exit 1
fi

if [[ -d "$BOX_KINGS" ]]; then
    ln -s "$BOX_KINGS" "$LINK_PATH"
    echo "  OK  $LINK_PATH -> $BOX_KINGS"
else
    echo "  MISSING  $BOX_KINGS  (symlink not created)"
    exit 1
fi

echo ""
echo "Done. Symlink created."
