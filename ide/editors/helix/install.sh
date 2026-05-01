#!/usr/bin/env bash
# Install Graphix support into the user's Helix configuration.
#
# This script:
#   1. Copies tree-sitter queries into $XDG_CONFIG_HOME/helix/runtime/queries/graphix/
#   2. Appends the graphix [[language]], [language-server.*], and [[grammar]]
#      blocks to $XDG_CONFIG_HOME/helix/languages.toml (idempotent — skips
#      if a graphix language entry already exists).
#   3. Runs `helix --grammar fetch` and `helix --grammar build`.
#
# Helix has no plugin system, so this is the closest thing to a one-shot
# install. Re-running is safe.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CONFIG_DIR="${XDG_CONFIG_HOME:-$HOME/.config}/helix"
QUERIES_DST="$CONFIG_DIR/runtime/queries/graphix"
LANGUAGES_TOML="$CONFIG_DIR/languages.toml"

mkdir -p "$QUERIES_DST"
cp "$SCRIPT_DIR/queries/"*.scm "$QUERIES_DST/"
echo "✓ queries → $QUERIES_DST"

touch "$LANGUAGES_TOML"
if grep -qE '^\s*name\s*=\s*"graphix"' "$LANGUAGES_TOML"; then
    echo "• languages.toml already has a graphix entry — leaving it alone"
else
    {
        echo ""
        cat "$SCRIPT_DIR/languages.toml"
    } >> "$LANGUAGES_TOML"
    echo "✓ appended graphix blocks → $LANGUAGES_TOML"
    echo "  (edit the [[grammar]] block to point at the right source)"
fi

if ! command -v helix >/dev/null 2>&1; then
    echo "✗ 'helix' not on PATH — skipping grammar build"
    exit 1
fi

if ! command -v graphix >/dev/null 2>&1; then
    echo "! 'graphix' not on PATH — install it with: cargo install --path graphix-shell"
fi

echo "→ helix --grammar fetch"
helix --grammar fetch || true
echo "→ helix --grammar build"
helix --grammar build

echo ""
echo "Done. Open a .gx file in Helix and run :tree-sitter-scopes to verify."
