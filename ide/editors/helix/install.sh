#!/usr/bin/env bash
# Install Graphix support into the user's Helix configuration.
#
# This script:
#   1. Links (or with --copy, copies) the tree-sitter queries into
#      $XDG_CONFIG_HOME/helix/runtime/queries/graphix/
#   2. Appends the graphix [[language]], [language-server.*], and [[grammar]]
#      blocks to $XDG_CONFIG_HOME/helix/languages.toml (idempotent — skips
#      if a graphix language entry already exists).
#   3. Runs `helix --grammar fetch` and `helix --grammar build`.
#
# Helix has no plugin system, so this is the closest thing to a one-shot
# install. Re-running is safe.

set -euo pipefail

link_queries=1
for arg in "$@"; do
    case "$arg" in
        --copy) link_queries=0 ;;
        -h|--help)
            sed -n '2,14p' "$0" | sed 's/^# \?//'
            echo
            echo "Options:"
            echo "  --copy   copy the queries instead of linking them (use when"
            echo "           this checkout is temporary — a copy does not track"
            echo "           later changes to the grammar)"
            exit 0
            ;;
        *) echo "unknown argument: $arg (try --help)" >&2; exit 2 ;;
    esac
done

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
QUERIES_SRC="$(cd "$SCRIPT_DIR/../../tree-sitter-graphix/queries" && pwd -P)"
CONFIG_DIR="${XDG_CONFIG_HOME:-$HOME/.config}/helix"
QUERIES_DST="$CONFIG_DIR/runtime/queries/graphix"
LANGUAGES_TOML="$CONFIG_DIR/languages.toml"

# The queries are LINKED by default. A copy is a snapshot of a moving
# target: the queries name grammar nodes, so the next syntax change
# leaves them referring to nodes the rebuilt grammar no longer has —
# and tree-sitter refuses a whole query file over one stale name, which
# helix shows as no syntax coloring at all. `helix --health graphix`
# does not catch it (it reports ✓ for a query that fails to compile),
# so nothing would tell you but the colors going away.
mkdir -p "$QUERIES_DST"
for src in "$QUERIES_SRC"/*.scm; do
    dst="$QUERIES_DST/$(basename "$src")"
    rm -f "$dst"
    if [ "$link_queries" = 1 ]; then ln -s "$src" "$dst"; else cp "$src" "$dst"; fi
done
if [ "$link_queries" = 1 ]; then
    echo "✓ queries linked → $QUERIES_DST"
else
    echo "✓ queries copied → $QUERIES_DST"
fi

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
if [ "$link_queries" = 1 ]; then
    echo "The queries track this checkout, but the COMPILED grammar does not:"
    echo "re-run this script after pulling a change to the grammar."
fi
