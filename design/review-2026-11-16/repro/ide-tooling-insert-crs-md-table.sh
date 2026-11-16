#!/bin/bash
# Title: insert_crs.py puts a markdown CR between table rows, which ends the table
#
# Command: bash design/review-2026-11-16/repro/ide-tooling-insert-crs-md-table.sh
#   (a throwaway book in a temp dir: one page with a 3-row table, one CR
#   anchored at the second row, inserted by review/tools/insert_crs.py, then
#   built with mdbook)
#
# Expected: the CR lands above the table (as it does above a code fence),
#   and the page still renders one table of three rows.
# Observed: the `<!-- CR ... -->` line is inserted above `| 3 | 4 |`; an HTML
#   block ends a GFM table, so mdbook renders the rows below it as a
#   paragraph of literal pipes:
#     <p>| 3 | 4 |
#     | 5 | 6 |</p>
set -euo pipefail
REPO="$(cd "$(dirname "$0")/../../.." && pwd)"
T="$(mktemp -d)"
trap 'rm -rf "$T"' EXIT
mkdir -p "$T/src"
printf '[book]\ntitle = "t"\n' > "$T/book.toml"
printf '# Summary\n\n- [P](p.md)\n' > "$T/src/SUMMARY.md"
printf '# P\n\n| a | b |\n|---|---|\n| 1 | 2 |\n| 3 | 4 |\n| 5 | 6 |\n' > "$T/src/p.md"
cat > "$T/crs.json" <<'EOF'
[{"id": "x-01", "file": "src/p.md", "line": 6, "anchor": "| 3 | 4 |", "tag": "doc-drift",
  "sev": "low", "addr": "claude", "text": "this row is wrong (x-01)"}]
EOF
python3 "$REPO/review/tools/insert_crs.py" "$T/crs.json" --root "$T"
cat "$T/src/p.md"
(cd "$T" && mdbook build -d out >/dev/null 2>&1)
grep -A1 '<p>| 3' "$T/out/p.html"
