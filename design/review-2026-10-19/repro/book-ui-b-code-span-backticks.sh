#!/bin/sh
# Inline code spans whose content holds a backtick break when rendered.
# CommonMark processes no backslash escapes inside a code span, and a
# single-backtick span ends at the next backtick, so:
#   book/src/ui/tui/input.md   `mode <- \`Insert`  renders "mode <- \" + "Insert`"
#   book/src/ui/gui/text.md    `#width: &`Fill``   renders "#width: &" + "Fill``"
# Run from the repo root (docs/book is the mdbook build of book/):
#   sh design/review-2026-10-19/repro/book-ui-b-code-span-backticks.sh
# Expected: no output (both spans render as code).
# Observed (2026-10-19):
#   such as <code>mode &lt;- \</code>Insert` fires once ...
#   with <code>#width: &amp;</code>Fill``).
grep -o 'such as <code>mode[^<]*</code>[^ ]* fires once' docs/book/ui/tui/input.html
grep -o 'with <code>#width[^<]*</code>Fill[^<]*' docs/book/ui/gui/text.html
