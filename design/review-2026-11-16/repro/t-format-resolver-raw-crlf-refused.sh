#!/bin/bash
# t-format-resolver: format_source's CRLF branch. A raw string whose value holds
# CR LF (written CR CR LF: the parser reads every CRLF as LF) is printed as a
# quoted string and the guard refuses it, though the program checks.
#
# Run: bash design/review-2026-11-16/repro/t-format-resolver-raw-crlf-refused.sh
# Expected: the program is formatted, or, if a raw string cannot hold that
#   value, the refusal says so; and the CRLF loop's comment ("a literal's own
#   [line breaks] already are" CRLF) matches what the parser hands it.
# Observed (e1be2fbf2): --check passes; fmt fails, rc=1, "formatter bug: the
#   formatted text changed a string's delimiters". A raw string spanning a
#   CRLF line (r"a<CR><LF>b") is laid out with LF inside it, which the loop
#   then turns back into CRLF: no literal reaches the loop with its own CRLF.
set -u
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
trap 'rm -rf "$D"' EXIT
printf 'let s = r"a\r\r\nb";\r\nstr::len(s)\r\n' > "$D/crcr.gx"
echo "== --check"
timeout -s KILL 30 $G --no-cache -n -i --check "$D/crcr.gx"; echo "rc=$?"
echo "== fmt"
timeout -s KILL 30 $G fmt --stdout "$D/crcr.gx"; echo "rc=$?"
