#!/bin/sh
# t-print-01: a decorated child printed after inline text puts its comment
# on that line; the formatter refuses its own output.
# Run: sh design/review-2026-10-12/repro/t-print-01.sh [path/to/graphix]
# Expected: each of the five programs (all parse) is formatted.
# Observed: each is refused with "formatter bug: the formatted text does
# not parse", at `a if // c`, `until // d`, `abort(// c`, `flush(// c`,
# `select // c` and `source // c` respectively.
G=${1:-graphix}
D=$(mktemp -d)
printf 'let r = select x {\n  a if\n    // c\n    a > 1 => 1,\n  _ => 2\n};\nr\n' > $D/guard.gx
printf 'seq t {\n  a;\n  until\n    // d\n    b;\n  c\n}\n' > $D/until.gx
printf 'seq t; abort(\n  // c\n  e\n) {\n  a;\n  c\n}\n' > $D/abort.gx
printf 'seqq t; flush(\n  // c\n  e\n) {\n  a;\n  c\n}\n' > $D/flush.gx
printf 'select\n  // c\n  x {\n  _ => 2\n}\n' > $D/scrutinee.gx
printf 'mod m dynamic {\n  sandbox unrestricted;\n  sig { val x: i64 };\n  source\n    // c\n    "let x = 1"\n};\nm::x\n' > $D/source.gx
for f in guard until abort flush scrutinee source; do
  echo "== $f"
  timeout -s KILL 30 "$G" fmt --stdout $D/$f.gx 2>&1 | grep -E 'formatter bug|// [cd]$|^ +[a-z].*// [cd]'
done
rm -rf $D
