#!/usr/bin/env bash
# x-rerun-8-02: `graphix-fuzz check|run|minimize <file>` reads the file raw;
# Subject::parse (graphix-fuzz/src/lib.rs:379) wraps it as
# `use super::*; let result = {<body>` on ONE line, so a file that opens with
# a `//` header (532 of the 547 pins under graphix-fuzz/findings) puts the
# comment after `{` on the same line, which the parser refuses since the
# own-line comment rule. Every mode then fails to compile alike and `check`
# prints AGREE (reject): a vacuous pass. `regress` is unaffected only because
# build.rs strip_header strips the header before embedding the corpus.
# command: FUZZ=/path/to/graphix-fuzz bash x-rerun-8-02.sh   (from the repo root)
# expected: the pin's manifest verdict, `trace` (value 2)
# observed: AGREE (reject); `run` shows CompileErr "a comment stands on a line of its own"
set -u
F=${FUZZ:-graphix-fuzz}
P=graphix-fuzz/findings/abstract-ord-mixed-types-sep2026/00_map_key_union.gx
grep -F "$(basename $P)" graphix-fuzz/outcome.manifest
timeout -s KILL 170 "$F" check "$P" 2>&1 | head -2
timeout -s KILL 170 "$F" run "$P" 2>&1 | head -1 | cut -c1-200
d=$(mktemp -d); trap 'rm -rf "$d"' EXIT
grep -v '^//' "$P" > "$d/stripped.gx"
echo "header stripped:"; timeout -s KILL 170 "$F" run "$d/stripped.gx" 2>&1 | head -1 | cut -c1-200
