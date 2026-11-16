#!/bin/bash
# t-format-resolver: format.rs merge_uses_within merges the use statements of a
# seq body and of a try body and handler, statement lists where the check
# refuses every `use` ("a declaration is not a seq step").
#
# Run: bash design/review-2026-11-16/repro/t-format-resolver-seq-use-merge-dead.sh
# Expected (if the Seq/TryWith arms served a valid program): some program with
#   a `use` directly in a seq body, try body or with handler checks.
# Observed (e1be2fbf2): each is refused by --check; fmt still rewrites the
#   first into `seq t { use array::{len, map}; ... }`.
set -u
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
trap 'rm -rf "$D"' EXIT
printf 'let t = 1;\nlet r = seq t { use array::len; use array::map; len(map([1], |x| x)) };\nr\n' > "$D/seq.gx"
printf 'let t = 1;\nlet r = seq t { try { use array::len; len([1]) } with(e) { 0 } };\nr\n' > "$D/try.gx"
printf 'let t = 1;\nlet r = seq t { try { 1; 2 } with(e) { use array::len; len([1]) } };\nr\n' > "$D/with.gx"
for f in seq try with; do
  echo "== $f: --check"
  timeout -s KILL 30 $G --no-cache -n -i --check "$D/$f.gx" 2>&1 | tail -1; echo "rc=${PIPESTATUS[0]}"
done
echo "== fmt of seq.gx"
timeout -s KILL 30 $G fmt --stdout "$D/seq.gx"
