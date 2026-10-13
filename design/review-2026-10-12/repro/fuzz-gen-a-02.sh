#!/bin/sh
# fuzz-gen-a-02: oracle_tier matches fire-count markers as substrings
# command: sh fuzz-gen-a-02.sh <path-to-graphix-fuzz>
# The two programs differ only in a lambda's name; neither calls a
# fire-count builtin and neither has `<-`.
# expected: both "AGREE (trace)" (values compared)
# observed: myor -> "EXCLUDED — the oracle compares no values of this program"
#           (the name `myor(` contains the marker `or(`); plusone -> "AGREE (trace)"
FUZZ=${1:-graphix-fuzz}
D=$(mktemp -d)
for name in myor plusone; do
  echo "{ let $name = |n: i64| n + i64:1; let r: i64 = { let q: i64 = json::read(json::write_str(i64:3)\$)\$; q }; $name(r) }" > "$D/$name.gx"
  printf '%s: ' "$name"
  timeout -s KILL 180 "$FUZZ" check "$D/$name.gx" 2>&1 | tail -1
done
rm -rf "$D"
