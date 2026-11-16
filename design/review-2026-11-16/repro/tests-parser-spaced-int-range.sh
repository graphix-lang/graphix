#!/bin/sh
# tests-parser-spaced-int-range: a typed integer written with a space after
# its colon is a literal (`u8: 255` checks and formats as `u8:255`), but out
# of range it says the value parser's "invalid unsigned integer" with no
# range note, where the tight form names the range.
# an_out_of_range_integer_literal_names_its_range asserts the note is absent.
# Command: sh design/review-2026-11-16/repro/tests-parser-spaced-int-range.sh
# Expected: `u8: 256` notes "`256` is out of the range of u8" as `u8:256` does.
# Observed: `u8: 255` checks; `u8: 256` says "invalid unsigned integer", no note.
G=/home/eric/tmp/review-2026-11-16/bin/graphix
D=$(mktemp -d)
for src in 'let x = u8: 255; x' 'let x = u8:256; x' 'let x = u8: 256; x'; do
    printf '%s' "$src" > "$D/t.gx"
    echo "== $src"
    timeout -s KILL 30 "$G" --check --no-cache "$D/t.gx" 2>&1 | sed -n '5,10p'
done
rm -rf "$D"
