#!/bin/sh
# typath accepts whitespace (even a newline) after `::` (`m::\n T`, `m::  T`)
# while value paths refuse it; every position consumer (TypeRef's one pos +
# graphix-lsp query.rs::segments, impl trait_at) assumes none, so the
# references for T land on whitespace. Expect 'T' at every site.
D=$(mktemp -d); cp "$(dirname "$0")/t-parser-b-02.gx" "$D/main.gx"
cp "$(dirname "$0")/t-parser-b-02-m.gx" "$D/m.gx"
timeout -s KILL 60 python3 -I "$(dirname "$0")/t-parser-b-lsp.py" "$D" main.gx \
  '[["textDocument/references",4,15]]'
