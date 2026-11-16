#!/bin/sh
# t-print: an impl with no methods prints without `{}`, and only a select
# scrutinee re-adds it (ends_bodyless): a seq trigger or a catch handler that
# ends in one is printed so the seq body / select arms read as the impl's body.
# Command: sh design/review-2026-11-16/repro/t-print-bodyless-impl-trigger.sh
# Expected: each source formats (it parses), e.g.
#   let r = seq a ~ |x| impl Foo for i64 {} { 1 };
# Observed: every line is refused:
#   Error: formatter bug: the formatted text does not parse: ...
#       let r = seq a ~ |x| impl Foo for i64 { 1 };
#   note: an impl body holds only `let name = ..` methods
# (the documented form `impl Cast for T {};` is also rewritten to
# `impl Cast for T;` by the same printer path)
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
for s in \
  'let r = seq a ~ |x| impl Foo for i64 {} { 1 };' \
  'let r = seq let p = a ~ |x| impl Foo for i64 {} { 1 };' \
  'let r = select catch(e) impl Foo for i64 {} { _ => 1 };' \
  ; do
  printf '%s\nr\n' "$s" | timeout -s KILL 10 "$G" fmt --stdout 2>&1 | head -3
  echo "--"
done
printf 'impl Cast for Port {};\n1\n' | timeout -s KILL 10 "$G" fmt --stdout
