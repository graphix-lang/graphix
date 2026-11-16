#!/bin/sh
# Title: fundamental_types.md "Lists as Data" says pack writes a list as a flat array; pack keeps list cells
# Command: sh design/review-2026-11-16/repro/book-core-pack-list-cells.sh
# Expected (the book): the two pack::write_bytes results are the same bytes, so the comparison prints true.
# Observed: false; the list's bytes are its marked cells (pack.gxi: "preserving tuple, struct,
#   variant and list kinds"; design/list_native.md: "Pack, db values and session images preserve
#   marked cells"), while json::write_str([<1, 2, 3>]) is "[1,2,3]".
G=/home/eric/tmp/review-2026-11-16/bin/graphix
printf 'pack::write_bytes([<1, 2, 3>]) == pack::write_bytes([1, 2, 3])\npack::write_bytes([<1, 2, 3>])\npack::write_bytes([1, 2, 3])\njson::write_str([<1, 2, 3>])\n' \
  | timeout -s KILL 30 $G --no-cache 2>&1 | grep -v '^Welcome\|^Press'
