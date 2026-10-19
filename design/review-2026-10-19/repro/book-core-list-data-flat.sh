#!/bin/bash
# Title: fundamental_types.md "Lists as Data" describes list data from before kind markers
# Command: bash design/review-2026-10-19/repro/book-core-list-data-flat.sh
# Expected (the book): pack carries a list as its chain of cells, so pack bytes of [<1, 2, 3>]
#   differ from those of [1, 2, 3]; cast<List<..>> of an Any holding a flat array takes only
#   "a well-formed chain", so the book casts to Array<Array<i64>> first.
# Observed: identical pack bytes for the list and the array (data carries no markers), and
#   cast<List<Array<i64>>> of an Any holding [[1, 2], [3, 4]] reads [<[1, 2], [3, 4]>] directly.
G=/home/eric/tmp/review-2026-10-19/bin/graphix
run() { printf "$1" | timeout -s KILL 4 $G --no-cache 2>&1 | grep -v '^Welcome\|^Press\|^$'; }
run 'pack::write_bytes([<1, 2, 3>])$\n'
run 'pack::write_bytes([1, 2, 3])$\n'
run 'let v: Any = [[1, 2], [3, 4]]\ncast<List<Array<i64>>>(v)\n'
