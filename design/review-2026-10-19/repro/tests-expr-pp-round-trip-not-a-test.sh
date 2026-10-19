#!/bin/sh
# Title: expr_pp_round_trip lost its #[test] (graphix-types/src/expr/test.rs): the
#        pretty-printer round-trip property never runs, and
#        printer_parenthesizes_unparenthesized_trees is registered twice.
# Command: sh design/review-2026-10-19/repro/tests-expr-pp-round-trip-not-a-test.sh
#          (lists the tests of the newest graphix-types unit-test binary in ~/tmp/target)
# Expected: expr::test::expr_pp_round_trip listed once, every other test once.
# Observed (binary built 2026-10-19 from HEAD 9445a1d21):
#   expr::test::expr_round_trip: test
#   expr::test::pack_round_trip: test
#   expr::test::printer_parenthesizes_unparenthesized_trees: test
#   expr::test::printer_parenthesizes_unparenthesized_trees: test
#   expr::test::sig_pack_round_trip: test
#   (no expr_pp_round_trip)
B=$(ls -t "$HOME"/tmp/target/debug/deps/graphix_types-* | grep -v '\.' | head -1)
timeout -s KILL 30 "$B" --list 2>/dev/null | grep -E '^expr::test::(expr_|pack_|printer_|sig_)'
