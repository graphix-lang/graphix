#!/bin/sh
# Title: --check time doubles with each nesting level of a constructor pattern
#        over a recursive union two of whose members have the pattern's shape,
#        in a select arm and in a parameter.
#
# Command: sh design/review-2026-11-16/repro/c-pattern-nested-pattern-check-exponential.sh
#
# D is the pattern's depth. The select program:
#   type T = [(T, i64), ([T, null], string), null];
#   let f = |x: T| select x { ((((v, _), _), _), _) => 1, _ => 0 };
#   0
# The parameter program (refused as refutable, after the time is spent):
#   type T = [(T, i64), (U, string)];
#   type U = [(T, bool), (U, f64)];
#   let f = |((((v, _), _), _), _): T| 0;
#   0
# Expected: the check's time grows with the pattern's size (D names).
# Observed (debug binary, one run each, a loaded machine):
#   select: D=10 ~0.1 s, D=14 ~0.55 s, D=16 ~2 s, D=18 ~7 s, D=20 ~30 s
#   param:  D=10 ~0.1 s, D=12 ~0.27 s, D=14 ~0.9 s, D=16 ~3.5 s, D=18 ~14 s
# Stack samples (gdb, the real binary):
#   select, D=20: Type::narrow_pattern_in -> narrow_pattern_inner ->
#     narrow_per_member_in -> narrow_member -> narrow_parts, D frames deep,
#     under Select::typecheck0_with; later StructurePattern::meets under
#     Select::check_undecided.
#   param, D=18: the same narrowing at 3 s; at 7 s and at 11 s
#     StructPatternNode::leaves -> leaves_inner -> admits -> footprint ->
#     member_footprint, under StructPatternNode::names_by_member.
G=/home/eric/tmp/review-2026-11-16/bin/graphix
d=$(mktemp -d)
for kind in select param; do
  for D in 10 14 16; do
    p="v"; i=0
    while [ $i -lt $D ]; do p="($p, _)"; i=$((i+1)); done
    case $kind in
      select) printf 'type T = [(T, i64), ([T, null], string), null];\nlet f = |x: T| select x { %s => 1, _ => 0 };\n0\n' "$p" > "$d/p.gx" ;;
      param) printf 'type T = [(T, i64), (U, string)];\ntype U = [(T, bool), (U, f64)];\nlet f = |%s: T| 0;\n0\n' "$p" > "$d/p.gx" ;;
    esac
    s=$(date +%s%N)
    timeout -s KILL 120 "$G" --no-cache --check "$d/p.gx" > /dev/null 2>&1
    rc=$?
    e=$(date +%s%N)
    echo "$kind D=$D rc=$rc ms=$(( (e - s) / 1000000 ))"
  done
done
rm -rf "$d"
