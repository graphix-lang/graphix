#!/bin/bash
# Title: open_init resolves a name by the top-level let of that name, past an arm bind that shadows it
# Command: bash design/review-2026-11-16/repro/fuzz-mutate-open-init-shadowed-name.sh
# Expected: "typemorph: no flips" for both bases: the name each family reads is an arm's
#   bind over the open `n`, which open_init should call open (and does, without the
#   same-named top-level let).
# Observed (graphix-fuzz at e1be2fbf2):
#   1) variant-widen#8: WRONG REFUSAL: N: `A tests a value of 'n, a type not decided at this
#      select, ... annotate it, writing for 'n the type of `n` ...
#   2) retype#5: MISPLACED: N: cannot compute string + iN ...
#      mutant: { let x = 1; let n = never(); let v = select n { x => x }; v <- "tm"; let w = n + 1; (v, w, x) }
# mustreject::earlier_let looks the name up among the root block's lets before the statement,
# whatever binds it in between (a select arm, a lambda parameter, an inner let).
F=/home/eric/tmp/review-2026-11-16/bin/graphix-fuzz
d=$(mktemp -d)
i=0
for p in \
  '{ let x: [`A, `B] = `A; let n = never(); let r = select n { x => select x { `A => 1, `B => 2 } }; (r, x) }' \
  '{ let x = 1; let n = never(); let v = select n { x => x }; let w = n + 1; (v, w, x) }'
do
  i=$((i+1)); printf '%s\n' "$p" > "$d/b$i.gx"
  echo "== $p"
  timeout -s KILL 170 "$F" typemorph "$d/b$i.gx" 2>&1 | grep -E "TYPEFLIP|mutant|no flips"
done
rm -rf "$d"
