#!/bin/sh
# fuzz-mutate-04: ref-widen annotates `&mut Any` over a let-over-bottom referent,
# which the annotation then decides (a false must-reject LEAK).
# The base is written without a comment: a commented subject yields no ref-widen mutant.
# command: sh fuzz-mutate-04.sh   (GRAPHIX_FUZZ = path to the graphix-fuzz binary)
# expected: "typemorph: no flips" for both bases; the mutant
#   { let x = never(); let r: &mut Any = &mut x; *r <- 2; x }
#   is rightly accepted (`graphix --check` exits 0: x's cell is decided Any)
# observed: TYPEFLIP base.gx ref-widen#1: LEAK: accepted at let
#   control (let x = 1): typemorph: no flips (that mutant is refused)
F=${GRAPHIX_FUZZ:-graphix-fuzz}
D=$(mktemp -d)
echo '{ let x = never(); let r = &mut x; *r <- 2; x }' > "$D/base.gx"
echo '{ let x = 1; let r = &mut x; *r <- 2; x }' > "$D/control.gx"
(cd "$D" && timeout -s KILL 170 "$F" typemorph base.gx; timeout -s KILL 170 "$F" typemorph control.gx)
rm -rf "$D"
