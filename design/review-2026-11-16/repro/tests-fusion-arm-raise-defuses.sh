#!/bin/sh
# Title: a select arm over an always-error operand (`error(..)$`,
#   `error(..)?`, or a `never(error(..)?)` arm) does not fuse: the `$`/`?`
#   result type stays an open type variable (Qop::typecheck0_with gives it
#   only the lower bound ⊥), so the region "has no kernel return" or its
#   discarded never-arg "doesn't classify". strict_fusion.md says these
#   fuse (emit_qop_always_bad; a never(..) arm's args are emitted and
#   consumed). lang/fusion.rs NEVER_ARM_ARGS_RAISE / ARM_RAISE_STANDING_INPUT
#   (FuseExpect::None) and bound_definitions_raise_to_their_own_handler
#   never run a kernel for their raise because of it.
# Command: sh tests-fusion-arm-raise-defuses.sh
# Expected: every #[native] below is accepted and prints 1, 3.
# Observed (HEAD e1be2fbf2):
#   d: refused "a region of type '_N: unbound has no kernel return"
#   a: refused "a region of type _ has no kernel return"
#      (graphix-fuzz run: "discarded result of type '_N doesn't classify")
#   b: refused "a region of type '_N: unbound has no kernel return"
#   c: (b with `let w: i64 =`) fuses and prints 1, 3; with the fixture's
#      catch and counter `n` it gives 0 / 1 in both engines (fuzz AGREE).
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
cat > $D/d.gx <<'EOF'
let x = array::iter([1, 2, 3]);
let w = #[native] select x { 2 => error(`E(x))$, _ => x };
w
EOF
cat > $D/a.gx <<'EOF'
let x = array::iter([1, 2, 3]);
let w = #[native] select x { 2 => never(error(`E(x))$), _ => x };
w
EOF
cat > $D/b.gx <<'EOF'
let x = array::iter([1, 2, 3]);
let v = { catch(e) println("[e]"); #[native] select x { 2 => error(`E(x))?, _ => x } };
v
EOF
cat > $D/c.gx <<'EOF'
let x = array::iter([1, 2, 3]);
let v = { catch(e) println("[e]"); let w: i64 = #[native] select x { 2 => error(`E(x))?, _ => x }; w };
v
EOF
for f in d a b c; do
  echo "== $f"
  timeout -s KILL 10 $G --no-cache $D/$f.gx 2>&1 | grep -v WARNING | head -6
done
rm -rf $D
