#!/bin/sh
# x-engine-seq-errors-03: a catch's bind type depends on the scope an
# ErrChain annotation was written in (TypeRef scope compared, not the
# definition it resolves to: setops.rs union_inner / same_inner / same_ref).
# command: sh x-engine-seq-errors-03.sh [path/to/graphix]
# expected: every case accepted; the handler's bind is Error<ErrChain<[`A, `B]>>
# observed: "block", "lambda" and "module" are refused with
#   expected struct not [{ cause: [null, ErrChain<`A>], error: `A, ori: Ori, pos: Pos },
#                        { cause: [null, ErrChain<`B>], error: `B, ori: Ori, pos: Pos }]
#   while "top" and "alias" (the same ErrChain ref, written at the root) are accepted.
G=${1:-graphix}
D=$(mktemp -d)
trap 'rm -rf "$D"' EXIT
H='catch(e) println(select (e.0).error { `A => "A", `B => "B" });'
cat > "$D/top.gx" <<GX
let saved: Error<ErrChain<\`A>> = never();
let r = { $H saved?; error(\`B)? };
r
GX
cat > "$D/block.gx" <<GX
{
  let saved: Error<ErrChain<\`A>> = never();
  let r = { $H saved?; error(\`B)? };
  r
}
GX
cat > "$D/alias.gx" <<GX
type EA = Error<ErrChain<\`A>>;
{
  let saved: EA = never();
  let r = { $H saved?; error(\`B)? };
  r
}
GX
# a let in a lambda body (a lambda PARAMETER annotation merges)
cat > "$D/lambda.gx" <<GX
let f = |x: i64| {
  let saved: Error<ErrChain<\`A>> = never();
  let r = { $H saved?; error(\`B)? };
  r + x
};
f(1)
GX
# the common shape: a module function returns its caught error, the caller re-raises it
cat > "$D/m.gx" <<GX
let g = |x: i64| -> Error<ErrChain<\`A>> { catch(e) e; error(\`A)? }
GX
cat > "$D/module.gx" <<GX
mod m;
let saved = m::g(1);
let r = { $H saved?; error(\`B)? };
r
GX
for f in top alias block lambda module; do
  if out=$(cd "$D" && timeout -s KILL 60 "$G" --check $f.gx 2>&1); then
    echo "$f: accepted"
  else
    echo "$f: REFUSED: $(echo "$out" | tail -1 | sed 's/^ *//')"
  fi
done
