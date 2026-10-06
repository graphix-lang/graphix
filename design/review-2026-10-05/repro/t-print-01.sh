#!/usr/bin/env bash
# t-print-01: a `?` that raises NullError inside a seq step names the seq
# rewrite (seqpc<ExprId> ~! x, seqqcap.., the issued-call lowering), not
# the operand as written; the ExprId comes from the process-global
# counter, so the program-visible text differs between --no-cache and a
# registration-warm start, and from run to run in a multi-module program.
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/t-print-01.sh
#
# expected (book/src/core/error.md: the string is the expression that was
# null; lang::errors::qop_null_raises pins "x" outside a seq):
#   1. "x was null"            2. "`NullError(\"x\")" (control, no seq)
#   3. "x was null"            4. every element "x was null", every run
#   5. "`NullError(\"x\")"     6. "`NullError(\"f(x)\")"
# observed (HEAD c722befe, debug build, both engines, --no-fusion too):
#   1. "NullError names seqpc4611686018427394755 ~! x"
#   2. "`NullError(\"x\")"
#   3. "NullError names seqpc4611686018427393600 ~! x"   (same program as 1)
#   4. a::r names seqpc...394802, then ...394844, then ...394805: each
#      module's id changes from run to run (the numbers differ every time)
#   5. "`NullError(\"seqpc4611686018427394750 ~! seqqcap4611686018427394750_0\")"
#   6. "`NullError(\"{ let seqargs... = (seqpc..., x); select any(seqpc...,
#      core::once(seqargs...)) ~! seqargs... { seqissued... => f(seqissued....1) } }\")"
# graphix-fuzz check on 1 reports AGREE: the engines share null_error, and
# a single file's ids are stable within one cache state.
set -u
GRAPHIX=${GRAPHIX:-graphix}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
export XDG_CACHE_HOME="$dir/cache"
run() { timeout -s KILL 60 "$GRAPHIX" "$@" | tr '\n' ' '; echo; }

cat > "$dir/one.gx" <<'EOF'
let x: [i64, null] = null;
let go = sys::time::timer(duration:10.ms, false);
let r = seq go {
  let v = try { let y = x?; "[y]" } with(e) {
    select (e.0).error {
      `NullError("x") => "x was null",
      `NullError(s) => "NullError names [s]"
    }
  };
  v
};
sys::exit(sys::time::after_idle(duration:200.ms, 0));
r
EOF
cat > "$dir/control.gx" <<'EOF'
let x: [i64, null] = null;
let out = never();
let r = {
  catch(e) out <- "[(e.0).error]";
  let y = x?;
  y
};
sys::exit(sys::time::after_idle(duration:200.ms, 0));
out
EOF
cat > "$dir/seqq.gx" <<'EOF'
let x: [i64, null] = null;
let go = sys::time::timer(duration:10.ms, false);
let r = seqq go {
  let v = try { let y = x?; "[y]" } with(e) { "[(e.0).error]" };
  v
};
sys::exit(sys::time::after_idle(duration:200.ms, 0));
r
EOF
cat > "$dir/call.gx" <<'EOF'
let x: [i64, null] = null;
let f = |a: [i64, null]| a;
let go = sys::time::timer(duration:10.ms, false);
let r = seq go {
  let v = try { let y = f(x)?; "[y]" } with(e) { "[(e.0).error]" };
  v
};
sys::exit(sys::time::after_idle(duration:200.ms, 0));
r
EOF
mkdir -p "$dir/mm"
for m in a b c d; do
  sed -e '/^sys::exit/d' -e '/^r$/d' "$dir/one.gx" > "$dir/mm/$m.gx"
  printf 'val r: string;\n' > "$dir/mm/$m.gxi"
done
cat > "$dir/mm/main.gx" <<'EOF'
mod a;
mod b;
mod c;
mod d;
sys::exit(sys::time::after_idle(duration:200.ms, 0));
(a::r, b::r, c::r, d::r)
EOF

echo "1. seq, --no-cache:";            run --no-cache "$dir/one.gx"
echo "2. no seq (control):";           run --no-cache "$dir/control.gx"
# warm the registration entry with another program, then compile one.gx over it
run "$dir/control.gx" > /dev/null
echo "3. seq, registration warm:";     run "$dir/one.gx"
echo "4. four modules, three runs:"
for i in 1 2 3; do run --no-cache "$dir/mm/main.gx"; done
echo "5. seqq, --no-cache:";           run --no-cache "$dir/seqq.gx"
echo "6. call operand, --no-cache:";   run --no-cache "$dir/call.gx"
