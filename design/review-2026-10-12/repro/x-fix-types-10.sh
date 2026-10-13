#!/bin/sh
# x-fix-types-10: ErrChain collapse and chain tests decide "is a chain" by field names alone
# (graphix-types Type::chain_type, compiler is_echain_shape / is_chain_type, run-time chain_error).
# Two programs, one per check-side copy:
#   a) fix_echain_typ keeps Error<P> at a ? over a lookalike alias P (is_chain_type);
#   b) a generic `?` throws ErrChain<'e>; at 'e = the lookalike struct, lookup_ref collapses
#      ErrChain<'e> to the struct itself (chain_type).
# In both, wrap_error nests the raised value as `cause` at run time, so `(e.0).cause` typed i64
# holds the original struct.
# Run: sh x-fix-types-10.sh /path/to/graphix
# Expected: --check refuses `f((e.0).cause)` (cause is [ErrChain<..>, null]), or the
#   value matches its type.
# Observed (HEAD 7324207f, debug build): --check exits 0 for both; the JIT panics at
#   fusion/kernel.rs:218 "kernel param `x`: runtime Array(...) does not match the compiled
#   Scalar(I64) slot" / "runtime did not respond"; --no-fusion prints an Array of parse errors
#   as the value of an i64 binding.
G=${1:-graphix}
D=$(mktemp -d)
cat > "$D/a.gx" <<'GX'
type P = {cause: i64, error: string, ori: bool, pos: null};
let r: Result<i64, P> = error({cause: 5, error: "e", ori: true, pos: null});
let out = never();
let f = |x: i64| -> i64 (x + 1);
{
  catch(e) out <- f((e.0).cause);
  r?
};
sys::exit(sys::time::after_idle(duration:100.ms, 0));
out
GX
cat > "$D/b.gx" <<'GX'
let r: Result<i64, {cause: i64, error: string, ori: bool, pos: null}> = error({cause: 5, error: "e", ori: true, pos: null});
let h = |r: Result<i64, 'e>| r?;
let out = never();
let f = |x: i64| -> i64 (x + 1);
{
  catch(e) out <- f((e.0).cause);
  h(r)
};
sys::exit(sys::time::after_idle(duration:100.ms, 0));
out
GX
for p in a b; do
  echo "== $p"
  timeout -s KILL 30 "$G" --check "$D/$p.gx"; echo "check exit $?"
  timeout -s KILL 30 "$G" --no-cache "$D/$p.gx" 2>&1 | cut -c1-200 | tail -3
  timeout -s KILL 30 "$G" --no-cache --no-fusion "$D/$p.gx" 2>&1 | cut -c1-200 | tail -2
done
rm -rf "$D"
