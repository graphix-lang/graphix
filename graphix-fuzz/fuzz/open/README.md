# Open divergences (triaged, not yet fixed)

Confirmed interp/jit divergences with a named root cause and a minimal
witness, kept OUT of `findings/` because they still diverge — `regress`
must stay green. Each names what it would take to close.

(01, the recursive-activation cache, is FIXED — per-activation block
trees, `findings/recursive-activation-blocks-aug2026`.)

Run one: `graphix-fuzz check graphix-fuzz/fuzz/open/<file>.gx`

## a newly-resolved deref fires one cycle late

WAS 02.

From aug15b aieka reactive 000000. INTERP-side.

`Deref::update` registers its wake interest only once the reference
VALUE arrives. On the cycle the reference first arrives, the read of the
target comes back Standing, so the deref produces STALE — with the right
value, but a tag that does not fire. The delivery shows up as Delivered
one cycle later, so the fire lands a cycle late and, in the witness, the
`<-` that consumed it never wrote.

Under organic firing the rule is not in doubt: the reference expression
is a consumed input, it fired, so the deref must fire. The fix is to
treat a fired child as making the read fresh. What needs care before
landing it is PACING — firing a cycle earlier moves the interp relative
to the JIT, and the oracle compares per-cycle traces, so the JIT's own
first-fire timing has to be checked against it rather than assumed.

## a bottomed string dependency rides in the interp, misses in the JIT

WAS 03.

From aug15b hz0 reactive 000000.

`select in0 { _ if str::contains("a", v0) => str::len(v0), _ => 0 }`
with `v0` a STANDING BOTTOM: the interp assembles a value out of its
designated rides (the guard's `Held` re-matches, and the arm's own
op-site cache serves the last `str::len`), and emits. The kernel's guard
ride is implemented (`emit_scalar_taint_cache` on the bool), so the arm
is taken — but the arm's value bottoms and nothing rides it there.

Scalar-typed twins AGREE (swap the string for an i64, or read the guard
off a scalar derived from the string), which places this squarely in the
"value/composite residents ride at region root only" family and makes it
a sibling of the honor-header bug rather than a new genus: a site whose
cache cannot ride must de-fuse, not pass through.
