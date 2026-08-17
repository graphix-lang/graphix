# Open divergences (triaged, not yet fixed)

Confirmed interp/jit divergences with a named root cause and a minimal
witness, kept OUT of `findings/` because they still diverge — `regress`
must stay green. Each names what it would take to close.

(The other two aug15b items are FIXED: the recursive-activation cache
became per-activation block trees
(`findings/recursive-activation-blocks-aug2026`), and the late deref was
a chain the READ path never followed
(`findings/deref-reads-the-referent-aug2026`).)

Run one: `graphix-fuzz check graphix-fuzz/fuzz/open/<file>.gx`

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
