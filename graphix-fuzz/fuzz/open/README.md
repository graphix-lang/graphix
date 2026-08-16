# Open divergences (triaged, not yet fixed)

Confirmed interp/jit divergences with a named root cause and a minimal
witness, kept OUT of `findings/` because they still diverge — `regress`
must stay green. Each names what it would take to close.

Run one: `graphix-fuzz check graphix-fuzz/fuzz/open/<file>.gx`

## 01 — recursive activations have no per-activation cache memory

From aug15b hz0 fuzz 000000, the recursive half (its non-recursive half
is fixed and pinned as `findings/callee-taint-cache-honor-aug2026`).

The interp gives every call site its own retained lambda instance, so a
recursive chain has one instance PER ACTIVATION, each with its own
select-scrutinee cache. `f(n % -1)` bottoming at depth 1 therefore rides
depth 1's own history (the previous cycle's `0`), and `8 - 0` publishes.

A kernel's per-call-site block is carved by the CALLER out of its own
storage, which a self-call cannot do: the block would have to nest one
level per activation, unboundedly. The back-edge passes 0, so the
recursive activation gets no memory and its ride misses.

Sharing the caller's own block was tried and REJECTED: it aliases two
activations' histories, which produces wrong values rather than missing
ones — strictly worse than no memory.

Closing it means one of:
  (a) **De-fuse** any non-tail-recursive body that claims interior cache
      words. Honest, and it is what Eric's "no storage → de-fuse, never
      pass through" bar (2026-08-07) already says. Costs the fusion of
      essentially every non-tail recursive function (fib and friends) —
      a real perf regression, hence a decision rather than a fix.
      NOT tail loops: `claim_site_word_replay` already refuses in
      tail-loop bodies, so the rebind-and-jump kernels are untouched.
  (b) **Per-activation blocks**: a retained chain of blocks indexed by
      recursion depth, the exact analogue of the interp's per-activation
      instances, freed with the kernel. The depth counter already exists
      (`graphix_depth_push`) and the per-slot chain machinery
      (`graphix_slot_state_table`) is the same shape, so this is a
      contained feature rather than a redesign.

## 02 — a newly-resolved deref fires one cycle late

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

## 03 — a bottomed string dependency rides in the interp, misses in the JIT

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
