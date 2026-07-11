# Const-valued fold body vs a firing source: which mode is right?

> **UPDATE (same night):** three of the four classes this file
> originally covered are FIXED (the spurious async-path flip — a
> resolved lambda outside a subtree-analysis' local fixpoint map read
> as Async via `unwrap_or_default`; the kernel's interior-bottom
> prior-success bridge leaking across loop iterations; the
> empty-source/fired-init scaffold term). p3–p9 and both reactive
> repros now AGREE at body-driven semantics in both modes. What
> REMAINS is one sharp question + one ABI-shaped consequence:
>
> **The DynCall side-channel arg stash is always-FIRED** (kernel.rs
> `dispatch_typed`, deliberate v1 parity: "delivers every arg on
> every call"). When an inner HOF instance INLINES into the caller's
> kernel, its loop is body-driven (quiet on const bodies); when
> inlining fails (e.g. the taint statement in frame_bottom_class) it
> becomes a DynCall, whose always-fired arg delivery makes the callee
> fire on every dispatch. The two shapes disagree with EACH OTHER —
> `p3` (inlined, [1:0 2:7]) vs `frame_bottom_class` (DynCall,
> [0,7,7,7]) are the same fold. The interp now implements body-driven
> consistently, matching the inlined shape.
>
> Ruling needed: body-driven is the settled P4 rule, so the DynCall
> path should carry REAL arg discs across the dispatch ABI (wire
> format change: (disc,payload) pairs instead of bare Values for the
> side-channel stash) — or the rule is amended. The remaining jul10e
> survivors (5 fuzz + frame_bottom_class, all the group() shape, and
> reactive/000009) are this class.

Found 2026-07-11 while validating the two-channel (TagValue) build.
PRE-EXISTING — the old binary diverges on the same shape (differently:
its interp additionally dropped an emission to the replay-leak bug).
Not a regression from the two-channel work; parked for a ruling
because it sits exactly on the body-driven-firing ruling's boundary.

## Repro (minimized, deterministic)

```graphix
{
  let n = i64:0;
  select n { v if v < i64:3 => n <- (v ~ n) + i64:1, _ => never() };
  let a = [n, n];
  array::fold(a, i64:0, |acc, x| {
    array::fold([i64:5, i64:7], i64:1, |a2, x2| x2);
    array::fold([i64:5, i64:7], i64:0, |a3, x3| x3)
  })
}
```

- interp: `7` at every n-fire (4 events)
- jit: `7` once (the init view), then quiet

A single-statement body (`|acc, x| array::fold([5,7], 0, |a3,x3| x3)`,
no discarded statement) AGREES — both emit per source fire — so the
divergence needs the two-callsite block shape. A body that consumes
acc (`{ ...stmt...; acc + x }`) also agrees.

## The semantics question

The source array `a` FIRES per cycle, but the body's RESULT path is
const-valued and reaches its value through nested in-language fold
CALLS (per-site instances), consuming neither acc nor elem.

- Body-driven firing (the P4 ruling) says: no body evaluation fired →
  the result is quiet. The KERNEL behaves this way.
- The node-walk emits per source fire in this shape (and has since
  before the two-channel work).

The `hof-lift-firing` pin (`|a,b| 100` — const source, firing INIT →
quiet) doesn't decide this: there the SOURCE never fired. There is no
pin for firing-source + const-CALL body.

Options: (a) rule the interp right (source fire ⟹ fold result fires;
teach the scaffold/inherit_hof_firing the source term); (b) rule the
kernel right (body-driven strictly; find and fix the interp path that
fires here — note the interp mechanism is not yet root-caused, the
inner instances' For gates never even log on cycles 2+, so the value
rides a cache seam). Instance-body INLINING (#36) would erase the
structural difference between this and the agreeing single-call shape.

Probe files (scratchpad copies also in this dir): p6_two_folds.gx,
p7_counter_src.gx, p8_one_fold_stmt.gx (agreeing control),
p4_no_taint.gx (agreeing control).

# Second, separable kernel gap: empty source + fired init

```graphix
{let n = i64:0; select n {v if v < i64:3 => n <- (v ~ n) + i64:1, _ => never()};
 array::fold(cast<Array<i64>>([])$, n, |a, x| a + x)}
```

- interp: emits n per fire (0,1,2,3) — the fold over an EMPTY source
  is the init, and the init fired
- jit: emits once

Also PRE-EXISTING (old binary identical). The scaffold's empty-source
firing term (`SlotFlags::apply`) folds the SOURCE discs into
`src_quiet` but routes the INIT through `fold_taint` (taint-only, no
stale contribution), so an init fire never reaches the empty-source
term. Fix sketch: include the init's disc in the empty-source
`src_quiet` AND-fold (it must NOT touch the non-empty slots-word —
the `hof-lift-firing` pin depends on init-fire alone staying quiet
for non-empty sources). Probe: p1_empty_static.gx.
