# Representable value-bottom in the fused backends

**Bottom = "no value this cycle."** A never-fired input, a pending async result,
a `?`-error, and a div-by-zero are all the same thing: the absence of a value on
this cycle. There is one representation and one rule, shared by both evaluators:

> A computation emits bottom **iff its OUTPUT consumes a bottom** — never because
> some *unrelated* input is bottom. `select c { 0 => x, 1 => never_fired }` with
> `c = 0` must yield `x`, even though the `never_fired` arm's input has no value.

"Resolves later vs. never" is not a this-cycle distinction: the runtime re-fires on
any input change (an async result arriving is an input update → re-fire; the arg
cache already gives `x + subscribe` a value once `subscribe` has *ever* fired), and
a div0 simply never triggers a re-fire. So async-pending and value-bottom collapse
into the single "bottom" notion — there is no separate pending channel.

## Node-walk (canonical)

`Update::update` returns `Option<Value>`; `None` = bottom. `Value` is already the
universal `#[repr(u64)]` tagged union, so no parallel bottom type is needed. `?`
absorbs (a bottom operand short-circuits the op to `None`); unchecked div/mod map
div0 to `None`; `?` on `Value::Error` → `None`; `error(v)` is a *value*
(`Some(Value::Error)`), not bottom. A `select` whose scrutinee is bottom produces
bottom (the node can't choose an arm); a bottom *guard* just falls through to the
next arm.

## Fused JIT — the taint channel (#219)

The JIT realizes the identical rule, implemented so a fused kernel can *run* past a
bottom input rather than aborting:

- A missing / unfired kernel input is fed a **taint-marked, helper-safe placeholder**
  (`Value::Null` / an empty `ValArray` / an empty `ArcStr`) — a value the drop/clone
  helpers can handle — with a `TAINT` bit set in its discriminant word.
- Pure ops **propagate taint** into their result disc (`propagate_taint`); the
  helpers mask the tag off before use, so a tainted disc never corrupts an operation.
- The kernel **forces bottom** (emits `None`) only where the taken output path
  actually consumes a tainted value (`is_tainted` at the output and at destructuring
  consumers) — matching the node-walk's "output consumes bottom" rule exactly. An
  unrelated tainted input on an un-taken `select` arm does not bottom the kernel.

This is paired with the per-param **STALE** ("did not fire this cycle") bit, so a
fused kernel replicates the node-walk's non-async firing: an output fires only when
an input feeding it actually fired. Together, taint + STALE are what let arbitrarily
wide regions fuse with no input-count cap (each rides the param's disc, no separate
validity bitmask). Emission lives in `fusion/emit.rs` / `fusion/emit_helpers.rs`.

## The one accepted limitation

An **infinite PURE tail recursion** hangs the JIT: a native loop can't yield to the
scheduler, so a genuinely non-terminating pure computation spins instead of producing
the node-walk's per-cycle "continue." This is accepted/correct — the node-walk's
reactive re-entry is the artifact, not a bottom the JIT is failing to represent.
