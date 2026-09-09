# Representable bottom

Status: current principle
Pins: `stdlib/graphix-tests/src/lib_tests/bottom.rs`; `graphix-fuzz/findings/select-bottom-out-hold-aug2026/`, `standing-bottom-refire-sep2026/`

**Bottom = "no value this cycle."** A never-fired input, a pending async
result, a `?`-error and a division by zero are the same thing: the
absence of a value on this cycle. There is one representation and one
rule, shared by both evaluators:

> A computation is bottom **iff its output consumes a bottom** — never
> because an unrelated input is bottom. `select c { 0 => x, 1 =>
> never_fired }` with `c = 0` yields `x`, although the other arm's
> input has no value.

"Resolves later" and "never" are not a this-cycle distinction: an async
result arriving is an input update, which re-fires the consumer, and a
div0 simply never re-fires. So async-pending and value-bottom are one
notion; there is no separate pending channel. `error(v)` is a VALUE
(`Value::Error`), not bottom; `?` on an error value is bottom.

## Node-walk

`Update::update` returns `&TagValue` every cycle (`design/
dense_delivery.md`); bottom is the TAINT bit on the production's tag,
never an absent production. Ops short-circuit to bottom on a bottom
operand; unchecked div/mod map div0 to bottom; a select whose scrutinee
is bottom is bottom (it cannot choose an arm — `hold` the scrutinee to
persist across a bottom cycle); a consulted guard whose channel is
bottom makes the select undecidable and bottom (`design/
activation_state.md`). Bottom never reaches a builtin author: the
`CachedArgs` wrapper bottoms the invocation without calling `eval`
when any argument slot is bottom (`CachedVals::any_bottom`).

## Fused JIT — the taint channel

The kernel realises the identical rule in a form that lets it RUN past a
bottom input instead of aborting:

- A bottom kernel input is staged as a **taint-marked, helper-safe
  placeholder** (`Value::Null`, an empty `ValArray`, an empty `ArcStr`)
  with the `TAINT` bit set in its discriminant word; a standing bottom
  carries `TAINT|STALE`, a triggering one bare `TAINT`.
- Pure ops **propagate taint** into their result disc
  (`propagate_taint`); helpers mask the tag off before use, so a
  tainted disc never corrupts an operation.
- The kernel's result is bottom only where the taken output path
  consumes a tainted value (`is_tainted` at the output and at
  destructuring consumers). A tainted input on an untaken select arm
  does not bottom the kernel. A fastcall site bottoms without invoking
  the fn when any argument is tainted.
- A bottom is a PRODUCTION: its STALE bit follows the same trigger fold
  as a value (`nodes::emit_bottom_placeholder` takes the governing
  discs), so a standing bottom is not re-fired every cycle. The
  absent-delivery placeholders of an unmatched select are standing by
  construction — a delivery that never happened has no trigger.

The taint bit pairs with the per-param STALE bit ("did not fire this
cycle"); together they let regions of any width fuse with no input
count cap, because both ride each param's disc rather than a separate
validity mask. Emission lives under `fusion/emit/`.

## Non-termination

Evaluation is atomic within a cycle on both engines: a program may
legally spin forever inside one, and a native loop cannot yield to the
scheduler. Containment is the cooperative interrupt
(`GXHandle::interrupt`, polled by the interpreter's tail driver and
every emitted loop head), not a bottom the kernel fails to represent.
