# Activation state and the bottom-out rule

Status: built 2026-08-20 (Ruling 2 amended 2026-08-24)
Pins: `graphix-fuzz/findings/select-quiet-scrutinee-aug2026/`, `guard-bottom-ride-aug2026/`, `select-bottom-out-hold-aug2026/`, `tail-select-bottom-out-aug2026/`, `recursive-activation-blocks-aug2026/`, `slot-shrink-truncate-aug2026/`

## The witness

```graphix
{let x = array::iter([i64:1, i64:2, i64:3, i64:0]);
 let m = x / x / i64:3;                       // 0, 0, 0, ⊥ (0/0)
 let rec f = |n: i64| -> i64 select n {
   i64:0 => select i64:0 {i64:0 if m == i64:0 => i64:1, _ => i64:2},
   _ => f(n)};
 f(m)}
```

Before the rulings this one function had three behaviours, selected by
execution strategy: a never-framed standing body rode its held guard
and emitted a fourth `1` on the cycle whose only fresh input was a
bottom; a retained per-depth instance did the same; a per-pass-reset
frame read the tainted guard as false and fell through to `2`. Each
twin's JIT agreed with its own interpreter, so the differential oracle
was blind to the family. As a normal programmer reads it: `x` updates
four times, `m` three times and bottoms on the fourth — `f` should not
produce four values. That inlined recursion was UNBUILDABLE on the
value plane (per-depth cross-cycle history forfeits constant-space tail
loops) was further evidence the interpretation was wrong. Under the
rulings all shapes produce `[1,1,1]` + FreshBottom on both engines.

## Ruling 1 — the bottom-out rule (value plane)

**Held state serves selection routing, re-matching and operand service
— never the cycle's output bottomness. A node's production OR-joins the
bottomness of every delivery it consumed this cycle. Bottom in, bottom
out.**

- The FIRED plane is untouched: organic firing stands, and a select
  still fires on a bottomed scrutinee delivery — its production is
  FreshBottom.
- The value channel this cycle is what was DELIVERED this cycle, fresh
  or stale, bottoms included. Reading a standing SOUND stale value is
  legitimate (that is what stale means, and what `~` does); a ride
  cache overwriting a delivered bottom for emission is banned.
- Bottoms are STICKY on the value plane: a standing bottom keeps the
  channel valueless until a sound delivery replaces it (`Rt::
  store_value` answers `None` for a bottomed bind). **`hold` is the
  explicit recovery tool** — riding over bottoms is what it is for,
  exactly as `uniq`/`filter`/`~` are the explicit tools on the fired
  plane; the compiler never rides silently.
- **A bottom scrutinee bottoms the select**, full stop (Eric,
  2026-08-29): no held-arm re-run, even if the taken arm is an active
  async producer. There is no stored-selection ride of any kind. The
  retained selection still routes the taken arm's OWN fires on a
  stale-PRESENT scrutinee (`ChainOut::Quiet` in `node/select.rs`),
  which is why `select p { null => 42, p => subscribe(p) }` updates
  when `subscribe` does; that is organic own-firing, not a ride.
- **The per-fire formulation**: the emission is the taken arm's
  current production, re-tagged fresh, whenever a consumed input fires
  SOUND (a sound scrutinee delivery, a consulted guard's sound
  production, or the arm's own sound fire); when every fired consumed
  input is a bottom, the emission is FreshBottom regardless of the
  arm's standing value. A select with no value view consults no guards,
  so only its scrutinee delivery is consumed and a settled bottom stays
  quiet on unrelated guard fires.
- **The init-phantom guard**: a guard that has never produced (its
  deps deliver after init) is the same knowledge state as a bottomed
  guard — unknown, not false. The select bottoms until the guard first
  becomes evaluable; the tool for a startup default is initialising the
  guard's source (`let enabled = false; enabled <- …`), not an invented
  false.
- **Nesting composes through arm productions**: an inner select whose
  only fires were bottoms emits FreshBottom, which the outer consumes as
  a fired-bottom arm production; the outer's own sound fires do not
  resurrect it. In the kernel's flattened tail spine this needs a
  compile-time scope stack (`LowerCtx::sel_fires`, applied
  innermost-first at every `emit_kernel_return`) because a single
  loop-carried accumulator conflated the two selects' scopes. Bottom
  fires are per-current-iteration (SSA values recompute each pass),
  never loop-carried.

Rejected: giving the kernel per-instance entry-history storage to
reproduce the interpreter's fourth value — the ride's two motivating
pins never demanded value manufacture (one demanded the arm body's own
FRESH productions under a bottomed scrutinee, the other that a
selection hold QUIET rather than flip), so the fourth value was an
unruled composition of organic firing with ride substitution, and the
kernel was right.

## Ruling 1a — the consulted-guard rule

**A select consults arms top-down: structure first, guard second. A
consulted guard whose CURRENT channel is bottom makes the selection
UNDECIDABLE — the chain stops (no flip, no wake, no arm body; selection
state holds) and the select bottoms, whatever else fired. Guards of
structure-failed arms and of arms below the stop or take point are
irrelevant: they neither fire nor bottom the select.**

`select a { [x, y] => x + y, [x, y, tail..] if x / y == 0 => f(tail), _
=> 42 }` with `y = 0` bottoms unless the array is a pair; a programmer
expects arm 1's match to make arm 2's guard irrelevant. "If the
programmer has guards, and they bottom, it makes the select undecidable
when it happens — we have to bottom."

- "Consumed" is chain-scoped. Guards tick every cycle (evaluation), but
  only CONSULTED productions are consumed on both planes. The
  interpreter's `arm_match` returns `ArmMatch::{NoStruct, GuardFalse,
  GuardBottom, Matched}`; `Select::consulted` stores the consulted mask
  from the last re-match so quiet cycles honour it, and a standing
  consulted bottom keeps the select bottom until the guard recovers.
  The kernel re-runs its chain every invocation and needs no memory:
  the prologue evaluates each guard and the chain branches a
  bottom-channel guard to a shared undetermined block
  (`emit_select_bottom_value`), whose freshness reads the fired-plane
  accumulator.
- Sound-beats-bottom does not apply to guards: a sound scrutinee fire
  cannot rescue a consulted bottom guard by riding a held verdict — a
  previous delivery's verdict cannot route this one. A tainted guard is
  unknown, never false; taking `_` on it would invent a selection flip
  from a bottom.
- There is no guard-ride machinery on either engine (no held-bool
  serve, no prologue taint cache). Selection survival across a guard
  bottom is the chain-stop itself. A mid-loop iteration's bottom guard
  therefore bottoms the derivation on both engines — no
  cross-iteration verdict to sever.

## Ruling 2 — state multiplicity = activation multiplicity

**State has the multiplicity of activations. Non-tail recursion creates
an activation per level — full inlining, lazily materialised, each
level's state standing across cycles. Collection slots are each an
activation. A tail call creates an activation like any other; a tail
loop may reuse ONE activation across its iterations only when its body
is STATELESS** (`analysis::lambda_is_stateless`: every builtin reached
is `Effect::Stateless`, no `<-` target, callees transitively
stateless) — because then no program can tell. A stateless body has no
per-depth history, so constant space is free; a stateful one has O(n)
history and pays O(n) space, exactly as a slot vector does (`acc +
count(x)` in a tail call counts every iteration; the same body as a
fold counts per slot). Depth is bounded by memory on both engines
(`design/recursive_activations.md`).

Corollaries:

- A connect target's identity has the multiplicity of its binds:
  per-slot in collection callbacks, per-activation in recursion, one
  reused cell for a stateless tail loop's lifted counter.
- MapQ/FoldQ per-slot live instances (`design/collection_intrinsics.md`);
  retained per-depth instances are the inlining's standing nodes,
  materialised on demand.
- Tail and non-tail twins of a STATE-carrying body legitimately differ
  by ruling: do not read tail-vs-native agreement as an invariant for
  such bodies.
- Scope: the collapse covers SELF tail calls. Mutual tail recursion
  (`f`→`g`→`f`) is per-level, retained, not constant-space — narrower
  than Scheme's guarantee, deliberate and predictable from the source.

Kernel realisation: a self-call roots a lazily grown per-ACTIVATION
block tree (`graphix_site_child_block`, one root per self-call site so
sibling calls get separate trees; `free_self_block_tree` reclaims
unreached activations after each run — the shrink=delete twin). Callee
kernels define in TOPOLOGICAL order over the recorded static call edges
(a callee defined after its caller would run below a recursion with no
interior memory). Every in-loop state chain re-ensures in its enclosing
loop's always-executed exit block (`TruncRec` → `emit_slot_truncates`),
so a shrink-to-zero truncates exactly when the interpreter deletes the
slot activations. Stateful builtins never fuse, so the interior-sleep
multiplicity question does not arise in kernels.
