# The plan: always-update select, no bottom value, `seq`

Status: DECIDED 2026-09-03 (Eric: "I actually think this change — seq,
select always updating every arm, and eliminating bottom as a value —
is worth doing"), not started. This document is the sequencing and
the acceptance criteria; the designs are `pure_select.md` (§11 the
select, §12 the bottom question, §13 seq's lowering and the port),
`seq_blocks.md` (the construct), and `levels_and_events.md` §0 (the
kind, which the always-update select no longer needs for correctness).

## 1. The end state, on one page

So every step has the same target. After all three changes:

- **Delivery is sparse.** `Update::update` returns `Option<_>`:
  `Some(v)` is a production this cycle, `None` is nothing this cycle.
  Consumers keep the one cache they always had. There is no bottom
  value, no tag algebra, no fresh/stale distinction. An expression
  that cannot compute (`1/0`, an out-of-range index under `$`, a
  scrutinee or guard that never produced) produces nothing, and its
  consumers keep their last value. Failure that a program should see
  is an ERROR VALUE (`Result`, checked arithmetic, `?`, `$`).
- **Select is a mux with no sleep.** An IMPURE arm (one reaching a
  `Sync` or `Async` builtin, `~`, `<-`, `catch`) is updated every
  cycle its inputs fire, taken or not. A PURE arm is evaluated only
  while taken — unobservable, and required so recursion terminates.
  The select's output is the taken arm's value, firing when a consumed
  input fires. Under an untaken arm, connects do not write and event
  effects do not issue; level effects follow their inputs regardless,
  and take `[T, null]` keys — `null` tears them down. A recursive call
  through an impure callee under its own select is a compile error.
- **`~` samples**, banking the trigger while the right side has no
  value and paying at its first arrival. Uniform by construction: a
  value, once present, cannot become absent.
- **`<-` is feedback** to the next cycle; variables are queues, one
  delivery per cycle in write order.
- **Kernels are pure** (strict fusion, unchanged). The boundary keeps
  two bits per input — cache present, fired this cycle — and inside
  the kernel an absent operand aborts the enclosing computation to
  `None`. Lazy select chains keep untaken arms unevaluated, as now.
- **`seq trigger { … }`** is a `~` chain with a busy flag and one
  catch, printable with `--expand`.

What is deleted, by name: sleep (`Update::sleep`, `Apply::sleep`,
115 implementations), wake catch-up (`TrackedFires`, `Bind::pattern`,
`Bind::facet`, the `slept` bits, `Event::wake_init`, the wire's wake
bit), the `Held` ride residents, the restart-on-sleep contracts,
`shrink_unwind`, the four-state `TagValue`/`TagView`, R3 bottom
propagation, `Sample`'s tainted arm, the bottom-scrutinee and
consulted-guard bottom rulings, the FreshBottom logging rule, the
`tval` bottom printer, the phantom resident. Superseded docs:
`dense_delivery.md`, `wake_catchup.md`, most of
`activation_state.md`, the sleep half of `organic_firing.md`.

## 2. The order, and why

**A. The select** (`pure_select.md` §11) → **B. Sparse delivery**
(§12) → **C. `seq`** (`seq_blocks.md`, lowering per §13.1).

- A before B: two of dense delivery's four legs are sleep (the
  re-surfacing that made tags necessary, the tail spine's stateful
  interiors). With A landed, B is a currency change with no semantic
  argument left against it; done first, B would have to re-derive
  sleep's rules in the new currency and then delete them.
- A and B as two passes, not one: both touch every node, and one pass
  over 115 nodes changing the return type AND deleting sleep is an
  unsoakable, unbisectable change. Each pass soaks a round-day on the
  fleet before the next starts (a semantics change is not landed
  until it has one).
- C last for its PINS, in parallel for its SYNTAX: the parser, the
  AST and the `--expand` printer have no semantics dependency and can
  start any time; the runtime pins and the port's ceremony conversion
  land after B so they are written once against the end state.

## 3. Step A — the select

Decisions to pin first (my recommendations; each is a one-line
ruling):

1. **What the taken bit gates.** RULED (Eric, 2026-09-03: "given that
   using the taken bit in this way solves the sample tax you can
   consider it ruled in favor"): connects, and the ISSUE of event
   effects (`spawn`, an rpc call, `write`, `println`) — both sampled
   on the arm's matching delivery per decision 5; never a level effect
   (`subscribe`, `publish`, `timer`, `suspend`, `PublishRpc`).
   `Effect::Async` grows a `Follow`/`Issue` distinction declared per
   builtin.
2. **The `[T, null]` key list.** RULED fine, expand as needed (Eric):
   `subscribe`, `publish`, `timer`'s duration, `PublishRpc`'s path,
   `tui::suspend`'s bool (already a level), `sys::fs::watch`, the
   gui/tui runners' subscriptions. `null` tears down; a bottomed arg
   (until B lands) means no change.
3. ~~A dead-write diagnostic for a literal RHS in an arm.~~ DROPPED
   (Eric): under decision 5 an unsampled connect in an arm is sampled
   on the scrutinee delivery, so `screen <- \`Pick` fires on every
   matching delivery — it is not dead, it is the on-entry write, and
   the port's three constant writes are correct as written.
4. **Refusal wording** for a recursive call through an impure callee
   under its own select.
5. **The write rule** (`pure_select.md` §11, "The write rule"): a
   connect inside an arm fires on the arm's matching scrutinee
   delivery with the RHS as it stands; an explicit `~` at the RHS
   root keeps its own trigger; the same for event-effect issues. This
   is what makes `select screen { A => x <- A, B => x <- B }` do what
   it says, removes `k ~` from handler writes, and makes the accidental
   counter unwritable in a handler. RULED yes (Eric, 2026-09-03:
   "when I said gated I meant sampled … when you put `x <- x + 1` in
   a select arm you almost never mean to build a free running loop,
   you mean to increment x when go is true"). An unsampled self-loop
   in an arm is one step per delivery; the free-running form is
   spelled `x <- x ~ x + 1`, and the book's counter (`x` as its own
   scrutinee) loops because each write re-delivers the scrutinee.

Work:

- A1. `Select::update`: impure arms every cycle, pure arms while
  taken; the write rule (decision 5) — `Connect` under an arm fires
  on the arm's matching delivery unless its RHS root is a `~`; per-arm purity from `lambda_is_stateless` over the arm's
  subtree at compile; the taken index as a per-cycle context bit
  (beside the frame flags) that `Connect` and the issue-class builtin
  wrappers consult, propagated into callees dispatched from an arm,
  conjoined through nesting.
- A2. Delete sleep and everything in §1's list that belongs to it.
  `Node`'s guarded `sleep` funnel goes; `Sample::sleep` was only a
  forward.
- A3. Level effects: the null keys; teardown on `null`; the nine
  `fn sleep` hooks in sys::net/sys::time become the null branch.
- A4. Interp recursion reclaim by reach stamp (the kernel's mechanism:
  stamp reached activations each run, delete the rest) replacing
  `CallSite::sleep` under `shrink_unwind`. Kernel side unchanged.
- A5. Compile-time refusal of decision 4.
- A6. Pins. Re-adjudicate in place, expecting identical traces:
  `wake-catchup-sep2026/00–06`, `dyncall-arm-init-stale-aug2026`,
  `select-wake-rematch-sep2026`, `default-arg-birth-sep2026`,
  `sleep-preserves-caches-jul2026`, `arm-local-bind-aug2026`,
  `sleep-restart-gate-aug2026`, `arm-rewake-ref-fired-aug2026`,
  `net_{subscribe,publish}_arm_rewake`. New pins for the rules: an
  untaken impure arm keeps counting; a connect under an untaken arm
  does not write, including inside a callee; a subscribe in an untaken
  arm stays live and `null` tears it down; a pure arm is lazy (the
  recursion base case); the impure-recursive refusal; the constant
  write's dead-write warning.
- A7. The kernel: `event.init` under `wake_init` and the wire's wake
  bit go; genuine init is `bit0` alone.
- A8. The port: the three outer-bind dispatches route the key if the
  latency number says so; `milestone_latency` before and after. (The
  three constant writes are correct under decision 5 — no edit.)
- A9. Book: the select chapter's "Writing From an Arm" becomes one
  sentence; the sleep/wake chapters go.
- A10. CLAUDE.md rules rewritten in the same change (the semantics
  bullets that name sleep, wake, restart, held residents).
- A11. Fleet soak, a round-day, before B starts.

Acceptance: the workspace gate green; the corpus `regress` green with
the pins re-adjudicated; the admin package's tests green with its
three-plus-three edits; the soak quiet; `milestone_latency` within
the release baseline (a role-menu key ≤ 3.2ms p50, or the reason
named).

## 4. Step B — sparse delivery

Decisions to pin first:

1. `Update::update -> Option<&Value>` — RULED (Eric): borrowed, the
   resident is the slot; keeps dense delivery's one real win (zero
   copies down delegation chains).
2. An absent scrutinee or guard is NO DECISION this cycle: the select
   produces nothing and keeps its selection for routing only — the
   next scrutinee production re-matches. Same on both engines (a
   kernel select with an absent scrutinee aborts to `None`).
3. A collection whose callback produced nothing for a slot produces
   nothing (the pre-dense rule).
4. `$` produces nothing on error and logs; unchecked arithmetic logs
   and produces nothing; `?` raises — unchanged in meaning, restated
   without "bottom".

Work:

- B1. The signature change through every node and `Apply`; `TagValue`,
  `TagView`, `Tag` deleted; the store holds values; `CachedVals` is the
  one cache.
- B2. The kernel boundary: the wrapper's per-input cache and fired
  bit; inside the kernel the existing TAINT plumbing becomes the
  implementation of abort-to-`None` (or a literal abort where the
  chain allows); `Kernel::update` returns `None` on abort.
- B3. `Sample`: delete the tainted arm; the bank is the whole rule.
- B4. The fuzz oracle: the trace records productions, not tags;
  `selfcheck`, `detcheck`, `gen-check` green.
- B5. Pins: `builtin-taint-gate-jul2026`, `taint-cache-callee-jul2026`,
  `dyncall-fire-gate-aug2026` must AGREE; the bottom-scrutinee and
  consulted-guard pins re-adjudicate to "no decision this cycle".
- B6. Delete: the `tval` bottom printer, the FreshBottom logging rule,
  R3, `dense_delivery.md` to superseded.
- B7. Book: the bottom chapter becomes the errors chapter's paragraph
  on "produces nothing".
- B8. Fleet soak, a round-day.

Acceptance: as A, plus the interp's per-cycle cost should DROP (no
ride-downgrade walk on quiet cycles) — measure.

## 5. Step C — `seq`

- C1 (any time). Parser and AST per `seq_blocks.md` §5: `seq
  [trigger] { … }`, `until`, `break`/`continue`, `while`, `for x in
  xs`; `seq` reserved; the core builtin `seq(i, j)` renamed `range`
  (~10 sites). `--expand` prints the lowering. Round-trip proptest
  updated.
- C2 (after B). The desugar to the chain form (`pure_select.md`
  §13.1): each step's leaves sampled on the previous completion;
  `busy` set at start and cleared at completion and in the handler;
  `filter(trigger, |_| !busy)` at the head; the user's catch hoisted
  as the block's handler; branches as a pure select choosing the
  chain head; loops as `any(entry, back)`; `until c` as the `go`
  idiom; the value is the last step's completion.
- C3. Pins per rule R1–R10 of `seq_blocks.md` §6, on both engines
  (a seq block never fuses as a whole; its pure selects do).
- C4. The port: the privileged handoff, install/finish/uninstall, the
  staged restore become `seq` blocks; `--expand` of each checked by
  eye against the hand-written original.
- C5. Book chapter; CLAUDE.md language-features entry.

Acceptance: the port's ceremonies read as sequences, the expanded
forms are the chains the port had by hand, the corpus stays green.

## 6. Risks, and what answers them

- **Interp cost under always-update** (every screen's impure arms
  walked per key). Answered early: A8's latency measurement is taken
  as soon as A1 works, before A2's deletions, so the number decides
  whether the outer-bind dispatches must route the key.
- **Engine disagreement on absent** (B). Answered by the rulings in §4
  and the oracle: both engines produce nothing, neither rides.
- **The generator's shapes.** The fuzz generator emits connects,
  stateful builtins and effects inside arms; all stay legal, their
  meaning changes per §1, and the oracle is bit-for-bit between
  engines, so the corpus keeps working as the differential gate
  through both soaks.
- **The port's lab validation** (ledger 6) is still pending and
  independent; the handoff fix of 2026-09-03 (sample the whole
  options) goes to the lab with it.

## 7. What each step lets us stop saying

After A: sleep, wake, catch-up, restart, "fires once per selection",
held residents, the quiet flag's wake bit. After B: bottom, taint,
fresh/stale, "bottom never reaches authors", the bottom-out rule, the
init-phantom guard as a special case. After C: "the ceremony is a
select over a step variable". The select chapter, the errors chapter
and the reactive-idioms chapter each get shorter.
