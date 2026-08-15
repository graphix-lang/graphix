# fuzz/crashes — the campaign's crash drop, AUDITED TO ZERO 2026-08-15

A campaign writes crash findings here when `$HOME` is unset (normally
they land in `~/tmp/target/fuzz/crashes` — see `main.rs`). Crashes are
parked rather than promoted to `findings/` because `regress` runs
subjects IN-PROCESS: a subject that aborts the process takes the gate
run with it instead of reporting a failure.

The 22 artifacts that had accumulated here (June 12 – Aug 6 2026) were
all re-run on 2026-08-15. **None reproduces.** The disposition, by
class:

* **Stack-overflow SIGABRTs on unbounded NON-TAIL recursion** (8:
  crash_1/2/3/4/9/22/23/24). Now bounded by the call-depth limit —
  each settles on the whole-derivation bottom in milliseconds. Two were
  promoted as the pins that state this invariant on its own:
  `findings/nontail-recursion-depth-bound-aug2026/`. Three of the eight
  (crash_1/3/9) are TAIL loops rather than non-tail, and are covered by
  the next bullet.
* **"child HANG (outer deadline)"** (13: crash_5, 10–21). Not hangs and
  not loops — mostly trivial arithmetic that bottoms
  (`i64:100 / (i64:-1 * i64:0)`), plus an over-limit
  `array::init(i64::MAX)`. They were PRE-DENSE HARNESS ARTIFACTS: a
  program that produced no value left the child waiting out its
  deadline. Under dense delivery a bottom is a production, and every
  one of these now returns a value or an honest empty trace.
* **Legally non-terminating tail loops** (3: crash_1/3/9, counted
  above). `sum_to(n + 1, acc + n)` and friends spin forever inside one
  cycle on BOTH engines. That is permitted as of the atomic-recursion
  ruling (`design/atomic_recursion.md`) — not a bug, and deliberately
  NOT added to the corpus, where a never-terminating subject would burn
  its budget (and, since 78b9003e, its sequential retry) on every gate
  run.
* **One stray divergence** (`divergence_000025`), a `rand::` program
  filed here by mistake. `rand::` subjects are excluded from divergence
  recording now; it agrees.

Anything landing here in future gets the same treatment: re-run it
before assuming it is still real, and if it is, fix it — a crash is the
worst failure mode we have.
