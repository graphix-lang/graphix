# Testing

Status: current principle.
Pins: `graphix-package-core/src/testing.rs`.

How `run!` fixtures run, and what a landing owes.

- `run!` (`graphix-package-core/src/testing.rs`) runs a fixture in
  each `testing::Mode`: `interp` and `jit` (serial unless `GRAPHIX_PAR`
  is set), and each again with every fork point forked (`par`,
  `jit_par`), asserting its predicate in each (and, in debug builds, that
  `interp` and `par` ran no fused code); nothing compares the
  modes, so a predicate pins the value exactly and a refusal names its
  message (`testing::refused`). The value must be of its static type in
  every form it carries, and the predicate sees it as data
  (`testing::observed`: markers stripped). `interp` also checks the program alone
  and fails unless the check refuses exactly when the build does
  (`; build_only` exempts a refusal only a build makes: `#[native]`,
  `#[parallel]`, the definition assertions). `FuseExpect::{Jit, None}` asserts
  WHETHER the build fuses anything, bidirectionally (`CompRes::fused`,
  taken before a cycle runs: a run-time bind fuses later); `#[native]`
  asserts that an expression does. Every mode, `testing::init`, the `eval*` helpers,
  `compile_error` and `Fixture` restore the package root from the image
  the process compiled once per root, as a warm shell start does, and
  fail where it does not restore; a start through `testing::config`
  compiles it, and `GRAPHIX_TEST_ROOT=compile` compiles every one. A
  `run!` fixture is a `testing::RunFixture` value its four tests hand to
  `testing::run_fixture`, compiled once, never a driver per fixture.
  Every wait for a value goes through `testing::next_update` (or
  `next_update_raw`, the value as delivered), which `run!`,
  `run_with_tempdir!`, the `eval*` helpers, `Fixture` and
  `Collect::First` share: it fails when the runtime settles without the
  value (nothing outstanding can bring it), and wall time bounds only a
  hang (`testing::HANG_GUARD`), so a program's work takes what the box's
  load makes it take. `Collect::Settled` takes every value until the
  settle, under the same guard. A test takes no deadline of its own for
  a value that must come; a wall window is only for what must not come.
  The GUI and TUI harnesses (`GuiTestHarness::wait_until`/`deliver_batch`,
  `TuiTestHarness::next_update`/`wait_for`) wait the same way, with no
  duration argument: a settle with the wait unmet fails at once, and a
  widget's own spawned task (the data table's dispatch task) keeps the
  runtime from settling, leaving `HANG_GUARD` its only limit.
  `Collect::Quiet` also ends a collection at the first gap of its window
  after a value, for a program with a standing source that never
  settles; a program that ends by itself collects `Settled`. The window
  counts only while the runtime is idle (it starts at a
  `GXHandle::wait_idle` answer and reopens if a batch came while that
  was asked again), so a long cycle is never a quiet one.
  `GRAPHIX_FUSE_AUDIT=1 cargo test -- jit --nocapture` prints the audit,
  and in the forced modes a `PARAUDIT-NOFORK` line for each fixture that
  forked nothing (a finding where the fixture has fork points).

- A semantics change is not landed until it has soaked; gates are not
  the fuzzer.
