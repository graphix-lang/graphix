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
  WHETHER anything fuses, bidirectionally; `#[native]` asserts that an
  expression does. Every mode, `testing::init`, the `eval*` helpers,
  `compile_error` and `Fixture` restore the package root from the image
  the process compiled once per root, as a warm shell start does, and
  fail where it does not restore; a start through `testing::config`
  compiles it, and `GRAPHIX_TEST_ROOT=compile` compiles every one. A
  `run!` fixture is a `testing::RunFixture` value its four tests hand to
  `testing::run_fixture`, compiled once, never a driver per fixture.
  `GRAPHIX_FUSE_AUDIT=1 cargo test -- jit --nocapture` prints the audit,
  and in the forced modes a `PARAUDIT-NOFORK` line for each fixture that
  forked nothing (a finding where the fixture has fork points).

- A semantics change is not landed until it has soaked; gates are not
  the fuzzer.
