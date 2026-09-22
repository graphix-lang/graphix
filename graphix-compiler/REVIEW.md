# Compiler CR re-evaluation

Rechecked CR10 and CR25 against `8495b9e9`. **CR10 is resolved and its comment
has been removed. CR25 remains open**, with a narrower updated CR and a failing
regression probe. Across the original review, 26 of 27 findings are closed.
This re-evaluation changes comments and this report only.

## Open: CR25 — nested dispatch still inherits the outer loan

Location: [node/coretraits.rs:547](src/node/coretraits.rs#L547).

The outer API now lends the context exclusively: its closure receives no context,
printers use an environment snapshot, and hook sites have their own events.
Removing the blanket loan from `CachedArgs::update_inner` fixes the ordinary
outermost invocation. Those changes are accepted.

However, `eval_with_hooks` leaves its guard installed until `f()` returns.
`abstract_value::hooked` calls the selected dispatch function without suspending
that handle. The dispatch then borrows `&mut ExecCtx` and calls
[the trait's site](src/node/coretraits.rs#L371), which can run a public safe
`EvalCached::eval`. That nested eval still sees the outer hook despite never
installing a loan itself.

The same safety violation remains reachable: a nested safe eval can retain
`&ctx.env` while formatting a `TVal`; the inherited hook can then reconstruct
`&mut ExecCtx` and mutate that environment while the shared borrow is live.
Neither owning a separate event nor removing the immediate wrapper masks an
already installed outer handle.

Suspend inherited dispatch while running code that receives the context, and let
explicit inner comparison/formatting wrappers install fresh loans. Restore the
outer handle on return and unwind. Include nested builtin calls in the regression
coverage.

## CR25 reproduction

Save the following as `stdlib/graphix-tests/tests/cr25_recheck.rs`, then run:

```sh
cargo test -p graphix-tests --test cr25_recheck
```

The probe deliberately compares without an explicit loan to detect inherited
hooks. It never reads the context or retains a context-derived reference. The
first assertion passes: the ordinary call answers structural `false`. The second
fails with `String("armed")`, expected `String("unarmed")`: the same builtin inside
a Display implementation invokes the custom Eq through the outer handle.

```rust
use anyhow::Result;
use graphix_compiler::{Effect, ExecCtx, Rt, UserEvent};
use graphix_package_core::{CachedArgs, CachedVals, EvalCached, testing::eval_with_setup};
use netidx_value::Value;

#[derive(Debug, Default)]
struct Probe;

graphix_package_core::unit_image_state!(Probe);

impl<R: Rt, E: UserEvent> EvalCached<R, E> for Probe {
    const NAME: &str = "cr25_probe";
    const EFFECT: Effect = Effect::Sync;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, from: &CachedVals) -> Option<Value> {
        Some(Value::Bool(from.0[0].as_ref()? == from.0[1].as_ref()?))
    }
}

#[tokio::test(flavor = "current_thread")]
async fn nested_builtin_runs_unarmed() -> Result<()> {
    let packages: &[graphix_package_core::testing::PackageRef] =
        graphix_package::package_refs!();
    let (value, ctx) = eval_with_setup(
        r#"{
            type Key = Abstract<i64>;
            impl Eq for Key { let eq = |a, b| true };
            let probe = |a: Key, b: Key| -> bool 'cr25_probe;
            type Outer = Abstract<i64>;
            impl Display for Outer {
                let fmt = |x| select probe(Key(1), Key(2)) {
                    true => "armed",
                    false => "unarmed"
                }
            };
            (probe(Key(1), Key(2)), "[Outer(0)]")
        }"#,
        packages,
        |ctx| ctx.register_builtin::<CachedArgs<Probe>>().unwrap(),
    )
    .await?;
    ctx.shutdown().await;
    let Value::Array(parts) = value else { panic!("expected tuple") };
    assert_eq!(parts[0], Value::Bool(false));
    assert_eq!(parts[1], Value::from("unarmed"));
    Ok(())
}
```

The temporary test was run and then removed after recording it here. It confirms
the inherited active hook; the shared-borrow safety consequence above follows
from the dispatch path, rather than a sanitizer or deliberate undefined-behavior
probe.

## Resolved: CR10 — concrete implementation matching

`impl_for` now matches a fresh implementation head in both directions, sharing one
substitution across repeated variables and preserving their constraints. Hook
sites and method signatures are instantiated and cached by the concrete carried
type. The previous overlap-based selector is gone.

Verified with the committed `core_eq_impl_must_apply` test in both engines and
with the rebuilt shell:

- Union parameters, mismatched repeated parameters, and unsatisfied bounds now
  correctly fall back to structural equality (`false` in the old reproductions).
- Valid repeated-parameter and bounded implementations still run (`true` in the
  committed test).
- The prior sole-specialization and phantom-specialization cases remain correct.
- A bound implemented generically for `Array<'a>` accepts `Marker<Array<i64>>`.
- Generic equality works across integer and string instantiations; generic
  Display prints `box:1`, `box:s`, `box:2` across repeated and different types.

Previously closed findings remain closed: 01–09, 11–24, 26, 27.

## Validation

- `cargo test -p graphix-compiler -p graphix-tests`: **2,979 passed, two ignored**
  (208 compiler tests and 2,771 language/package tests).
- Rebuilt `graphix-shell`. All ten CLI probes passed `--check` and produced their
  expected results with fusion enabled and disabled.
- The separate nested-builtin probe above failed as described.
- `cargo fmt --all --check` and `git diff --check`: passed.

## Disposition of the fourth round

| CR | Resolution |
|---|---|
| 25 | `abstract_value::hooked` suspends the installed handle for a dispatch's duration (restored on return and unwind), so the code an implementation runs — a builtin's `eval` included — is armed only by a loan it takes itself. Pin: `graphix-tests/tests/hook_loan.rs` (the review's probe), which fails without the suspension. |
