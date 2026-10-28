// A binding deleted while a reference or a function value still needs it
// lives on as an orphan; the orphans only other orphans keep are collected

use super::dense_deltas::run_delta_with;
use anyhow::Result;
use compact_str::format_compact;
use graphix_compiler::{Effect, ExecCtx, Rt};
use graphix_package_core::{
    CachedArgs, CachedVals, EvalCached, run,
    testing::{Mode, as_i64s, is},
};
use netidx::publisher::Value;
use std::time::Duration;

#[derive(Debug, Default)]
struct LiveBindings;

graphix_package_core::unit_image_state!(LiveBindings);

impl<R: Rt> EvalCached<R> for LiveBindings {
    const NAME: &str = "test_live_bindings";
    const EFFECT: Effect = Effect::Sync;

    fn eval(&mut self, ctx: &mut ExecCtx<'_, R>, _: &CachedVals) -> Option<Value> {
        Some(Value::I64((ctx.env.by_id.len() + ctx.control.lives().cells()) as i64))
    }
}

/// Eight collection slots running `body` (over the slot's `k`) live three
/// cycles, so what they write lands, and are deleted, 200 times; the
/// bindings and reference cells alive after each deletion must not grow.
async fn slot_bindings_settle(mode: Mode, decls: &str, body: &str) -> Result<()> {
    let code = format_compact!(
        "{{
            {decls}
            let live_bindings = |n: i64| -> i64 'test_live_bindings;
            let n = 0;
            n <- select n {{ k if k < 1200 => k + 1, _ => never() }};
            let src = select (n / 3) % 2 {{ 0 => [1, 2, 3, 4, 5, 6, 7, 8], _ => [] }};
            let out = array::map(src, |k| {{ {body} }});
            select n % 6 {{ 4 => live_bindings(n ~ array::len(out)), _ => never() }}
        }}"
    );
    let delta = run_delta_with(
        &code,
        mode,
        |ctx| {
            ctx.register_builtin::<CachedArgs<LiveBindings>>().unwrap();
        },
        Duration::from_millis(700),
    )
    .await?;
    let counts = as_i64s(&delta.values);
    assert_eq!(counts.len(), 200);
    let early = counts[20];
    assert!(counts[20..].iter().all(|n| *n == early), "{counts:?}");
    Ok(())
}

async fn recursive_closures_in_deleted_slots_are_collected(mode: Mode) -> Result<()> {
    slot_bindings_settle(
        mode,
        "",
        "let rec f = |i: i64| -> i64 select i { 0 => 0, i => k + f(i - 1) }; f(k)",
    )
    .await
}

async fn mutually_reaching_closures_in_deleted_slots_are_collected(
    mode: Mode,
) -> Result<()> {
    slot_bindings_settle(
        mode,
        "",
        "let rec g = |i: i64| -> i64 select i { 0 => 0, i => k + g(i - 1) };
         let rec f = |i: i64| -> i64 select i { 0 => 0, i => g(i) + f(i - 1) };
         f(k)",
    )
    .await
}

async fn self_referencing_values_in_deleted_slots_are_collected(
    mode: Mode,
) -> Result<()> {
    slot_bindings_settle(
        mode,
        "type Node = {me: [&Node, null], data: Array<i64>};",
        "let x: Node = {me: null, data: []};
         x <- {me: &x, data: [k, k]};
         k",
    )
    .await
}

modes!(
    recursive_closures_in_deleted_slots_are_collected,
    mutually_reaching_closures_in_deleted_slots_are_collected,
    self_referencing_values_in_deleted_slots_are_collected,
);

run!(
    a_saved_recursive_closure_outlives_its_slot,
    r#"{
        let n = 0;
        n <- select n { k if k < 8 => k + 1, _ => never() };
        let src = select n { k if k < 2 => [10], _ => [] };
        let gs = array::map(
            src,
            |k| { let rec f = |i: i64| -> i64 select i { 0 => 0, i => k + f(i - 1) }; f }
        );
        let saved: fn(x: i64) -> i64 = never();
        saved <- gs[0]$;
        select n { k if k >= 6 => saved(k), _ => never() }
    }"#,
    is(Value::I64(60))
);

run!(
    a_saved_reference_into_a_self_referencing_value_outlives_its_slot,
    r#"{
        type Node = {me: [&Node, null], data: i64};
        let n = 0;
        n <- select n { k if k < 8 => k + 1, _ => never() };
        let src = select n { k if k < 2 => [10], _ => [] };
        let rs = array::map(src, |k| {
            let x: Node = {me: null, data: 0};
            x <- {me: &x, data: k};
            &x
        });
        let saved: [&Node, null] = null;
        saved <- rs[0]$;
        select n { k if k >= 6 => select saved { null => -1, r => (*r).data + k }, _ => never() }
    }"#,
    is(Value::I64(16))
);

run!(
    a_saved_closure_writing_through_a_captured_reference_outlives_its_slot,
    r#"{
        let n = 0;
        n <- select n { k if k < 8 => k + 1, _ => never() };
        let src = select n { k if k < 2 => [10], _ => [] };
        let gs = array::map(src, |k| {
            let v = k;
            let p = &mut v;
            |x: i64| -> i64 { *p <- x; x + k }
        });
        let saved: fn(x: i64) -> i64 = never();
        saved <- gs[0]$;
        select n { k if k >= 4 => saved(k), _ => never() }
    }"#,
    is(Value::I64(14))
);
