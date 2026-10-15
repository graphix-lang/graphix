use anyhow::Result;
use graphix_package_core::{run, testing::FuseExpect};
use netidx_value::Value;

// A callee first dispatched late, under an arm, prints its constant at
// that dispatch, once. The arm node-walks: its callee's print does not
// follow the arm's entry, which a kernel could not re-fire at a re-entry.
#[tokio::test]
async fn late_callee_dispatch_primes_constants() -> Result<()> {
    use arcstr::literal;
    use graphix_package_core::{
        PrintSink,
        testing::{Mode, fixture_runtime, updates_until_quiet},
    };
    use std::time::Duration;
    use tokio::time::Instant;

    for mode in Mode::ALL {
        let sink = PrintSink::default();
        let (ctx, mut rx) = fixture_runtime([], crate::TEST_REGISTER, mode, |ctx| {
            ctx.libstate.set(sink.clone());
        })
        .await?;
        let result = ctx
            .rt
            .compile(literal!(
                r#"{
                    let k = 0;
                    k <- select k < 6 { true => k + 1, false => never() };
                    let g = |n: i64| -> i64 { println("first"); n };
                    let f = |s: i64| -> i64 select s { 0 => 0, n => g(n) };
                    f(select k < 3 { true => 0, false => k })
                }"#
            ))
            .await?;
        let values = updates_until_quiet(
            &mut rx,
            result.exprs[0].id,
            Duration::from_millis(100),
            Instant::now() + Duration::from_secs(5),
        )
        .await?;
        assert_eq!(values, [0, 0, 0, 3, 4, 5, 6].map(Value::I64), "{mode:?}");
        assert_eq!(sink.take(), "first\n", "{mode:?}");
        ctx.shutdown().await;
    }
    Ok(())
}

#[tokio::test]
async fn deleted_kernel_releases_activation_budget() -> Result<()> {
    use arcstr::literal;
    use graphix_package_core::testing::{Mode, fixture_runtime, next_update};
    use std::time::Duration;
    use tokio::time::Instant;

    for mode in Mode::ALL {
        let (ctx, mut rx) = fixture_runtime([], crate::TEST_REGISTER, mode, |ctx| {
            ctx.control.set_activation_budget(300);
        })
        .await?;
        let definitions = ctx
            .rt
            .compile(literal!(
                r#"
            let g = |x: i64| x + 1;
            let rec f = |n: i64| -> i64 select n < 1 {
                true => 0,
                false => f(n - 1) + g(n)
            }
        "#
            ))
            .await?;
        let base = ctx.rt.control().live_activations();
        for _ in 0..8 {
            let result = ctx.rt.compile(literal!("#[native] f(50)")).await?;
            let id = result.exprs.last().unwrap().id;
            let value =
                next_update(&mut rx, id, Instant::now() + Duration::from_secs(5)).await?;
            assert_eq!(value, Value::I64(1325), "{mode:?}");
            if !mode.node_walk() {
                assert!(ctx.rt.control().live_self_blocks() >= 50, "vacuous kernel test");
            }
            drop(result);
            let (blocks, activations) = ctx
                .rt
                .with_ctx(|ctx| {
                    (ctx.control.live_self_blocks(), ctx.control.live_activations())
                })
                .await?;
            assert_eq!(blocks, 0, "{mode:?}");
            assert_eq!(activations, base, "{mode:?}");
        }
        drop(definitions);
        ctx.shutdown().await;
    }
    Ok(())
}

run!(outlined_loop_stack_space, {
    use std::fmt::Write;
    let mut src = poolshark::local::LPooled::<String>::take();
    src.push_str("{ let f = |a0: Array<i64>| {\n");
    for i in 1..=500 {
        writeln!(src, "let a{i} = array::map(a{}, |x| x + 1);", i - 1).unwrap();
    }
    src.push_str("a500 }; #[native] f([0]) }");
    arcstr::ArcStr::from(src.as_str())
}, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if **a == [Value::I64(500)]); FuseExpect::Jit);

run!(inferred_guard_masks_unmatched_bind, r#"
{
    let errors = 0;
    catch(e) errors <- e ~ errors + 1;
    let f = |v: [i64, null]| select v {
        null as _ => 0,
        x if (100 /? x)? > 1 => 1,
        _ => 2
    };
    let n: [i64, null] = null;
    let selected = #[native] f(n);
    let variant = |v: [`A(i64), `B(i64)]| select v {
        `A(_) => 0,
        x if cast<i64>(x)? > 1 => 1,
        _ => 2
    };
    let selected_variant = #[native] variant(`A(0));
    sys::time::after_idle(duration:10.ms, (selected, selected_variant, errors))
}
"#, |v: Result<&Value>| matches!(v, Ok(v) if v.to_string() == "[i64:0, i64:0, i64:0]"); FuseExpect::Jit);

run!(decimal_guard_bottom_is_undecided, r#"
{
    let g = |v: decimal| select v {
        x if x * x > decimal:1.0 => 1,
        _ => 2
    };
    any(#[native] g(decimal:79228162514264337593543950335.0), 7)
}
"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(7))); FuseExpect::Jit);

run!(variant_payload_literal_tags, r#"
{
    let count = |v: [`Count([i64, null]), `Off]| select v {
        `Count(0) => 0,
        `Count(_) => 1,
        `Off => 2
    };
    let flag = |v: `Flag([bool, string])| select v {
        `Flag(false) => 3,
        `Flag(_) => 4
    };
    let nested = |v: `Outer(`Inner([i32, null]))| select v {
        `Outer(`Inner(i32:0)) => 5,
        `Outer(`Inner(_)) => 6
    };
    #[native] (count(`Count(null)), count(`Count(0)), count(`Count(5)),
        flag(`Flag("hello")), flag(`Flag(false)),
        nested(`Outer(`Inner(null))), nested(`Outer(`Inner(i32:0))))
}
"#, |v: Result<&Value>| matches!(v, Ok(v) if v.to_string() == "[i64:1, i64:0, i64:1, i64:4, i64:3, i64:6, i64:5]"); FuseExpect::Jit);

run!(select_null_literal, r#"
{
    let f = |v: [i64, null]| select v { null => 0, n => n + 1 };
    let g = |v: [i64, string, null]| select v { null => 2, _ => 3 };
    #[native] (f(null), f(5), g(null), g("present"))
}
"#, |v: Result<&Value>| matches!(v, Ok(v) if v.to_string() == "[i64:0, i64:6, i64:2, i64:3]"); FuseExpect::Jit);

run!(select_named_numeric_predicates, r#"
{
    let f = |v: [i64, f64]| select v { Int as _ => 0, _ => 1 };
    let g = |v: [i64, f64, decimal, string]| select v { Number as _ => 2, _ => 3 };
    #[native] (f(7), f(2.5), g(7), g(2.5), g(decimal:1.5), g("text"))
}
"#, |v: Result<&Value>| matches!(v, Ok(v) if v.to_string() == "[i64:0, i64:1, i64:2, i64:2, i64:2, i64:3]"); FuseExpect::Jit);

run!(select_variant_composite_leaves, r#"
{
    let f = |m: [`Idle, `Busy(i64)], n: i64| select (m, n) {
        (`Idle, 0) => 1,
        (`Busy(x), y) => x + y,
        (_, _) => 0
    };
    let g = |m: [`Idle, `Busy(i64)]| select {mode: m, step: 0} {
        {mode: `Busy(x), step: _} => x,
        _ => 0
    };
    #[native] (f(`Idle, 0), f(`Idle, 1), f(`Busy(5), 7), g(`Busy(9)), g(`Idle))
}
"#, |v: Result<&Value>| matches!(v, Ok(v) if v.to_string() == "[i64:1, i64:0, i64:12, i64:9, i64:0]"); FuseExpect::Jit);

run!(select_variant_composite_whole_bind, r#"
{
    let f = |m: [`No, `Some(string)]| select (m, 0) {
        (v@ `Some(s), _) => (select v { `Some(t) => t }) == s,
        _ => false
    };
    #[native] (f(`Some("payload")), f(`No))
}
"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if **a == [Value::Bool(true), Value::Bool(false)]); FuseExpect::Jit);
