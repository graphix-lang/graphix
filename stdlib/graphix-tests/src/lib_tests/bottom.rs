use anyhow::{Context, Result};
use arcstr::format;
use graphix_compiler::{
    Event, Scope, Tag, TagValue, View, compile,
    expr::{ModPath, parser::parse_one},
};
use graphix_package_core::testing::{Collect, Mode, TestCtx, collect, config};
use netidx_value::Value;
use std::time::Duration;
use tokio::sync::mpsc;

async fn strict_bottom(mode: Mode) -> Result<()> {
    let (tx, _rx) = mpsc::channel(10);
    let flags = mode.flags();
    let ctx = TestCtx::start(
        config(tx, crate::TEST_REGISTER, vec![], |ctx| {
            ctx.control.set_par_mode(mode.par())
        })?
        .flags(flags),
    )
    .await?;
    let result = ctx
        .rt
        .with_ctx(move |ctx| -> Result<()> {
            let ctx = &mut ctx.view();
            let scope = Scope::root().append("strict_bottom");
            let mut input = compile(
                ctx,
                flags,
                &scope,
                parse_one("let input: (i64, i64) = never()")?,
            )?;
            let id = ctx
                .env
                .lookup_bind(&scope.lexical, &ModPath::from(["input"]))?
                .context("input binding")?
                .1
                .id;
            let value = graphix_compiler::rep::tuple([Value::I64(17), Value::I64(3)]);
            for (code, expected) in [
                ("input.0", Value::I64(17)),
                ("({x: input.0}).x", Value::I64(17)),
                ("(input.0 + 1) * 2", Value::I64(36)),
                ("input.0 > 0", Value::Bool(true)),
                ("input.0 == 17 && input.1 != 0", Value::Bool(true)),
                ("-input.0", Value::I64(-17)),
                ("!(input.0 == 0)", Value::Bool(true)),
                ("cast<i64>(input.0)?", Value::I64(17)),
                (r#""[input.0]""#, Value::from("17")),
                ("[input.0, input.1][0]?", Value::I64(17)),
                ("{17 => 17}{input.0}?", Value::I64(17)),
            ] {
                let spec = if mode.node_walk() {
                    parse_one(code)?
                } else {
                    parse_one(&format!("#[native]\n{code}"))?
                };
                let mut node = compile(ctx, flags, &scope, spec).with_context(|| code)?;
                for sleep in [false, true] {
                    for (i, tag) in [
                        Tag::FIRED,
                        Tag::STALE_BOTTOM,
                        Tag::STALE_BOTTOM,
                        Tag::STALE,
                        Tag::STALE,
                        Tag::FRESH_BOTTOM,
                        Tag::STALE_BOTTOM,
                        Tag::FIRED,
                    ]
                    .into_iter()
                    .enumerate()
                    {
                        if sleep && i == 1 {
                            node.sleep(ctx);
                        }
                        let mut event = Event::new();
                        event.view = if i == 0 { View::Birth } else { View::Cycle };
                        let payload =
                            if tag.is_bottom() { Value::Null } else { value.clone() };
                        event.variables.insert(id, TagValue::tagged(payload, tag));
                        let actual = node.update(&mut ctx.with_event(&mut event));
                        assert_eq!(
                            actual.tag(),
                            tag,
                            "{code}, sleep={sleep}, step={i}: {actual:?}"
                        );
                        if !tag.is_bottom() {
                            assert_eq!(
                                actual.value_cloned(),
                                expected,
                                "{code}, sleep={sleep}, step={i}"
                            );
                        }
                    }
                }
                node.delete(ctx);
            }
            let mut unused =
                compile(ctx, flags, &scope, parse_one("let unused: i64 = never()")?)?;
            let unused_id = ctx
                .env
                .lookup_bind(&scope.lexical, &ModPath::from(["unused"]))?
                .context("unused binding")?
                .1
                .id;
            let code = "select input.1 { 3 => input.0, _ => unused }";
            let spec = if mode.node_walk() {
                parse_one(code)?
            } else {
                parse_one(&format!("#[native]\n{code}"))?
            };
            let mut node = compile(ctx, flags, &scope, spec)?;
            for (i, tag) in [
                Tag::FIRED,
                Tag::STALE_BOTTOM,
                Tag::FRESH_BOTTOM,
                Tag::STALE_BOTTOM,
                Tag::STALE,
            ]
            .into_iter()
            .enumerate()
            {
                let mut event = Event::new();
                event.view = if i == 0 { View::Birth } else { View::Cycle };
                let expected = if i == 0 { Tag::FIRED } else { Tag::STALE };
                event.variables.insert(id, TagValue::tagged(value.clone(), expected));
                let payload = if tag.is_bottom() { Value::Null } else { Value::I64(9) };
                event.variables.insert(unused_id, TagValue::tagged(payload, tag));
                let actual = node.update(&mut ctx.with_event(&mut event));
                assert_eq!(actual.tag(), expected, "unused bottom, step={i}: {actual:?}");
                assert_eq!(actual.value_cloned(), Value::I64(17));
            }
            node.delete(ctx);
            unused.delete(ctx);
            input.delete(ctx);
            Ok(())
        })
        .await?;
    ctx.shutdown().await;
    result
}

async fn strict_sample(mode: Mode) -> Result<()> {
    let (tx, _rx) = mpsc::channel(10);
    let flags = mode.flags();
    let ctx = TestCtx::start(
        config(tx, crate::TEST_REGISTER, vec![], |ctx| {
            ctx.control.set_par_mode(mode.par())
        })?
        .flags(flags),
    )
    .await?;
    let result = ctx
        .rt
        .with_ctx(move |ctx| -> Result<()> {
            let ctx = &mut ctx.view();
            let scope = Scope::root().append("strict_sample");
            let mut bindings = Vec::new();
            for name in ["clock", "input", "sampled"] {
                let node = compile(
                    ctx,
                    flags,
                    &scope,
                    parse_one(&format!("let {name}: i64 = never()"))?,
                )?;
                let id = ctx
                    .env
                    .lookup_bind(&scope.lexical, &ModPath::from([name]))?
                    .context("test binding")?
                    .1
                    .id;
                bindings.push((node, id));
            }
            let mut sample = compile(ctx, flags, &scope, parse_one("clock ~! input")?)?;
            let consumer =
                if mode.node_walk() { "sampled + 1" } else { "#[native]\nsampled + 1" };
            let mut consumer = compile(ctx, flags, &scope, parse_one(consumer)?)?;
            for (i, (clock_tag, input_tag, value, expected_tag, expected_value)) in [
                (Tag::FIRED, Tag::STALE_BOTTOM, 0, Tag::FRESH_BOTTOM, 0),
                (Tag::STALE, Tag::FIRED, 7, Tag::STALE_BOTTOM, 0),
                (Tag::FIRED, Tag::STALE, 7, Tag::FIRED, 7),
                (Tag::STALE, Tag::STALE_BOTTOM, 0, Tag::STALE, 7),
                (Tag::FIRED, Tag::STALE_BOTTOM, 0, Tag::FRESH_BOTTOM, 0),
                (Tag::STALE, Tag::FIRED, 11, Tag::STALE_BOTTOM, 0),
                (Tag::STALE, Tag::STALE, 11, Tag::STALE_BOTTOM, 0),
                (Tag::FIRED, Tag::STALE, 11, Tag::FIRED, 11),
                // CR claude for claude: [bug/medium] This row pins `~` (and `~!`) keeping
                // its last value when its trigger goes bottom: Sample::update rides on
                // a trigger that is not fired, and a bottom trigger counts as not
                // fired. Everywhere else bottom in is bottom out (dense_delivery.md
                // R1/R3). The designated rides list `~`'s argument, not its trigger.
                // `select c { _ => 5 }`, which organic_firing.md calls the same
                // sampler, goes bottom with c, and so do once(c) and the other restart
                // builtins. Observable: `(y, c ~ 5)` keeps delivering after c went
                // bottom, while `(y, select c { _ => 5 })` does not. Rule which is
                // intended. Either the sample goes bottom with its trigger
                // (Sample::update sets the resident bottom when the trigger's tag is
                // bottom, under both bankings, and this row expects bottom), or the
                // trigger joins the designated rides in dense_delivery.md and
                // CLAUDE.md. probe:
                // design/review-2026-10-19/repro/tests-lib-b1-sample-rides-bottom-trigger.gx
                // (tests-lib-b1-02)
                // 2026-10-24 claude: Eric ruled (2026-10-24): try it: a sample goes bottom
                // with its trigger (dense_delivery R1/R3), and see how much it breaks.
                // 2026-10-25 claude: Tried with x-engine-firing-04: a sample bottomed by its
                // trigger breaks the never()-armed event idiom and the seq lowering (48
                // fixtures); reverted, see the note there. Readdressed to eric with it.
                // 2026-10-27 claude: Eric ruled (2026-10-27, cr-questions round 2, q2, `~`
                // with a bottom trigger (round-1 q2, follow-up)): yes, a (the
                // recommendation: (a).)
                (Tag::FRESH_BOTTOM, Tag::FIRED, 13, Tag::STALE, 11),
                (Tag::FIRED, Tag::STALE, 13, Tag::FIRED, 13),
            ]
            .into_iter()
            .enumerate()
            {
                let mut event = Event::new();
                event.view = if i == 0 { View::Birth } else { View::Cycle };
                event.variables.insert(
                    bindings[0].1,
                    TagValue::tagged(Value::I64(i as i64), clock_tag),
                );
                event.variables.insert(
                    bindings[1].1,
                    TagValue::tagged(Value::I64(value), input_tag),
                );
                let actual = sample.update(&mut ctx.with_event(&mut event)).clone();
                assert_eq!(actual.tag(), expected_tag, "sample step {i}: {actual:?}");
                if !expected_tag.is_bottom() {
                    assert_eq!(actual.value_cloned(), Value::I64(expected_value));
                }
                event.variables.insert(bindings[2].1, actual);
                let actual = consumer.update(&mut ctx.with_event(&mut event));
                assert_eq!(actual.tag(), expected_tag, "consumer step {i}: {actual:?}");
                if !expected_tag.is_bottom() {
                    assert_eq!(actual.value_cloned(), Value::I64(expected_value + 1));
                }
            }
            consumer.delete(ctx);
            sample.delete(ctx);
            for (mut node, _) in bindings {
                node.delete(ctx);
            }
            Ok(())
        })
        .await?;
    ctx.shutdown().await;
    result
}

/// A bottom scrutinee makes no selection and consults no guard: the
/// select is the scrutinee's bottom, fresh only when the scrutinee
/// fired, whatever a guard's channel does.
async fn bottom_scrutinee_consults_no_guard(mode: Mode) -> Result<()> {
    let (tx, _rx) = mpsc::channel(10);
    let flags = mode.flags();
    let ctx = TestCtx::start(
        config(tx, crate::TEST_REGISTER, vec![], |ctx| {
            ctx.control.set_par_mode(mode.par())
        })?
        .flags(flags),
    )
    .await?;
    let result = ctx
        .rt
        .with_ctx(move |ctx| -> Result<()> {
            let ctx = &mut ctx.view();
            let scope = Scope::root().append("bottom_scrutinee");
            let mut bindings = Vec::new();
            for name in ["x", "g"] {
                let node = compile(
                    ctx,
                    flags,
                    &scope,
                    parse_one(&format!("let {name}: i64 = never()"))?,
                )?;
                let id = ctx
                    .env
                    .lookup_bind(&scope.lexical, &ModPath::from([name]))?
                    .context("test binding")?
                    .1
                    .id;
                bindings.push((node, id));
            }
            let code = "select x { v if g > 0 => v, _ => 0 }";
            let spec = if mode.node_walk() {
                parse_one(code)?
            } else {
                parse_one(&format!("#[native]\n{code}"))?
            };
            let mut node = compile(ctx, flags, &scope, spec)?;
            for (i, (x_tag, g_tag, expected)) in [
                (Tag::FRESH_BOTTOM, Tag::FRESH_BOTTOM, Tag::FRESH_BOTTOM),
                (Tag::STALE_BOTTOM, Tag::FRESH_BOTTOM, Tag::STALE_BOTTOM),
                (Tag::STALE_BOTTOM, Tag::FIRED, Tag::STALE_BOTTOM),
                (Tag::STALE_BOTTOM, Tag::FRESH_BOTTOM, Tag::STALE_BOTTOM),
                (Tag::FRESH_BOTTOM, Tag::STALE, Tag::FRESH_BOTTOM),
                (Tag::STALE_BOTTOM, Tag::STALE_BOTTOM, Tag::STALE_BOTTOM),
            ]
            .into_iter()
            .enumerate()
            {
                let mut event = Event::new();
                event.view = if i == 0 { View::Birth } else { View::Cycle };
                event
                    .variables
                    .insert(bindings[0].1, TagValue::tagged(Value::Null, x_tag));
                let g = if g_tag.is_bottom() { Value::Null } else { Value::I64(1) };
                event.variables.insert(bindings[1].1, TagValue::tagged(g, g_tag));
                let actual = node.update(&mut ctx.with_event(&mut event));
                assert_eq!(actual.tag(), expected, "step {i}: {actual:?}");
            }
            node.delete(ctx);
            for (mut node, _) in bindings {
                node.delete(ctx);
            }
            Ok(())
        })
        .await?;
    ctx.shutdown().await;
    result
}

modes!(strict_bottom, strict_sample, bottom_scrutinee_consults_no_guard);

/// `x` and `xs` stand until 20ms and then go bottom; `y` turns 1 the
/// cycle after, from `t`: two timers' fires reach the runtime in no set
/// order under load.
const GOES_AT_20MS: &str = r#"
  let t = 0;
  t <- sys::time::timer(duration:20.ms, false) ~ 1;
  let x = select t { 0 => 7, _ => never() };
  let xs: Array<i64> = select t { 0 => [7], _ => never() };
  let y = 0;
  y <- select t { 0 => never(), n => n };
"#;

/// Every value `(y, expr)` delivers, in each mode, over [`GOES_AT_20MS`].
async fn beside_y(expr: &str) -> Result<Vec<(Mode, Vec<Value>)>> {
    let mut runs = tokio::task::JoinSet::new();
    for mode in Mode::ALL {
        let code = format!("{{ {GOES_AT_20MS} (y, {expr}) }}");
        runs.spawn(async move {
            let quiet = Collect::Quiet(Duration::from_millis(300));
            let got = collect(
                &code,
                crate::TEST_REGISTER,
                mode,
                quiet,
                Duration::from_secs(10),
            )
            .await;
            (mode, got)
        });
    }
    let mut out = Vec::new();
    while let Some(run) = runs.join_next().await {
        let (mode, got) = run?;
        out.push((mode, got.with_context(|| format!("{expr} in {mode:?}"))?));
    }
    Ok(out)
}

/// rand::pick and rand::shuffle go bottom with their input: beside `y`
/// each gives its value once, before `x` goes, and never again.
#[tokio::test(flavor = "multi_thread")]
async fn rand_picks_go_bottom_with_their_input() -> Result<()> {
    for (expr, want) in
        [("rand::pick(xs)", "[i64:0, i64:7]"), ("rand::shuffle(xs)", "[i64:0, [i64:7]]")]
    {
        for (mode, got) in beside_y(expr).await? {
            let got: Vec<String> = got.iter().map(|v| v.to_string()).collect();
            assert_eq!(got, [want], "{expr} in {mode:?}");
        }
    }
    Ok(())
}

/// The restart and iteration builtins go bottom with their input instead
/// of re-delivering their last value: once `x` is gone each, beside `y`,
/// delivers nothing, so no value carries `y`'s later 1.
#[tokio::test(flavor = "multi_thread")]
async fn restart_builtins_go_bottom_with_their_input() -> Result<()> {
    for expr in [
        "once(x)",
        "count(x)",
        "take(#n: 5, x)",
        "skip(#n: 0, x)",
        "queue(#clock: x, x)",
        "throttle(x)",
        "filter(x, |v| v > 0)",
        "array::group(x, |n, _| n >= 1)",
        "array::iter(xs)",
        "array::iterq(#clock: x, xs)",
    ] {
        for (mode, got) in beside_y(expr).await? {
            assert!(!got.is_empty(), "{expr} in {mode:?}: nothing before x went");
            for v in &got {
                let Value::Array(a) = v else { panic!("{expr} in {mode:?}: {v}") };
                assert_eq!(
                    a[0],
                    Value::I64(0),
                    "{expr} in {mode:?}: delivered after x went: {v}"
                );
            }
        }
    }
    Ok(())
}

/// An element owed to a collection that fired before its source went
/// bottom still delivers: the bottom stands only where a ride would.
#[tokio::test(flavor = "multi_thread")]
async fn owed_elements_deliver_through_a_bottom_source() -> Result<()> {
    const CODE: &str = r#"{
  let t = 0;
  let xs = select t { 0 => [1, 2, 3], _ => never() };
  t <- xs ~ 1;
  let e = array::iter(xs);
  let log: Array<i64> = [];
  log <- e ~ array::push(log, e);
  log
}"#;
    for mode in Mode::ALL {
        let quiet = Collect::Quiet(Duration::from_millis(200));
        let got =
            collect(CODE, crate::TEST_REGISTER, mode, quiet, Duration::from_secs(10))
                .await?;
        assert_eq!(
            got.last().map(|v| v.to_string()).as_deref(),
            Some("[i64:1, i64:2, i64:3]"),
            "{mode:?}: {got:?}"
        );
    }
    Ok(())
}
