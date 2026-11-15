// State the runtime makes for a call, an instance or a core-trait hook
// goes when the thing it was made for does: a long session (REPL,
// dynamic modules, a regrowing map) holds what is live, not its history.

use crate::init;
use anyhow::{Context, Result, bail};
use arcstr::{ArcStr, literal};
use graphix_compiler::{
    BindId,
    expr::{ExprId, FilesResolver, VfsEntry},
};
use graphix_package_core::testing::{
    Events, Mode, TestCtx, config, fixture_runtime, next_update,
};
use netidx::publisher::Value;
use tokio::sync::mpsc;
type Compiled = graphix_rt::CompRes<graphix_rt::NoExt>;

/// Wait for `eid`'s next update satisfying `done`.
async fn await_update(
    ctx: &TestCtx,
    rx: &mut Events,
    eid: ExprId,
    done: impl Fn(&Value) -> bool,
) -> Result<()> {
    await_value(ctx, rx, eid, done).await.map(drop)
}

/// [`await_update`], with the value that satisfied `done`.
async fn await_value(
    ctx: &TestCtx,
    rx: &mut Events,
    eid: ExprId,
    done: impl Fn(&Value) -> bool,
) -> Result<Value> {
    let mut last = None;
    loop {
        let v = next_update(&ctx.rt, rx, eid)
            .await
            .with_context(|| format!("waiting on {eid:?}, last saw {last:?}"))?;
        if done(&v) {
            return Ok(v);
        }
        last = Some(v);
    }
}

/// Compile `code`; the expression lives while the result is held.
async fn compile_one(
    ctx: &TestCtx,
    code: impl Into<ArcStr>,
) -> Result<(ExprId, Compiled)> {
    let res = ctx.rt.compile(code.into()).await?;
    Ok((res.exprs[0].id, res))
}

async fn store_len(ctx: &TestCtx) -> Result<usize> {
    ctx.rt.with_ctx(|ctx| ctx.rt.store().len()).await
}

/// A periodic timer's fires are private to it: each is gone from the
/// store once the timer has read it.
#[tokio::test(flavor = "current_thread")]
async fn timer_fires_leave_no_store_entries() -> Result<()> {
    let (tx, mut rx) = mpsc::channel(64);
    let ctx = init(tx).await?;
    let (eid, _expr) = compile_one(
        &ctx,
        "{
          let k = 0;
          k <- sys::time::timer(duration:0.005s, true) ~ k + 1;
          k
        }",
    )
    .await?;
    await_update(&ctx, &mut rx, eid, |v| matches!(v, Value::I64(10))).await?;
    let early = store_len(&ctx).await?;
    await_update(&ctx, &mut rx, eid, |v| matches!(v, Value::I64(60))).await?;
    let late = store_len(&ctx).await?;
    ctx.shutdown().await;
    assert!(late <= early, "the store grew from {early} to {late} entries over 50 ticks");
    Ok(())
}

/// A site whose callee alternates between lambdas with labeled defaults
/// keeps one default's store entry, not one per rebind.
#[tokio::test(flavor = "current_thread")]
async fn rebind_defaults_leave_no_store_entries() -> Result<()> {
    let (tx, mut rx) = mpsc::channel(64);
    let ctx = init(tx).await?;
    let (eid, _expr) = compile_one(
        &ctx,
        "{
          let f = |#a: i64 = 1, x: i64| x + a;
          let g = |#b: i64 = 2, x: i64| x * b;
          let h: fn(x: i64) -> i64 = f;
          let k = 0;
          k <- sys::time::timer(duration:0.005s, true) ~ k + 1;
          h <- select k % 2 { 0 => f, _ => g };
          (k, h(k))
        }",
    )
    .await?;
    let at = |n: i64| move |v: &Value| matches!(v, Value::Array(a) if a.first() == Some(&Value::I64(n)));
    await_update(&ctx, &mut rx, eid, at(10)).await?;
    let early = store_len(&ctx).await?;
    await_update(&ctx, &mut rx, eid, at(60)).await?;
    let late = store_len(&ctx).await?;
    ctx.shutdown().await;
    assert!(
        late <= early,
        "the store grew from {early} to {late} entries over 50 rebinds"
    );
    Ok(())
}

/// The fn-typed formals a static resolution records for an instance go
/// with the instance: a map whose slots regrow holds the live slots'.
#[tokio::test(flavor = "current_thread")]
async fn fn_forward_resolutions_go_with_their_instance() -> Result<()> {
    let (tx, mut rx) = mpsc::channel(64);
    let ctx = init(tx).await?;
    let _arr = ctx.rt.compile(literal!("let arr: Array<i64> = [];")).await?;
    let (eid, _expr) = compile_one(
        &ctx,
        "{
          let h = |f: fn(x: i64) -> i64, x: i64| f(throttle(x));
          array::map(arr, |x| h(|y| y + 1, x))
        }",
    )
    .await?;
    let env = ctx.rt.get_env().await?;
    let arr = env
        .lookup_bind(&graphix_compiler::Scope::root().lexical, &["arr"].into())?
        .ok_or_else(|| anyhow::anyhow!("arr not in scope"))?
        .1
        .id;
    let len = |n: usize| move |v: &Value| matches!(v, Value::Array(a) if a.len() == n);
    let mut forward_resolutions = vec![];
    for _ in 0..4 {
        let four: Vec<Value> = (1..=4).map(Value::I64).collect();
        ctx.rt.set(arr, Value::Array(four.into()))?;
        await_update(&ctx, &mut rx, eid, len(4)).await?;
        ctx.rt.set(arr, Value::Array(Vec::<Value>::new().into()))?;
        await_update(&ctx, &mut rx, eid, len(0)).await?;
        forward_resolutions
            .push(ctx.rt.with_ctx(|ctx| ctx.fn_forward_resolutions.len()).await?);
    }
    ctx.shutdown().await;
    assert!(
        forward_resolutions.iter().all(|b| *b == forward_resolutions[0]),
        "{forward_resolutions:?}"
    );
    Ok(())
}

/// A core-trait hook site rebuilt after the implementation list changed
/// takes its argument bindings with it.
#[tokio::test(flavor = "current_thread")]
async fn core_hook_sites_leave_no_bindings() -> Result<()> {
    let (tx, mut rx) = mpsc::channel(64);
    let ctx = init(tx).await?;
    let (eid, _expr) = compile_one(
        &ctx,
        "{
          type T = Abstract<i64>;
          impl Eq for T { let eq = |a, b| true };
          let n = 0;
          n <- sys::time::timer(duration:0.01s, true) ~ n + 1;
          (n, n ~ (T(1) == T(2)))
        }",
    )
    .await?;
    let mut decls = vec![];
    for k in 0..5 {
        let (id, decl) = compile_one(
            &ctx,
            format!(
                "{{ type U{k} = Abstract<i64>; impl Eq for U{k} {{ let eq = |a, b| true }}; null }}"
            ),
        )
        .await?;
        await_update(&ctx, &mut rx, id, |_| true).await?;
        decls.push(decl);
        // two ticks: a comparison ran against the grown list
        await_update(&ctx, &mut rx, eid, |_| true).await?;
        await_update(&ctx, &mut rx, eid, |_| true).await?;
    }
    let env = ctx.rt.get_env().await?;
    let seams =
        env.by_id.into_iter().filter(|(_, b)| b.name.starts_with("#seam")).count();
    ctx.shutdown().await;
    assert!(seams <= 2, "{seams} hook argument bindings after 5 rebuilds");
    Ok(())
}

/// A statement that fails to compile leaves nothing it registered: the
/// definitions it built before the failure go with it.
#[tokio::test(flavor = "current_thread")]
async fn failed_compiles_leave_no_definitions() -> Result<()> {
    let (tx, _rx) = mpsc::channel(64);
    let ctx = init(tx).await?;
    let before = ctx.rt.env_stats().await?.lambda_defs_len;
    for _ in 0..5 {
        let r =
            ctx.rt.compile(literal!("{ let f = |x| x; let g = |y| y; missing }")).await;
        assert!(r.is_err(), "the statement names nothing called missing");
    }
    let after = ctx.rt.env_stats().await?.lambda_defs_len;
    ctx.shutdown().await;
    assert_eq!(before, after, "failed compiles kept {} definitions", after - before);
    Ok(())
}

/// A definition a returned function names outlives its enclosing
/// definition's check, and goes with that definition.
#[tokio::test(flavor = "current_thread")]
async fn returned_definitions_go_with_their_enclosing_definition() -> Result<()> {
    let (tx, _rx) = mpsc::channel(64);
    let ctx = init(tx).await?;
    let before = ctx.rt.env_stats().await?.lambda_defs_len;
    for _ in 0..5 {
        let r = ctx
            .rt
            .compile(literal!("let mk = |k: i64| |#d = k, x: i64| cast<string>(d)$"))
            .await?;
        let held = ctx.rt.env_stats().await?.lambda_defs_len;
        assert_eq!(held, before + 2, "mk and the function it returns stay registered");
        drop(r);
        let after = ctx.rt.env_stats().await?.lambda_defs_len;
        assert_eq!(before, after, "deleted definitions kept {} entries", after - before);
    }
    ctx.shutdown().await;
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn nested_returned_definitions_go_with_their_enclosing_definition() -> Result<()> {
    let (tx, _rx) = mpsc::channel(64);
    let ctx = init(tx).await?;
    let before = ctx.rt.env_stats().await?.lambda_defs_len;
    for _ in 0..5 {
        let r = ctx
            .rt
            .compile(literal!(
                "let mk = |k: i64| |j: i64| |#d = k, x: i64| cast<string>(d)$"
            ))
            .await?;
        let held = ctx.rt.env_stats().await?.lambda_defs_len;
        assert_eq!(
            held,
            before + 3,
            "mk and the two functions it returns stay registered"
        );
        drop(r);
        let after = ctx.rt.env_stats().await?.lambda_defs_len;
        assert_eq!(before, after, "deleted definitions kept {} entries", after - before);
    }
    ctx.shutdown().await;
    Ok(())
}

/// A dynamic module's source that fails to compile leaves nothing it
/// registered, as a statement does.
#[tokio::test(flavor = "current_thread")]
async fn failed_dynamic_module_compiles_leave_no_definitions() -> Result<()> {
    let (tx, mut rx) = mpsc::channel(64);
    let ctx = init(tx).await?;
    let before = ctx.rt.env_stats().await?;
    for _ in 0..5 {
        let (eid, res) = compile_one(
            &ctx,
            r#"{
                let status = mod m dynamic {
                    sig { use core; val h: i64 };
                    source "let a = 1; let f = |x| x + a; let g = |y| y; let h = missing"
                };
                select status { error as e => e, null as _ => never() }
            }"#,
        )
        .await?;
        await_update(&ctx, &mut rx, eid, |v| matches!(v, Value::Error(_))).await?;
        drop(res);
    }
    let after = ctx.rt.env_stats().await?;
    ctx.shutdown().await;
    assert_eq!(before.lambda_defs_len, after.lambda_defs_len);
    assert_eq!(before.ref_var_total, after.ref_var_total);
    Ok(())
}

/// What the runtime and the env hold: a count that grows with what a
/// session has run and deleted is a leak.
async fn held(ctx: &TestCtx) -> Result<[usize; 17]> {
    let s = ctx.rt.env_stats().await?;
    let env = ctx.rt.get_env().await?;
    macro_rules! names {
        ($m:expr) => {
            (&$m).into_iter().map(|(_, names)| names.len()).sum::<usize>()
        };
    }
    Ok([
        s.store_len,
        s.aliases_len,
        s.ref_var_keys,
        s.ref_var_total,
        s.by_id_len,
        env.trait_methods.len(),
        env.trait_defs.len(),
        names!(*env.impls),
        env.kept_traits.len(),
        env.poly_binds.len(),
        names!(env.lexical.binds),
        names!(env.lexical.typedefs),
        names!(env.lexical.traits),
        env.lexical.modules.len(),
        env.names.len(),
        env.kept_impls.len(),
        env.abstract_reps.len(),
    ])
}

/// The root binding `name`, which a test writes to drive a program.
async fn root_bind(ctx: &TestCtx, name: &str) -> Result<BindId> {
    let env = ctx.rt.get_env().await?;
    let root = &graphix_compiler::Scope::root().lexical;
    match env.lookup_bind(root, &[name].into())? {
        Some((_, bind)) => Ok(bind.id),
        None => bail!("{name} not in scope"),
    }
}

/// Whether a value is the tuple `(a, b)`.
fn is_pair(a: i64, b: i64) -> impl Fn(&Value) -> bool {
    move |v| matches!(v, Value::Array(t) if t[..] == [Value::I64(a), Value::I64(b)])
}

/// `RELOADS` reloads of a dynamic module whose source is `body` after
/// `let v = [tick];` and whose interface declares `sig` before `v` and
/// `get`, each load's `get` saved and called through the next reload,
/// which retires the body it was made in, then replaced by the new
/// load's. `get(x)` is `x + v`. `outside` is declared at the root, where
/// the sig imports from.
async fn saved_across_reloads(
    mode: Mode,
    outside: &str,
    sig: &str,
    body: &str,
) -> Result<()> {
    reloads(
        mode,
        outside,
        sig,
        body,
        "let saved: fn(x: i64) -> i64 = never(); saved <- d::get; (d::v, saved(d::v))",
    )
    .await
}

/// [`saved_across_reloads`], the program's tail after the module `tail`.
async fn reloads(
    mode: Mode,
    outside: &str,
    sig: &str,
    body: &str,
    tail: &str,
) -> Result<()> {
    const RELOADS: i64 = 1000;
    let (ctx, mut rx) = fixture_runtime([], crate::TEST_REGISTER, mode, |_| {}).await?;
    let roots = ArcStr::from(format!("let tick: i64 = 0; {outside}"));
    let _roots = ctx.rt.compile(roots).await?;
    let program = format!(
        r#"{{
          let src = "let v = [tick]; {body}";
          let s = mod d dynamic {{
              sig {{ {sig} val v: i64; val get: fn(x: i64) -> i64 }};
              source src
          }};
          {tail}
        }}"#
    );
    let (eid, _expr) = compile_one(&ctx, program.as_str()).await?;
    let tick = root_bind(&ctx, "tick").await?;
    let at = |n: i64| is_pair(n, 2 * n);
    for n in 1..=20 {
        ctx.rt.set(tick, n)?;
        await_update(&ctx, &mut rx, eid, at(n)).await?;
    }
    let early = held(&ctx).await?;
    for n in 21..=20 + RELOADS {
        ctx.rt.set(tick, n)?;
        await_update(&ctx, &mut rx, eid, at(n)).await?;
    }
    let late = held(&ctx).await?;
    ctx.shutdown().await;
    assert_eq!(early, late, "{mode:?}: grew over {RELOADS} reloads");
    Ok(())
}

/// A retired binding's value, readers and alias go with the last function
/// that names it.
async fn a_function_saved_across_reloads_keeps_nothing_once_dropped(
    mode: Mode,
) -> Result<()> {
    saved_across_reloads(mode, "", "", "let get = |x: i64| x + v").await
}

/// A retired body's dispatchers, defaults and impl methods go with the
/// last function made in it.
async fn a_dispatching_function_saved_across_reloads_keeps_nothing_once_dropped(
    mode: Mode,
) -> Result<()> {
    saved_across_reloads(
        mode,
        "",
        "",
        "trait T { val d: fn(self) -> i64; val e: fn(self) -> i64 = |s| d(s) }; \
         type C = Abstract<i64>; impl T for C { let d = |c: C| c.0 + v }; \
         let get = |x: i64| T::e(C(x))",
    )
    .await
}

/// Each load's type is its own, and so is its `Eq`, which decides the
/// saved function's comparisons past the reload: the representation and
/// the impl go with the last function made in the load.
async fn a_function_saved_across_reloads_keeps_its_loads_type_no_longer(
    mode: Mode,
) -> Result<()> {
    saved_across_reloads(
        mode,
        "",
        "use core;",
        "type C = Abstract<i64>; impl Eq for C { let eq = |a, b| a.0 != b.0 }; \
         let get = |x: i64| select C(x) == C(x) { true => 0, false => x + v }",
    )
    .await
}

/// A load's type and its `Eq` go with the load at the reload that
/// retires it, where nothing made in the load is saved.
async fn a_reloaded_module_keeps_its_retired_loads_type_no_longer(
    mode: Mode,
) -> Result<()> {
    reloads(
        mode,
        "",
        "use core;",
        "type C = Abstract<i64>; impl Eq for C { let eq = |a, b| a.0 != b.0 }; \
         let get = |x: i64| select C(x) == C(x) { true => 0, false => x + v }",
        "(d::v, d::get(d::v))",
    )
    .await
}

/// How many impls and abstract types the env registers.
async fn impls_and_types(ctx: &TestCtx) -> Result<(usize, usize)> {
    let env = ctx.rt.get_env().await?;
    let impls = (&*env.impls).into_iter().map(|(_, l)| l.len()).sum();
    Ok((impls, env.abstract_reps.len()))
}

/// A load's type keeps its representation and its `Eq` past the reload
/// that retires the load while a function made in it lives, and loses
/// them once that function goes, with no reload after.
async fn a_retired_loads_type_goes_with_its_last_function(mode: Mode) -> Result<()> {
    let (ctx, mut rx) = fixture_runtime([], crate::TEST_REGISTER, mode, |_| {}).await?;
    let roots = literal!("let tick: i64 = 0; let held: fn(x: i64) -> i64 = never();");
    let _roots = ctx.rt.compile(roots).await?;
    let before = impls_and_types(&ctx).await?;
    let _module = ctx
        .rt
        .compile(literal!(
            r#"let src = select tick {
                 1 => "type C = Abstract<i64>; impl Eq for C { let eq = |a, b| a.0 != b.0 }; let get = |x: i64| select C(x) == C(x) { true => 0, false => x + 1 }",
                 _ => "let get = |x: i64| x * 10"
               };
               let s = mod d dynamic { sig { use core; val get: fn(x: i64) -> i64 }; source src };"#
        ))
        .await?;
    let tick = root_bind(&ctx, "tick").await?;
    let (e_held, _held) = compile_one(&ctx, "(tick, held(tick))").await?;
    ctx.rt.set(tick, 1i64)?;
    let (id, w) = compile_one(&ctx, "{ held <- d::get; d::get(tick) }").await?;
    await_update(&ctx, &mut rx, id, |v| *v == Value::I64(2))
        .await
        .context("first load")?;
    drop(w);
    for n in 2..=5i64 {
        ctx.rt.set(tick, n)?;
        await_update(&ctx, &mut rx, e_held, is_pair(n, n + 1)).await.context("held")?;
        let kept = impls_and_types(&ctx).await?;
        assert_eq!(kept, (before.0 + 1, before.1 + 1), "{mode:?}: dropped at {n}");
    }
    let (id, w) = compile_one(&ctx, "{ held <- d::get; d::get(tick) }").await?;
    await_update(&ctx, &mut rx, id, |v| *v == Value::I64(50))
        .await
        .context("later load")?;
    drop(w);
    let mut n = 5;
    while impls_and_types(&ctx).await? != before && n < 100 {
        n += 1;
        ctx.rt.set(tick, n)?;
        await_update(&ctx, &mut rx, e_held, is_pair(n, 10 * n)).await?;
    }
    let after = impls_and_types(&ctx).await?;
    ctx.shutdown().await;
    assert_eq!(after, before, "{mode:?}: held past its last function");
    Ok(())
}

/// A deleted module's load keeps its type and its `Eq` while a function
/// made in it lives, and loses them once that function goes.
async fn a_deleted_modules_type_goes_with_its_last_function(mode: Mode) -> Result<()> {
    let (ctx, mut rx) = fixture_runtime([], crate::TEST_REGISTER, mode, |_| {}).await?;
    let roots = literal!("let tick: i64 = 0; let held: fn(x: i64) -> i64 = never();");
    let _roots = ctx.rt.compile(roots).await?;
    let before = impls_and_types(&ctx).await?;
    let tick = root_bind(&ctx, "tick").await?;
    let (e_held, _held) = compile_one(&ctx, "(tick, held(tick))").await?;
    let (id, module) = compile_one(
        &ctx,
        r#"{
             let s = mod d dynamic {
               sig { use core; val get: fn(x: i64) -> i64 };
               source "type C = Abstract<i64>; impl Eq for C { let eq = |a, b| a.0 != b.0 }; let get = |x: i64| select C(x) == C(x) { true => 0, false => x + 1 }"
             };
             held <- d::get;
             d::get(tick)
           }"#,
    )
    .await?;
    ctx.rt.set(tick, 1i64)?;
    await_update(&ctx, &mut rx, id, |v| *v == Value::I64(2)).await.context("load")?;
    drop(module);
    for n in 2..=5i64 {
        ctx.rt.set(tick, n)?;
        await_update(&ctx, &mut rx, e_held, is_pair(n, n + 1)).await.context("held")?;
        let kept = impls_and_types(&ctx).await?;
        assert_eq!(kept, (before.0 + 1, before.1 + 1), "{mode:?}: dropped at {n}");
    }
    let (_, w) = compile_one(&ctx, "held <- |x: i64| 10 * x").await?;
    let mut n = 5;
    while impls_and_types(&ctx).await? != before && n < 100 {
        n += 1;
        ctx.rt.set(tick, n)?;
        await_update(&ctx, &mut rx, e_held, is_pair(n, 10 * n)).await?;
    }
    drop(w);
    let after = impls_and_types(&ctx).await?;
    ctx.shutdown().await;
    assert_eq!(after, before, "{mode:?}: held past its last function");
    Ok(())
}

/// A function outside the module that each load calls is instantiated in
/// the load, and the closure it returns is a function of the load's: the
/// instances and the closure keep the load's impl no longer than the
/// saved function does.
async fn a_saved_function_calling_an_outside_function_keeps_nothing_once_dropped(
    mode: Mode,
) -> Result<()> {
    saved_across_reloads(
        mode,
        "trait Sh { val sh: fn(self) -> i64 }; \
         let app = |x: Sh| Sh::sh(x); let mk = |x: Sh| |u: i64| Sh::sh(x) + u;",
        "use super::{Sh, app, mk};",
        "use super::{Sh, app, mk}; \
         type C = Abstract<i64>; impl Sh for C { let sh = |c: C| c.0 + v }; \
         let add = mk(C(0)); let get = |x: i64| app(C(x)) + add(0) - v",
    )
    .await
}

/// The outside function is another dynamic module's: the instance each
/// load makes of it is made in a join of the two loads, which the saved
/// function keeps no longer than it keeps its own.
async fn a_saved_function_calling_another_modules_function_keeps_nothing_once_dropped(
    mode: Mode,
) -> Result<()> {
    saved_across_reloads(
        mode,
        "trait Sh { val sh: fn(self) -> i64 }; \
         let sa = mod a dynamic { sig { use super::Sh; val app: fn(x: Sh) -> i64 }; \
         source \"use super::Sh; let app = |x: Sh| Sh::sh(x)\" };",
        "use super::{Sh, a};",
        "use super::{Sh, a}; \
         type C = Abstract<i64>; impl Sh for C { let sh = |c: C| c.0 + v }; \
         let get = |x: i64| a::app(C(x))",
    )
    .await
}

/// The same where the interface declares the trait, the type and the
/// impl, so each load's methods join the declared entry's.
async fn a_function_saved_past_a_declared_impl_keeps_nothing_once_dropped(
    mode: Mode,
) -> Result<()> {
    saved_across_reloads(
        mode,
        "",
        "trait T { val d: fn(self) -> i64; val e: fn(self) -> i64 = |s| d(s) }; \
         type C; impl T for C;",
        "type C = Abstract<i64>; impl T for C { let d = |c: C| c.0 + v }; \
         let get = |x: i64| T::e(C(x))",
    )
    .await
}

/// The same where each load declares a module whose interface declares
/// the trait and the impl: the retired load keeps what that interface
/// registered while the saved function names it, and no longer.
async fn a_function_saved_across_reloads_around_an_interface_module_keeps_nothing_once_dropped(
    mode: Mode,
) -> Result<()> {
    saved_across_reloads(
        mode,
        "",
        "",
        "let r = mod inner dynamic { sig { trait T { val d: fn(self) -> i64; val e: fn(self) -> i64 = |s| d(s) }; \
         type C; impl T for C; val mk: fn(x: i64) -> C }; \
         source \\\"type C = Abstract<i64>; impl T for C { let d = |c: C| c.0 }; \
         let mk = |x: i64| C(x)\\\" }; \
         let get = |x: i64| inner::T::e(inner::mk(x)) + v",
    )
    .await
}

/// `ROUNDS` slots of a function whose body declares an interface module,
/// each born, its `m::get` saved and called once the slot is deleted,
/// then replaced by the next slot's.
async fn a_function_escaping_deleted_interface_modules_keeps_nothing_once_dropped(
    mode: Mode,
) -> Result<()> {
    const ROUNDS: i64 = 1000;
    let files = [
        ("/m.gxi", VfsEntry::from(literal!("val v: i64; val get: fn(x: i64) -> i64"))),
        ("/m.gx", VfsEntry::from(literal!("let v = 1; let get = |x: i64| x + v"))),
    ];
    let (ctx, mut rx) =
        fixture_runtime(files, crate::TEST_REGISTER, mode, |_| {}).await?;
    let _tick = ctx.rt.compile(literal!("let tick: i64 = 0;")).await?;
    let (eid, _expr) = compile_one(
        &ctx,
        r#"{
          let f = |k: i64| { mod m; m::v <- once(k) ~ k * 100; m::get };
          let xs = select tick % 2 { 0 => [tick], _ => [] };
          let fs = array::map(xs, |x| f(x));
          let saved: fn(x: i64) -> i64 = never();
          saved <- fs[0]$;
          (tick, saved(tick))
        }"#,
    )
    .await?;
    let tick = root_bind(&ctx, "tick").await?;
    // the slot born at an even tick writes its `v` a hundred times the
    // tick, and `saved` is its `get` until the next even tick
    let at = |n: i64| is_pair(n, n + 100 * (n - n % 2));
    for n in 1..=20 {
        ctx.rt.set(tick, n)?;
        await_update(&ctx, &mut rx, eid, at(n)).await?;
    }
    let early = held(&ctx).await?;
    for n in 21..=20 + 2 * ROUNDS {
        ctx.rt.set(tick, n)?;
        await_update(&ctx, &mut rx, eid, at(n)).await?;
    }
    let late = held(&ctx).await?;
    ctx.shutdown().await;
    assert_eq!(early, late, "{mode:?}: grew over {ROUNDS} slots");
    Ok(())
}

/// `ROUNDS` slots of a function whose body declares an interface module
/// and returns a reader and a writer of each side of its `v`: the
/// module's own `get` and `set`, and two made outside it over `m::v`.
/// The test holds each slot's four in root bindings past the slot's
/// deletion, where the two sides read and write one variable, then drops
/// one side, the val's and the binding's by turns: the other goes on from
/// the value they shared, in a call born after the drop.
async fn both_sides_of_deleted_interface_modules_keep_nothing_once_dropped(
    mode: Mode,
) -> Result<()> {
    const ROUNDS: i64 = 40;
    let files = [
        (
            "/m.gxi",
            VfsEntry::from(literal!(
                "val v: i64; val get: fn(x: i64) -> i64; val set: fn(x: i64) -> null"
            )),
        ),
        (
            "/m.gx",
            VfsEntry::from(literal!(
                "let v = 1; let get = |x: i64| x + v; let set = |x: i64| -> null { v <- x; null }"
            )),
        ),
    ];
    let (ctx, mut rx) =
        fixture_runtime(files, crate::TEST_REGISTER, mode, |_| {}).await?;
    let _roots = ctx
        .rt
        .compile(literal!(
            r#"
            let tick: i64 = 0;
            let ys: Array<i64> = [];
            let wv: i64 = never();
            let sv: i64 = never();
            let g: fn(x: i64) -> i64 = never();
            let o: fn(x: i64) -> i64 = never();
            let s: fn(x: i64) -> null = never();
            let w: fn(x: i64) -> null = never();
            let f = |k: i64| {
                mod m;
                (m::get, m::set, |x: i64| x + m::v, |x: i64| -> null { m::v <- x; null })
            };
            s(sv);
            w(wv)
            "#
        ))
        .await?;
    let (e_gone, _gone) =
        compile_one(&ctx, "(|x: i64| x, |x: i64| -> null null)").await?;
    let gone = match await_value(&ctx, &mut rx, e_gone, |_| true).await? {
        Value::Array(gone) => gone,
        v => bail!("expected two functions, got {v:?}"),
    };
    let (e_slots, _slots) = compile_one(
        &ctx,
        "array::map(select tick % 2 { 1 => [tick], _ => [] }, |x| f(x))",
    )
    .await?;
    let (e_probe, _probe) = compile_one(&ctx, "(g(1), o(1))").await?;
    let (e_late, _late) = compile_one(&ctx, "array::map(ys, |y| (g(y), o(y)))").await?;
    let mut ids = [BindId::new(); 8];
    for (id, name) in ids.iter_mut().zip(["tick", "ys", "wv", "sv", "g", "s", "o", "w"]) {
        *id = root_bind(&ctx, name).await?
    }
    let [tick, ys, wv, sv, g, s, o, w] = ids;
    let empty = |v: &Value| matches!(v, Value::Array(a) if a.is_empty());
    let late = |a: i64, b: i64| move |v: &Value| matches!(v, Value::Array(s) if s.len() == 1 && is_pair(a, b)(&s[0]));
    let one = Value::Array([Value::I64(0)].into_iter().collect());
    let none = Value::Array([].into_iter().collect());
    let mut early = None;
    for r in 1..=ROUNDS {
        ctx.rt.set(tick, 2 * r - 1)?;
        let born = |v: &Value| matches!(v, Value::Array(a) if a.len() == 1);
        match await_value(&ctx, &mut rx, e_slots, born).await? {
            Value::Array(slots) => match &slots[0] {
                Value::Array(four) => {
                    for (id, f) in [g, s, o, w].into_iter().zip(four.iter()) {
                        ctx.rt.set(id, f.clone())?
                    }
                }
                v => bail!("expected four functions, got {v:?}"),
            },
            v => bail!("expected a slot, got {v:?}"),
        }
        ctx.rt.set(tick, 2 * r)?;
        await_update(&ctx, &mut rx, e_slots, empty).await?;
        ctx.rt.set(wv, 700 + r)?;
        ctx.rt.set(ys, one.clone())?;
        await_update(&ctx, &mut rx, e_late, late(700 + r, 700 + r)).await?;
        ctx.rt.set(ys, none.clone())?;
        await_update(&ctx, &mut rx, e_late, empty).await?;
        let val_first = r % 2 == 0;
        let (first, second) = if val_first { ([o, w], [g, s]) } else { ([g, s], [o, w]) };
        for (id, f) in first.into_iter().zip(gone.iter()) {
            ctx.rt.set(id, f.clone())?
        }
        let dropped = move |v: &Value| match v {
            Value::Array(p) => p[val_first as usize] == Value::I64(1),
            _ => false,
        };
        await_update(&ctx, &mut rx, e_probe, dropped).await?;
        ctx.rt.set(ys, one.clone())?;
        let kept = |n: i64| if val_first { late(n, 0) } else { late(0, n) };
        await_update(&ctx, &mut rx, e_late, kept(700 + r)).await?;
        ctx.rt.set(if val_first { sv } else { wv }, 5000 + r)?;
        await_update(&ctx, &mut rx, e_late, kept(5000 + r)).await?;
        ctx.rt.set(ys, none.clone())?;
        await_update(&ctx, &mut rx, e_late, empty).await?;
        for (id, f) in second.into_iter().zip(gone.iter()) {
            ctx.rt.set(id, f.clone())?
        }
        await_update(&ctx, &mut rx, e_probe, is_pair(1, 1)).await?;
        if r == 4 {
            early = Some(held(&ctx).await?)
        }
    }
    let late = held(&ctx).await?;
    ctx.shutdown().await;
    assert_eq!(early, Some(late), "{mode:?}: grew over {ROUNDS} slots");
    Ok(())
}

/// A slot's module holds a recursive `get` over its `v`; the slot's body
/// holds a recursive `h` over `m::get`, which nothing else names. The
/// test alone holds `get` past the slot's deletion: `h` is collected, and
/// `get`, called for the first time cycles later, still reads `v`.
async fn a_held_function_outlives_a_collected_reader_of_its_val(
    mode: Mode,
) -> Result<()> {
    let files = [
        ("/m.gxi", VfsEntry::from(literal!("val get: fn(x: i64) -> i64"))),
        (
            "/m.gx",
            VfsEntry::from(literal!(
                "let v = 1; let rec get = |x: i64| -> i64 select x { n if n > 0 => get(n - 1) + v, _ => 0 }"
            )),
        ),
    ];
    let (ctx, mut rx) =
        fixture_runtime(files, crate::TEST_REGISTER, mode, |_| {}).await?;
    let _roots = ctx
        .rt
        .compile(literal!(
            r#"
            let tick: i64 = 0;
            let ys: Array<i64> = [];
            let g: fn(x: i64) -> i64 = never();
            let f = |k: i64| {
                mod m;
                let rec h = |x: i64| -> i64 select x { n if n > 0 => h(n - 1) + m::get(1), _ => 0 };
                m::get
            }
            "#
        ))
        .await?;
    let (e_slots, _slots) = compile_one(
        &ctx,
        "array::map(select tick % 2 { 1 => [tick], _ => [] }, |x| f(x))",
    )
    .await?;
    let (e_late, _late) = compile_one(&ctx, "array::map(ys, |y| g(y))").await?;
    let (tick, ys, g) = (
        root_bind(&ctx, "tick").await?,
        root_bind(&ctx, "ys").await?,
        root_bind(&ctx, "g").await?,
    );
    let empty = |v: &Value| matches!(v, Value::Array(a) if a.is_empty());
    ctx.rt.set(tick, 1i64)?;
    let born = |v: &Value| matches!(v, Value::Array(a) if a.len() == 1);
    let get = match await_value(&ctx, &mut rx, e_slots, born).await? {
        Value::Array(slots) => slots[0].clone(),
        v => bail!("expected a slot, got {v:?}"),
    };
    for n in 1..=8i64 {
        ctx.rt.set(tick, 2 * n)?;
        await_update(&ctx, &mut rx, e_slots, empty).await?;
    }
    ctx.rt.set(g, get)?;
    ctx.rt.set(ys, Value::Array([Value::I64(3)].into_iter().collect()))?;
    let three = |v: &Value| matches!(v, Value::Array(a) if a[..] == [Value::I64(3)]);
    await_update(&ctx, &mut rx, e_late, three).await?;
    ctx.shutdown().await;
    Ok(())
}

/// Compile `code` and drop it once its last expression is `want`, 50
/// times over: the same text compiles each time, and the session holds
/// after the last what it held after the fifth.
async fn compiled_and_dropped<const N: usize>(
    files: [(&'static str, &'static str); N],
    code: &'static str,
    want: Value,
) -> Result<()> {
    const ROUNDS: usize = 50;
    let files = files.map(|(path, text)| (path, VfsEntry::from(ArcStr::from(text))));
    let (ctx, mut rx) =
        fixture_runtime(files, crate::TEST_REGISTER, Mode::Interp, |_| {}).await?;
    let mut early = None;
    for i in 0..5 + ROUNDS {
        if i == 5 {
            early = Some(held(&ctx).await?)
        }
        let res = match ctx.rt.compile(ArcStr::from(code)).await {
            Ok(r) => r,
            Err(e) => bail!("compile {i} failed: {e:?}"),
        };
        let eid = res.exprs[res.exprs.len() - 1].id;
        await_update(&ctx, &mut rx, eid, |v| *v == want).await?;
        drop(res);
    }
    let late = held(&ctx).await?;
    ctx.shutdown().await;
    assert_eq!(early, Some(late), "grew over {ROUNDS} compiles");
    Ok(())
}

/// A dropped dynamic module's interface `impl` of a core trait goes with
/// it: the declaration registers again at the next compile.
#[tokio::test(flavor = "current_thread")]
async fn a_dropped_module_takes_its_declared_core_impl() -> Result<()> {
    compiled_and_dropped(
        [],
        r#"{
            let status = mod m dynamic {
                sig { use core; type C; impl Cast for C; val make: fn(x: i64) -> C };
                source "type C = Abstract<i64>; impl Cast for C { let to_value = |c: C| c.0 * 10 }; let make = |x: i64| C(x)"
            };
            select status { error as e => never(dbg(e)), null as _ => json::write_str(m::make(3))$ }
        }"#,
        Value::String(literal!("{\"C\":30}")),
    )
    .await
}

/// The same for a trait the interface declares itself: its dispatchers,
/// its record, the impl's method bindings and the type's name go too.
#[tokio::test(flavor = "current_thread")]
async fn a_dropped_module_takes_its_declared_trait_and_impl() -> Result<()> {
    compiled_and_dropped(
        [],
        r#"{
            let status = mod m dynamic {
                sig { use core; trait Sh { val sh: fn(self) -> i64 }; type C; impl Sh for C; val make: fn(x: i64) -> C };
                source "type C = Abstract<i64>; impl Sh for C { let sh = |c: C| c.0 * 10 }; let make = |x: i64| C(x)"
            };
            select status { error as e => never(dbg(e)), null as _ => m::Sh::sh(m::make(3)) }
        }"#,
        Value::I64(30),
    )
    .await
}

/// A dropped module with an interface file leaves nothing its interface
/// declared, a submodule's names included.
#[tokio::test(flavor = "current_thread")]
async fn a_dropped_interface_module_takes_what_its_interface_declared() -> Result<()> {
    compiled_and_dropped(
        [
            (
                "/m/mod.gxi",
                "trait Sh { val sh: fn(self) -> i64; val twice: fn(self) -> i64 = |s| sh(s) * 2 }; \
                 type C; impl Sh for C; impl Cast for C; mod sub; \
                 val make: fn(x: i64) -> C",
            ),
            (
                "/m/mod.gx",
                "type C = Abstract<i64>; impl Sh for C { let sh = |c: C| c.0 * sub::ten }; \
                 impl Cast for C { let to_value = |c: C| Sh::sh(c) }; \
                 let make = |x: i64| C(x)",
            ),
            ("/m/sub.gxi", "trait Tw { val tw: fn(self) -> i64 }; type T = i64; val ten: T"),
            ("/m/sub.gx", "let ten = 10"),
        ],
        r#"{ mod m; "[m::Sh::twice(m::make(3))] [json::write_str(m::make(4))$] [m::sub::ten]" }"#,
        Value::String(literal!("60 {\"C\":40} 10")),
    )
    .await
}

/// A dropped block's trait whose default calls its own method goes with
/// the block: the default names the dispatcher that holds it.
#[tokio::test(flavor = "current_thread")]
async fn a_dropped_blocks_default_that_calls_its_trait_goes_with_it() -> Result<()> {
    compiled_and_dropped(
        [],
        "{ type C = Abstract<i64>; \
           trait Sh { val sh: fn(self) -> i64; val twice: fn(self) -> i64 = |s| sh(s) * 2 }; \
           impl Sh for C { let sh = |c: C| c.0 * 10 }; \
           Sh::twice(C(3)) }",
        Value::I64(60),
    )
    .await
}

/// A dropped block's import table goes with it.
#[tokio::test(flavor = "current_thread")]
async fn a_dropped_block_takes_its_import_table() -> Result<()> {
    compiled_and_dropped([], r#"{ use str::len; len("ab") }"#, Value::I64(2)).await
}

/// A function that left a dropped block builds its body against the
/// block's imports for as long as a value names it, and the block's
/// tables go with the function.
#[tokio::test(flavor = "current_thread")]
async fn a_function_escaping_a_dropped_block_keeps_its_imports_while_held() -> Result<()>
{
    const ROUNDS: i64 = 50;
    let (ctx, mut rx) =
        fixture_runtime([], crate::TEST_REGISTER, Mode::Interp, |_| {}).await?;
    let _g = ctx.rt.compile(literal!("let g: fn(s: string) -> i64 = never();")).await?;
    let is = |n: i64| move |v: &Value| *v == Value::I64(n);
    let mut early = None;
    for k in 0..5 + ROUNDS {
        if k == 5 {
            early = Some(held(&ctx).await?)
        }
        let block =
            format!("{{ use str::len; let f = |s: string| len(s) + {k}; g <- f; null }}");
        let (_, made) = compile_one(&ctx, block.as_str()).await?;
        let (written, call) = compile_one(&ctx, r#"g("ab")"#).await?;
        await_update(&ctx, &mut rx, written, is(2 + k)).await?;
        drop((made, call));
        // the block is gone, and this call is the first to build f's body
        // since
        let (late, call) = compile_one(&ctx, r#"g("abc")"#).await?;
        await_update(&ctx, &mut rx, late, is(3 + k)).await?;
        drop(call);
    }
    let late = held(&ctx).await?;
    ctx.shutdown().await;
    assert_eq!(early, Some(late), "grew over {ROUNDS} escaped functions");
    Ok(())
}

/// A value keeps the type its block imported once the block's table is
/// gone: later inputs read the type through the value, never the table,
/// which would now give them the root's type of the name.
#[tokio::test(flavor = "current_thread")]
async fn a_blocks_imported_type_outlives_its_table() -> Result<()> {
    let files = [
        ("/m.gxi", "type T = {a: i64}; type U<'a> = ['a, null]; val mk: fn(x: i64) -> T"),
        ("/m.gx", "let mk = |x: i64| -> T {a: x}"),
    ]
    .map(|(path, text)| (path, VfsEntry::from(ArcStr::from(text))));
    let (ctx, mut rx) =
        fixture_runtime(files, crate::TEST_REGISTER, Mode::Interp, |_| {}).await?;
    let before = ctx.rt.get_env().await?.names.len();
    let _held = ctx
        .rt
        .compile(literal!(
            "mod m;
             type T = string;
             let v = { use m::T; let x: T = m::mk(1); x };
             let none = { use m::{T, U}; let n: U<T> = null; n };
             let some = { use m::{T, U}; let s: U<T> = m::mk(2); s };
             let empty = { use m::T; let e: Array<T> = []; e };
             let held = { use m::T; let h: fn(x: i64) -> T = m::mk; h };
             let made = { use m::T; cast<T>(m::mk(5))$ }"
        ))
        .await?;
    for _ in 0..3 {
        let (id, res) = compile_one(&ctx, "{ use str::len; len(\"ab\") }").await?;
        await_update(&ctx, &mut rx, id, |v| *v == Value::I64(2)).await?;
        drop(res)
    }
    // the last block compiled keeps its table until the next compile
    let swept = ctx.rt.get_env().await?.names.len();
    assert!(swept <= before + 1, "the blocks' tables are held: {before} then {swept}");
    let (id, _late) = compile_one(
        &ctx,
        "{
          let f = |q: m::T| q.a;
          let w: m::U<m::T> = some;
          let es: Array<m::T> = array::push(empty, v);
          let a = select none { null as _ => 10, t => t.a };
          let b = select w { null as _ => 20, t => t.a };
          (f(v), a, b, array::len(es), held(3).a, made.a)
        }",
    )
    .await?;
    let want: Value =
        [1i64, 10, 2, 1, 3, 5].into_iter().map(Value::I64).collect::<Vec<_>>().into();
    await_update(&ctx, &mut rx, id, |v| *v == want).await?;
    ctx.shutdown().await;
    Ok(())
}

/// A callback in a block that stays builds each new slot's body against
/// the block's imports, whatever compiled since.
#[tokio::test(flavor = "current_thread")]
async fn a_live_blocks_callback_builds_new_slots_against_its_imports() -> Result<()> {
    let (ctx, mut rx) =
        fixture_runtime([], crate::TEST_REGISTER, Mode::Interp, |_| {}).await?;
    let array = |ns: &[i64]| -> Value {
        ns.iter().copied().map(Value::I64).collect::<Vec<_>>().into()
    };
    let _a = ctx.rt.compile(literal!("let a = [1, 2]")).await?;
    let (mapped, _live) =
        compile_one(&ctx, r#"{ use str::len; array::map(a, |x| len("ab") + x) }"#)
            .await?;
    let want = array(&[3, 4]);
    await_update(&ctx, &mut rx, mapped, |v| *v == want).await?;
    for _ in 0..3 {
        let (id, res) = compile_one(&ctx, "{ use str::len; len(\"ab\") }").await?;
        await_update(&ctx, &mut rx, id, |v| *v == Value::I64(2)).await?;
        drop(res)
    }
    let _grown = ctx.rt.compile(literal!("a <- [1, 2, 3, 4]")).await?;
    let want = array(&[3, 4, 5, 6]);
    await_update(&ctx, &mut rx, mapped, |v| *v == want).await?;
    ctx.shutdown().await;
    Ok(())
}

/// A module declared at the root is declared again once it is dropped.
#[tokio::test(flavor = "current_thread")]
async fn a_dropped_root_module_is_declared_again() -> Result<()> {
    compiled_and_dropped(
        [
            ("/m.gxi", "val make: fn(x: i64) -> i64"),
            ("/m.gx", "let make = |x: i64| x + 1"),
        ],
        "mod m; m::make(3)",
        Value::I64(4),
    )
    .await?;
    compiled_and_dropped(
        [],
        r#"mod m dynamic {
            sig { use core; trait Sh { val sh: fn(self) -> i64 }; type C; impl Sh for C; val make: fn(x: i64) -> C };
            source "type C = Abstract<i64>; impl Sh for C { let sh = |c: C| c.0 * 10 }; let make = |x: i64| C(x)"
        }"#,
        Value::Null,
    )
    .await
}

/// A file module with no interface, and the module and the type it
/// declares, are declared again once it is dropped, and hold nothing.
#[tokio::test(flavor = "current_thread")]
async fn a_dropped_root_file_module_without_an_interface_is_declared_again() -> Result<()>
{
    compiled_and_dropped(
        [
            ("/m.gx", "mod n; type T = i64; let make = |x: T| n::inc(x)"),
            ("/m/n.gx", "let inc = |x: i64| x + 1"),
        ],
        "mod m; m::make(3)",
        Value::I64(4),
    )
    .await
}

const DYNAMIC_LEN_MODULE: &str = r#"sig { use core; use str; val make: fn(s: string) -> i64 }; source "use str::len; let make = |s: string| len(s)""#;

/// Compile and drop a dynamic module at `/m` whose source writes `use
/// str::len`, and a unit after it, which takes the module's tables.
async fn a_dynamic_module_declared_and_dropped(
    ctx: &TestCtx,
    rx: &mut Events,
    name: &str,
) -> Result<()> {
    let (id, a) =
        compile_one(ctx, format!("mod {name} dynamic {{ {DYNAMIC_LEN_MODULE} }}"))
            .await?;
    await_update(ctx, rx, id, |v| *v == Value::Null).await?;
    drop(a);
    let (id, x) = compile_one(ctx, "1").await?;
    await_update(ctx, rx, id, |v| *v == Value::I64(1)).await?;
    drop(x);
    Ok(())
}

/// A deleted module's import table does not outlive it where its path
/// crosses no body: each of a session's dynamic modules, declared under a
/// name of its own and dropped, leaves none.
#[tokio::test(flavor = "current_thread")]
async fn a_dropped_module_at_the_root_takes_its_import_table() -> Result<()> {
    let (ctx, mut rx) =
        fixture_runtime([], crate::TEST_REGISTER, Mode::Interp, |_| {}).await?;
    let mut early = 0;
    for i in 0..25 {
        if i == 5 {
            early = ctx.rt.get_env().await?.names.len()
        }
        a_dynamic_module_declared_and_dropped(&ctx, &mut rx, &format!("m{i}")).await?;
    }
    let late = ctx.rt.get_env().await?.names.len();
    ctx.shutdown().await;
    assert_eq!(early, late, "the import tables of dropped modules are held");
    Ok(())
}

/// The module declared at a path reads no import table a deleted module
/// at the path left: `len` is the new module's own to import.
#[tokio::test(flavor = "current_thread")]
async fn a_module_declared_at_a_dropped_modules_path_reads_none_of_its_imports()
-> Result<()> {
    let one = |text: &'static str| [("/m.gx", VfsEntry::from(ArcStr::from(text)))];
    let (ctx, mut rx) = fixture_runtime(
        one("let make = |s: string| len(s)"),
        crate::TEST_REGISTER,
        Mode::Interp,
        |_| {},
    )
    .await?;
    a_dynamic_module_declared_and_dropped(&ctx, &mut rx, "m").await?;
    let Err(e) = ctx.rt.compile(literal!(r#"mod m; m::make("ab")"#)).await else {
        bail!("a module read the import table of a deleted one")
    };
    ctx.shutdown().await;
    let e = format!("{e:?}");
    assert!(e.contains("len not defined"), "{e}");
    let (ctx, mut rx) = fixture_runtime(
        one("use array::len; let make = |a: Array<i64>| len(a)"),
        crate::TEST_REGISTER,
        Mode::Interp,
        |_| {},
    )
    .await?;
    a_dynamic_module_declared_and_dropped(&ctx, &mut rx, "m").await?;
    let made = ctx.rt.compile(literal!("mod m; m::make([1, 2])")).await?;
    await_update(&ctx, &mut rx, made.exprs[1].id, |v| *v == Value::I64(2)).await?;
    ctx.shutdown().await;
    Ok(())
}

/// A function that left a dropped module builds its body against the
/// imports its source wrote, while a module declared at the path imports
/// the names of its own.
#[tokio::test(flavor = "current_thread")]
async fn a_function_escaping_a_dropped_module_keeps_its_imports_beside_a_new_module()
-> Result<()> {
    let files = [(
        "/m.gx",
        VfsEntry::from(ArcStr::from("use array::len; let make = |a: Array<i64>| len(a)")),
    )];
    let (ctx, mut rx) =
        fixture_runtime(files, crate::TEST_REGISTER, Mode::Interp, |_| {}).await?;
    let _g = ctx.rt.compile(literal!("let g: fn(s: string) -> i64 = never();")).await?;
    let (id, a) = compile_one(
        &ctx,
        format!("mod m dynamic {{ {DYNAMIC_LEN_MODULE} }}; g <- m::make; null"),
    )
    .await?;
    await_update(&ctx, &mut rx, id, |v| *v == Value::Null).await?;
    let (id, read) = compile_one(&ctx, "{ let f = g; 7 }").await?;
    await_update(&ctx, &mut rx, id, |v| *v == Value::I64(7)).await?;
    drop((a, read));
    let (id, x) = compile_one(&ctx, "1").await?;
    await_update(&ctx, &mut rx, id, |v| *v == Value::I64(1)).await?;
    drop(x);
    let made = ctx.rt.compile(literal!("mod m; m::make([1, 2])")).await?;
    await_update(&ctx, &mut rx, made.exprs[1].id, |v| *v == Value::I64(2)).await?;
    let late = ctx.rt.compile(literal!(r#"g("abc")"#)).await?;
    await_update(&ctx, &mut rx, late.exprs[0].id, |v| *v == Value::I64(3)).await?;
    ctx.shutdown().await;
    Ok(())
}

/// A function that left a dropped file module builds its body against the
/// imports the file wrote then, where the module declared again at the path
/// reads the file as since edited: the old input's views read the table the
/// new module displaced.
#[tokio::test(flavor = "current_thread")]
async fn a_function_escaping_a_dropped_file_module_keeps_its_imports_past_an_edit()
-> Result<()> {
    let dir = tempfile::tempdir()?;
    let m = dir.path().join("m.gx");
    std::fs::write(dir.path().join("m.gxi"), "val make: fn(s: string) -> i64")?;
    std::fs::write(&m, "use str::len; let make = |s: string| len(s)")?;
    let (tx, mut rx) = mpsc::channel(1024);
    let files = FilesResolver::new(dir.path().to_path_buf(), None);
    let ctx =
        TestCtx::start(config(tx, crate::TEST_REGISTER, vec![files], |_| {})?).await?;
    let _g = ctx.rt.compile(literal!("let g: fn(s: string) -> i64 = never();")).await?;
    let a = ctx.rt.compile(literal!("mod m; g <- m::make; null")).await?;
    await_update(&ctx, &mut rx, a.exprs[2].id, |v| *v == Value::Null).await?;
    let (id, read) = compile_one(&ctx, "{ let f = g; 7 }").await?;
    await_update(&ctx, &mut rx, id, |v| *v == Value::I64(7)).await?;
    drop((a, read));
    let (id, x) = compile_one(&ctx, "1").await?;
    await_update(&ctx, &mut rx, id, |v| *v == Value::I64(1)).await?;
    drop(x);
    std::fs::write(&m, "use array::len; let make = |s: string| len([s, s, s, s])")?;
    let made = ctx.rt.compile(literal!(r#"mod m; m::make("ab")"#)).await?;
    await_update(&ctx, &mut rx, made.exprs[1].id, |v| *v == Value::I64(4)).await?;
    let late = ctx.rt.compile(literal!(r#"g("abc")"#)).await?;
    await_update(&ctx, &mut rx, late.exprs[0].id, |v| *v == Value::I64(3)).await?;
    ctx.shutdown().await;
    Ok(())
}

/// A function that left a dropped static module builds its body against the
/// imports its file wrote, while a dynamic module loaded at the path writes
/// tables of its own there.
#[tokio::test(flavor = "current_thread")]
async fn a_function_escaping_a_dropped_module_keeps_its_imports_beside_a_dynamic_load()
-> Result<()> {
    let files = [
        ("/m.gxi", VfsEntry::from(ArcStr::from("val make: fn(s: string) -> i64"))),
        (
            "/m.gx",
            VfsEntry::from(ArcStr::from("use str::len; let make = |s: string| len(s)")),
        ),
    ];
    let (ctx, mut rx) =
        fixture_runtime(files, crate::TEST_REGISTER, Mode::Interp, |_| {}).await?;
    let _g = ctx.rt.compile(literal!("let g: fn(s: string) -> i64 = never();")).await?;
    let a = ctx.rt.compile(literal!("mod m; g <- m::make; null")).await?;
    await_update(&ctx, &mut rx, a.exprs[2].id, |v| *v == Value::Null).await?;
    let (id, read) = compile_one(&ctx, "{ let f = g; 7 }").await?;
    await_update(&ctx, &mut rx, id, |v| *v == Value::I64(7)).await?;
    drop((a, read));
    let (id, x) = compile_one(&ctx, "1").await?;
    await_update(&ctx, &mut rx, id, |v| *v == Value::I64(1)).await?;
    drop(x);
    let made = ctx
        .rt
        .compile(literal!(
            r#"mod m dynamic { sig { use core; use array; val make: fn(a: Array<i64>) -> i64 }; source "use array::len; let make = |a: Array<i64>| len(a)" }; m::make([1, 2])"#
        ))
        .await?;
    await_update(&ctx, &mut rx, made.exprs[1].id, |v| *v == Value::I64(2)).await?;
    let late = ctx.rt.compile(literal!(r#"g("abc")"#)).await?;
    await_update(&ctx, &mut rx, late.exprs[0].id, |v| *v == Value::I64(3)).await?;
    ctx.shutdown().await;
    Ok(())
}

/// The same where the dynamic module's first source fails to load: the
/// function keeps its imports through the refused load and the load after.
#[tokio::test(flavor = "current_thread")]
async fn a_function_escaping_a_dropped_module_keeps_its_imports_past_a_refused_dynamic_load()
-> Result<()> {
    let files = [
        ("/m.gxi", VfsEntry::from(ArcStr::from("val make: fn(s: string) -> i64"))),
        (
            "/m.gx",
            VfsEntry::from(ArcStr::from("use str::len; let make = |s: string| len(s)")),
        ),
    ];
    let (ctx, mut rx) =
        fixture_runtime(files, crate::TEST_REGISTER, Mode::Interp, |_| {}).await?;
    let _g = ctx.rt.compile(literal!("let g: fn(s: string) -> i64 = never();")).await?;
    let a = ctx.rt.compile(literal!("mod m; g <- m::make; null")).await?;
    await_update(&ctx, &mut rx, a.exprs[2].id, |v| *v == Value::Null).await?;
    let (id, read) = compile_one(&ctx, "{ let f = g; 7 }").await?;
    await_update(&ctx, &mut rx, id, |v| *v == Value::I64(7)).await?;
    drop((a, read));
    let (id, x) = compile_one(&ctx, "1").await?;
    await_update(&ctx, &mut rx, id, |v| *v == Value::I64(1)).await?;
    drop(x);
    let refused = ctx
        .rt
        .compile(literal!(
            r#"{ let status = mod m dynamic { sig { use core; use array; val make: fn(a: Array<i64>) -> i64 }; source "use array::len; let make = |a: Array<i64>| len(a); let zz = missing" }; select status { error as e => e, null as _ => never() } }"#
        ))
        .await?;
    await_update(&ctx, &mut rx, refused.exprs[0].id, |v| matches!(v, Value::Error(_)))
        .await?;
    let late = ctx.rt.compile(literal!(r#"g("abc")"#)).await?;
    await_update(&ctx, &mut rx, late.exprs[0].id, |v| *v == Value::I64(3)).await?;
    drop(refused);
    let made = ctx
        .rt
        .compile(literal!(
            r#"mod m dynamic { sig { use core; use array; val make: fn(a: Array<i64>) -> i64 }; source "use array::len; let make = |a: Array<i64>| len(a)" }; m::make([1, 2])"#
        ))
        .await?;
    await_update(&ctx, &mut rx, made.exprs[1].id, |v| *v == Value::I64(2)).await?;
    let late = ctx.rt.compile(literal!(r#"g("abcd")"#)).await?;
    await_update(&ctx, &mut rx, late.exprs[0].id, |v| *v == Value::I64(4)).await?;
    ctx.shutdown().await;
    Ok(())
}

/// `ROUNDS` times over: compile `code`, which writes a function to the
/// root `saved`, drop it, and call `saved(3)` at a site compiled after the
/// drop, the first to build the function's body since. Each call answers
/// 30, and the session holds after the last round what it held after the
/// fifth: what the dropped expression declared goes with the function.
async fn escaped_and_called<const N: usize>(
    mode: Mode,
    files: [(&'static str, &'static str); N],
    code: &'static str,
) -> Result<()> {
    escaped_and_called_over(mode, files, "", code).await
}

/// [`escaped_and_called`] with `prelude` compiled first and kept: what
/// the rounds' expression declares is beside it.
async fn escaped_and_called_over<const N: usize>(
    mode: Mode,
    files: [(&'static str, &'static str); N],
    prelude: &'static str,
    code: &'static str,
) -> Result<()> {
    const ROUNDS: usize = 30;
    let files = files.map(|(path, text)| (path, VfsEntry::from(ArcStr::from(text))));
    let (ctx, mut rx) =
        fixture_runtime(files, crate::TEST_REGISTER, mode, |_| {}).await?;
    let _saved =
        ctx.rt.compile(literal!("let saved: fn(x: i64) -> i64 = never();")).await?;
    let _prelude = ctx.rt.compile(ArcStr::from(prelude)).await?;
    let mut early = None;
    for i in 0..5 + ROUNDS {
        if i == 5 {
            early = Some(held(&ctx).await?)
        }
        let made = match ctx.rt.compile(ArcStr::from(code)).await {
            Ok(r) => r,
            Err(e) => bail!("compile {i} failed: {e:?}"),
        };
        let eid = made.exprs[made.exprs.len() - 1].id;
        await_update(&ctx, &mut rx, eid, |v| *v == Value::I64(1)).await?;
        let (landed, read) = compile_one(&ctx, "{ let f = saved; 7 }").await?;
        await_update(&ctx, &mut rx, landed, |v| *v == Value::I64(7)).await?;
        drop((made, read));
        let (late, call) = compile_one(&ctx, "saved(3)").await?;
        await_update(&ctx, &mut rx, late, |v| *v == Value::I64(30)).await?;
        drop(call);
    }
    let late = held(&ctx).await?;
    ctx.shutdown().await;
    assert_eq!(early, Some(late), "{mode:?}: grew over {ROUNDS} escaped functions");
    Ok(())
}

/// A function that left a dropped block dispatches through the trait and
/// the impl the block declared, wherever it is first called.
async fn a_function_escaping_a_dropped_block_dispatches_to_its_impl(
    mode: Mode,
) -> Result<()> {
    escaped_and_called(
        mode,
        [],
        "{ type C = Abstract<i64>; trait Sh { val sh: fn(self) -> i64 }; \
         impl Sh for C { let sh = |c: C| c.0 * 10 }; \
         saved <- |x: i64| Sh::sh(C(x)); 1 }",
    )
    .await
}

/// The same through a default, of an impl that writes its other method
/// and of one that writes none, and through a generic function and a
/// union.
async fn a_function_escaping_a_dropped_block_dispatches_to_its_defaults(
    mode: Mode,
) -> Result<()> {
    escaped_and_called(
        mode,
        [],
        "{ type C = Abstract<i64>; type D = Abstract<i64>; \
         trait Sh { val sh: fn(self) -> i64 = |s| 3; \
                    val twice: fn(self) -> i64 = |s| sh(s) * 2 }; \
         impl Sh for C { let sh = |c: C| c.0 * 2 }; impl Sh for D {}; \
         let app = |v: Sh| Sh::twice(v); \
         let u: [C, D] = D(0); \
         saved <- |x: i64| { let g = |y: i64| app(C(y)) + Sh::twice(u); \
                             g(x) + app(D(x)) + Sh::sh(u) + Sh::sh(D(x)) }; \
         1 }",
    )
    .await
}

const SH_INTERFACE: &str = "trait Sh { val sh: fn(self) -> i64 }; type C; impl Sh for C; \
                            val get: fn(x: i64) -> i64; val make: fn(x: i64) -> C";
const SH_BODY: &str = "type C = Abstract<i64>; impl Sh for C { let sh = |c: C| c.0 * 10 }; \
                       let get = |x: i64| Sh::sh(C(x)); let make = |x: i64| C(x)";

/// The same for a function of a dropped module whose interface file
/// declares the trait and the impl.
async fn a_function_escaping_a_dropped_interface_module_dispatches_to_its_impl(
    mode: Mode,
) -> Result<()> {
    escaped_and_called(
        mode,
        [("/m.gxi", SH_INTERFACE), ("/m.gx", SH_BODY)],
        "{ mod m; saved <- m::get; 1 }",
    )
    .await
}

/// The same for a function made outside the module that calls the
/// interface's trait.
async fn a_function_over_a_dropped_interface_modules_trait_dispatches_to_its_impl(
    mode: Mode,
) -> Result<()> {
    escaped_and_called(
        mode,
        [("/m.gxi", SH_INTERFACE), ("/m.gx", SH_BODY)],
        "{ mod m; saved <- |x: i64| m::Sh::sh(m::make(x)); 1 }",
    )
    .await
}

/// The same for a function of a dropped dynamic module, whose view
/// carries its load.
async fn a_function_escaping_a_dropped_dynamic_module_dispatches_to_its_impl(
    mode: Mode,
) -> Result<()> {
    escaped_and_called(
        mode,
        [],
        r#"{ let status = mod m dynamic {
               sig { use core; trait Sh { val sh: fn(self) -> i64 }; type C; impl Sh for C; val get: fn(x: i64) -> i64 };
               source "type C = Abstract<i64>; impl Sh for C { let sh = |c: C| c.0 * 10 }; let get = |x: i64| Sh::sh(C(x))"
           };
           saved <- m::get;
           select status { error as e => never(dbg(e)), null as _ => 1 } }"#,
    )
    .await
}

/// The same for a function made outside the dynamic module.
async fn a_function_over_a_dropped_dynamic_modules_trait_dispatches_to_its_impl(
    mode: Mode,
) -> Result<()> {
    escaped_and_called(
        mode,
        [],
        r#"{ let status = mod m dynamic {
               sig { use core; trait Sh { val sh: fn(self) -> i64 }; type C; impl Sh for C; val make: fn(x: i64) -> C };
               source "type C = Abstract<i64>; impl Sh for C { let sh = |c: C| c.0 * 10 }; let make = |x: i64| C(x)"
           };
           saved <- |x: i64| m::Sh::sh(m::make(x));
           select status { error as e => never(dbg(e)), null as _ => 1 } }"#,
    )
    .await
}

/// The same for a function made in a dynamic module's load where the
/// interface of a module that load declares declares the trait and the
/// impl.
async fn a_function_escaping_a_dropped_load_around_an_interface_module_dispatches(
    mode: Mode,
) -> Result<()> {
    escaped_and_called(
        mode,
        [],
        r#"{ let status = mod d dynamic {
               sig { val make: fn(x: i64) -> i64 };
               source "let r = mod inner dynamic { sig { trait Sh { val sh: fn(self) -> i64 }; type C; impl Sh for C; val mk: fn(x: i64) -> C }; source \"type C = Abstract<i64>; impl Sh for C { let sh = |c: C| c.0 * 10 }; let mk = |x: i64| C(x)\" }; let make = |x: i64| inner::Sh::sh(inner::mk(x))"
           };
           saved <- d::make;
           select status { error as e => never(dbg(e)), null as _ => 1 } }"#,
    )
    .await
}

/// A function that writes a dropped trait only as a parameter's type or
/// a variable's bound, calling no method, binds the call it makes once
/// the trait is gone: the bound's declaration stands with it.
async fn a_function_over_a_dropped_trait_as_a_parameters_type_binds_its_calls(
    mode: Mode,
) -> Result<()> {
    escaped_and_called(
        mode,
        [],
        "{ type C = Abstract<i64>; trait Sh { val sh: fn(self) -> i64 }; \
         impl Sh for C { let sh = |c: C| c.0 * 10 }; \
         saved <- |x: i64| { let f = |v: Sh| 30; f(C(x)) }; 1 }",
    )
    .await
}

async fn a_function_over_a_dropped_trait_as_a_variables_bound_binds_its_calls(
    mode: Mode,
) -> Result<()> {
    escaped_and_called(
        mode,
        [],
        "{ type C = Abstract<i64>; trait Sh { val sh: fn(self) -> i64 }; \
         impl Sh for C { let sh = |c: C| c.0 * 10 }; \
         saved <- |x: i64| { let f = 'a: Sh |v: 'a| 30; f(C(x)) }; 1 }",
    )
    .await
}

async fn a_function_over_a_dropped_trait_with_no_methods_binds_its_calls(
    mode: Mode,
) -> Result<()> {
    escaped_and_called(
        mode,
        [],
        "{ type C = Abstract<i64>; trait Mk {}; impl Mk for C {}; \
         saved <- |x: i64| { let f = |v: Mk| 30; f(C(x)) }; 1 }",
    )
    .await
}

/// A module at the root whose interface declares `Sh`, a type and its
/// impl, `sh` its value times `k`; `name` is `(x)`'s `sh` through the
/// module, made outside it.
fn root_module_and_function(name: &str, k: i64) -> String {
    format!(
        r#"mod m dynamic {{
               sig {{ use core; trait Sh {{ val sh: fn(self) -> i64 }}; type C; impl Sh for C; val make: fn(x: i64) -> C }};
               source "type C = Abstract<i64>; impl Sh for C {{ let sh = |c: C| c.0 * {k} }}; let make = |x: i64| C(x)"
           }};
           {name} <- |x: i64| m::Sh::sh(m::make(x));
           1"#
    )
}

/// The root functions `names` replaced by the identity, once that has
/// landed and the functions they held are collected.
async fn released(ctx: &TestCtx, rx: &mut Events, names: &[&str]) -> Result<()> {
    let writes: String = names.iter().map(|n| format!("{n} <- |x: i64| x; ")).collect();
    let calls: String = names.iter().map(|n| format!("{n}(3) + ")).collect();
    let written = ctx.rt.compile(ArcStr::from(format!("{{ {writes}1 }}"))).await?;
    let want = Value::I64(3 * names.len() as i64);
    for _ in 0..3 {
        let (id, call) = compile_one(ctx, format!("{calls}0")).await?;
        await_update(ctx, rx, id, |v| *v == want).await?;
        drop(call)
    }
    drop(written);
    Ok(())
}

/// A function that outlives its trait's declaration keeps dispatching
/// through it once the trait and the impl are declared again at the same
/// path: each function calls the impl it was made under, whichever of the
/// two declarations stands.
async fn a_function_escaping_a_dropped_trait_dispatches_past_its_redeclaration(
    mode: Mode,
) -> Result<()> {
    let (ctx, mut rx) = fixture_runtime([], crate::TEST_REGISTER, mode, |_| {}).await?;
    let _roots = ctx
        .rt
        .compile(literal!(
            "let old: fn(x: i64) -> i64 = never(); let new: fn(x: i64) -> i64 = never();"
        ))
        .await?;
    let is = |n: i64| move |v: &Value| *v == Value::I64(n);
    let before = held(&ctx).await?;
    let mut made = Vec::new();
    for (name, k) in [("old", 10), ("new", 20)] {
        // the first declaration is gone before the second is made
        made.clear();
        let res = ctx.rt.compile(ArcStr::from(root_module_and_function(name, k))).await?;
        await_update(&ctx, &mut rx, res.exprs[res.exprs.len() - 1].id, is(1)).await?;
        made.push(res);
        let (id, call) = compile_one(&ctx, format!("{name}(3)")).await?;
        await_update(&ctx, &mut rx, id, is(3 * k)).await?;
        drop(call)
    }
    let (id, call) = compile_one(&ctx, "(old(3), new(3))").await?;
    await_update(&ctx, &mut rx, id, is_pair(30, 60)).await?;
    drop((call, made));
    let (id, call) = compile_one(&ctx, "(old(4), new(4))").await?;
    await_update(&ctx, &mut rx, id, is_pair(40, 80)).await?;
    drop(call);
    released(&ctx, &mut rx, &["old", "new"]).await?;
    let after = held(&ctx).await?;
    ctx.shutdown().await;
    assert_eq!(
        before[5..9],
        after[5..9],
        "{mode:?}: dispatchers, records, impls or kept"
    );
    Ok(())
}

/// A function made in a later input than the trait and the impl it calls
/// dispatches through them once their input is dropped.
async fn a_function_outliving_another_inputs_trait_dispatches_to_its_impl(
    mode: Mode,
) -> Result<()> {
    let (ctx, mut rx) = fixture_runtime([], crate::TEST_REGISTER, mode, |_| {}).await?;
    let _saved =
        ctx.rt.compile(literal!("let saved: fn(x: i64) -> i64 = never();")).await?;
    let before = held(&ctx).await?;
    let declared = ctx
        .rt
        .compile(literal!(
            "type C = Abstract<i64>; \
             trait Sh { val sh: fn(self) -> i64; val twice: fn(self) -> i64 = |s| sh(s) * 2 }; \
             impl Sh for C { let sh = |c: C| c.0 * 5 }"
        ))
        .await?;
    let is = |n: i64| move |v: &Value| *v == Value::I64(n);
    let (made, kept) =
        compile_one(&ctx, "{ saved <- |x: i64| Sh::twice(C(x)); 1 }").await?;
    await_update(&ctx, &mut rx, made, is(1)).await?;
    let (live, call) = compile_one(&ctx, "saved(3)").await?;
    await_update(&ctx, &mut rx, live, is(30)).await?;
    drop((declared, call));
    let (late, call) = compile_one(&ctx, "saved(3)").await?;
    await_update(&ctx, &mut rx, late, is(30)).await?;
    drop((kept, call));
    let (later, call) = compile_one(&ctx, "saved(4)").await?;
    await_update(&ctx, &mut rx, later, is(40)).await?;
    drop(call);
    released(&ctx, &mut rx, &["saved"]).await?;
    let after = held(&ctx).await?;
    ctx.shutdown().await;
    assert_eq!(
        before[5..9],
        after[5..9],
        "{mode:?}: dispatchers, records, impls or kept"
    );
    Ok(())
}

/// `ROUNDS` times over: compile `trait_input`, which declares `Sh` at the
/// root, then `impl_input`, then `{ saved <- <function>; 1 }`; drop the
/// trait's input, then the impl's, then the function's, and call
/// `saved(3)` at a new site after each. Every call answers `want`, and the
/// session holds after the last round what it held after the fifth.
async fn trait_dropped_before_its_impl(
    mode: Mode,
    trait_input: &'static str,
    impl_input: &'static str,
    function: &'static str,
    want: i64,
) -> Result<()> {
    inputs_dropped_in_order(mode, trait_input, impl_input, function, want, [0, 1, 2])
        .await
}

/// [`trait_dropped_before_its_impl`] dropping the inputs in `order`: 0 is
/// the trait's, 1 the impl's, 2 the function's.
async fn inputs_dropped_in_order(
    mode: Mode,
    trait_input: &'static str,
    impl_input: &'static str,
    function: &'static str,
    want: i64,
    order: [usize; 3],
) -> Result<()> {
    const ROUNDS: usize = 30;
    let (ctx, mut rx) = fixture_runtime([], crate::TEST_REGISTER, mode, |_| {}).await?;
    let _saved =
        ctx.rt.compile(literal!("let saved: fn(x: i64) -> i64 = never();")).await?;
    let is = |n: i64| move |v: &Value| *v == Value::I64(n);
    let mut early = None;
    for i in 0..5 + ROUNDS {
        if i == 5 {
            early = Some(held(&ctx).await?)
        }
        let declared = ctx.rt.compile(ArcStr::from(trait_input)).await?;
        let (made, read) = compile_one(&ctx, "{ let f = app; 7 }").await?;
        await_update(&ctx, &mut rx, made, is(7)).await?;
        drop(read);
        let implemented = ctx.rt.compile(ArcStr::from(impl_input)).await?;
        let (made, function) =
            compile_one(&ctx, format!("{{ saved <- {function}; 1 }}")).await?;
        await_update(&ctx, &mut rx, made, is(1)).await?;
        let (landed, read) = compile_one(&ctx, "{ let f = saved; 7 }").await?;
        await_update(&ctx, &mut rx, landed, is(7)).await?;
        drop(read);
        let mut inputs = [Some(declared), Some(implemented), Some(function)];
        for k in order {
            drop(inputs[k].take());
            let (late, call) = compile_one(&ctx, "saved(3)").await?;
            await_update(&ctx, &mut rx, late, is(want)).await?;
            drop(call);
        }
    }
    let late = held(&ctx).await?;
    ctx.shutdown().await;
    assert_eq!(early, Some(late), "{mode:?}: grew over {ROUNDS} rounds");
    Ok(())
}

/// A generic function made before an impl of its trait is registered
/// reaches that impl through a later function's call, once the trait's
/// input and then the impl's are dropped.
async fn a_function_made_before_an_impl_dispatches_to_it_once_both_are_dropped(
    mode: Mode,
) -> Result<()> {
    trait_dropped_before_its_impl(
        mode,
        "trait Sh { val sh: fn(self) -> i64; val twice: fn(self) -> i64 = |s| sh(s) * 2 }; \
         let app = |v: Sh| Sh::twice(v) + 1",
        "type C = Abstract<i64>; impl Sh for C { let sh = |c: C| c.0 * 5 }",
        "|x: i64| app(C(x))",
        31,
    )
    .await
}

/// An impl given back while its trait's declaration stands is kept for a
/// function that dispatches through the trait: the block that declares
/// the impl and the function is dropped first, then the trait; and an
/// impl and a function of separate inputs, dropped in each order.
async fn a_function_outliving_its_impl_beside_a_standing_trait_dispatches_to_it(
    mode: Mode,
) -> Result<()> {
    let trait_input = "trait Sh { val sh: fn(self) -> i64; val twice: fn(self) -> i64 = |s| sh(s) * 2 }; \
         let app = |v: Sh| Sh::twice(v) + 1";
    let impl_input = "type C = Abstract<i64>; impl Sh for C { let sh = |c: C| c.0 * 5 }";
    let functions =
        [("|x: i64| Sh::sh(C(x)) + 1", 16), ("|x: i64| Sh::twice(C(x)) + app(C(x))", 61)];
    for (function, want) in functions {
        for order in [[1, 0, 2], [1, 2, 0], [0, 1, 2], [2, 1, 0]] {
            inputs_dropped_in_order(mode, trait_input, impl_input, function, want, order)
                .await?
        }
    }
    Ok(())
}

/// A function that left a dropped block dispatches to the block's impl
/// of a trait declared outside it, which stays declared.
async fn a_function_escaping_a_dropped_block_dispatches_to_its_impl_of_a_standing_trait(
    mode: Mode,
) -> Result<()> {
    escaped_and_called_over(
        mode,
        [],
        "trait Sh { val sh: fn(self) -> i64 }",
        "{ type C = Abstract<i64>; impl Sh for C { let sh = |c: C| c.0 * 10 }; \
         saved <- |x: i64| Sh::sh(C(x)); 1 }",
    )
    .await
}

/// The same for an impl of a plain head, called over a union.
async fn a_function_escaping_a_dropped_block_dispatches_to_its_impl_of_a_plain_head(
    mode: Mode,
) -> Result<()> {
    escaped_and_called_over(
        mode,
        [],
        "trait Sh { val sh: fn(self) -> i64 }; impl Sh for i64 { let sh = |c: i64| c * 20 }",
        "{ impl Sh for string { let sh = |c: string| 10 }; \
         saved <- |x: i64| { let u: [i64, string] = \"s\"; Sh::sh(u) + Sh::sh(x) - 40 }; 1 }",
    )
    .await
}

/// Impls of a plain head declared again while the trait stands: each
/// function calls the impl it was made under, through a union too.
async fn a_function_outliving_its_impl_of_a_plain_head_past_a_redeclaration_beside_a_standing_trait(
    mode: Mode,
) -> Result<()> {
    let (ctx, mut rx) = fixture_runtime([], crate::TEST_REGISTER, mode, |_| {}).await?;
    let _roots = ctx
        .rt
        .compile(literal!(
            "trait Sh { val sh: fn(self) -> i64 }; \
             let old: fn(x: i64) -> i64 = never(); let new: fn(x: i64) -> i64 = never();"
        ))
        .await?;
    let is = |n: i64| move |v: &Value| *v == Value::I64(n);
    let declare = |name: &str, k: i64| {
        format!(
            "impl Sh for i64 {{ let sh = |c: i64| c * {k} }}; \
             impl Sh for string {{ let sh = |c: string| {k} }}; \
             {name} <- |x: i64| {{ let u: [i64, string] = x; Sh::sh(u) + Sh::sh(x) }}; 1"
        )
    };
    for (name, k, want) in [("old", 2, 12), ("new", 3, 18)] {
        let res = ctx.rt.compile(ArcStr::from(declare(name, k))).await?;
        await_update(&ctx, &mut rx, res.exprs[res.exprs.len() - 1].id, is(1)).await?;
        drop(res);
        let (id, call) = compile_one(&ctx, format!("{name}(3)")).await?;
        await_update(&ctx, &mut rx, id, is(want)).await?;
        drop(call)
    }
    let (id, call) = compile_one(&ctx, "(old(3), new(3))").await?;
    await_update(&ctx, &mut rx, id, is_pair(12, 18)).await?;
    drop(call);
    ctx.shutdown().await;
    Ok(())
}

/// A generic function and a closure made beside the impl of a dropped block
/// dispatch to it while the trait stands.
async fn a_function_over_a_generic_one_beside_a_dropped_impl_dispatches_to_it(
    mode: Mode,
) -> Result<()> {
    escaped_and_called_over(
        mode,
        [],
        "trait Sh { val sh: fn(self) -> i64 }",
        "{ type C = Abstract<i64>; impl Sh for C { let sh = |c: C| c.0 * 10 }; \
         let app = |v: Sh| Sh::sh(v); \
         saved <- |x: i64| { let g = |y: i64| app(C(y)); g(x) }; 1 }",
    )
    .await
}

/// A union of two impls of a dropped block dispatches to both while the trait stands.
async fn a_union_over_two_dropped_impls_of_a_standing_trait_dispatches_to_both(
    mode: Mode,
) -> Result<()> {
    escaped_and_called_over(
        mode,
        [],
        "trait Sh { val sh: fn(self) -> i64 }",
        "{ type C = Abstract<i64>; impl Sh for C { let sh = |c: C| c.0 * 10 }; \
         type D = Abstract<i64>; impl Sh for D { let sh = |c: D| 1 }; let u: [C, D] = C(3); \
         saved <- |x: i64| Sh::sh(u) + Sh::sh(C(x)) - 30; 1 }",
    )
    .await
}

/// A function made beside the dropped impl of a standing trait reaches it
/// through the trait's default method and through a function of the
/// trait's input that names the trait only as a parameter's type, called
/// and as values: each is instantiated in a view made before the impl.
async fn a_function_reaching_a_dropped_impl_through_an_earlier_definition_dispatches_to_it(
    mode: Mode,
) -> Result<()> {
    escaped_and_called_over(
        mode,
        [],
        "trait Sh { val sh: fn(self) -> i64; val twice: fn(self) -> i64 = |s| sh(s) * 2 }; \
         let app = |v: Sh| Sh::sh(v)",
        "{ type C = Abstract<i64>; impl Sh for C { let sh = |c: C| c.0 * 2 }; \
         saved <- |x: i64| { let t = Sh::twice; let g = |c: C| app(c); \
                             Sh::twice(C(x)) + app(C(x)) + t(C(x)) + g(C(x)) - 6 }; 1 }",
    )
    .await
}

/// A function made beside the dropped impl of a standing trait that names
/// the trait only through a function of an earlier input, called and
/// passed on as a value, dispatches to the impl.
async fn a_function_reaching_the_trait_only_through_a_captured_function_dispatches_to_its_dropped_impl(
    mode: Mode,
) -> Result<()> {
    escaped_and_called_over(
        mode,
        [],
        "trait Sh { val sh: fn(self) -> i64 }; let app = |v: Sh| Sh::sh(v)",
        "{ type C = Abstract<i64>; impl Sh for C { let sh = |c: C| c.0 * 10 }; \
         saved <- |x: i64| { let ap = |f: fn(v: C) -> i64, c: C| f(c); app(C(x)) + ap(app, C(x)) - 30 }; 1 }",
    )
    .await
}

/// A run-time bind in a function made beside the dropped impl of a standing
/// trait (the callee is chosen by a select, so it binds when the call runs)
/// builds its instance under the view of the code the call is in: the impl
/// is reached through the generic function of an earlier input and by name.
async fn a_run_time_bind_beside_a_dropped_impl_of_a_standing_trait_dispatches_to_it(
    mode: Mode,
) -> Result<()> {
    escaped_and_called_over(
        mode,
        [],
        "trait Sh { val sh: fn(self) -> i64 }; let app = |v: Sh| Sh::sh(v)",
        "{ type C = Abstract<i64>; impl Sh for C { let sh = |c: C| c.0 * 10 }; \
         saved <- |x: i64| { let f = select x { 3 => app, _ => |c: C| 0 }; \
                             f(C(x)) + Sh::sh(C(x)) - 30 }; 1 }",
    )
    .await
}

/// A method of a dropped impl of a standing trait, instantiated for a call
/// beside it, names a later method of its trait for another type: the
/// name is the trait's, as it was where the method was defined, not the
/// later sibling the caller's view holds.
async fn a_method_of_a_dropped_impl_naming_a_later_method_dispatches_by_the_trait(
    mode: Mode,
) -> Result<()> {
    escaped_and_called_over(
        mode,
        [],
        "trait Sh { val sh: fn(self) -> i64; val k: fn(self) -> i64 }; \
         type D = Abstract<i64>; \
         impl Sh for D { let sh = |d: D| 0; let k = |d: D| d.0 * 9 + 2 }",
        "{ type C = Abstract<i64>; \
         impl Sh for C { let sh = |c: C| k(D(c.0)) + 1; let k = |c: C| c.0 * 2 }; \
         saved <- |y: i64| Sh::sh(C(y)); 1 }",
    )
    .await
}

/// A function value passed through a higher-order function reaches the dropped
/// impl of a standing trait.
async fn a_function_passed_to_another_beside_a_dropped_impl_dispatches_to_it(
    mode: Mode,
) -> Result<()> {
    escaped_and_called_over(
        mode,
        [],
        "trait Sh { val sh: fn(self) -> i64 }",
        "{ type C = Abstract<i64>; impl Sh for C { let sh = |c: C| c.0 * 10 }; \
         let mk = |x: i64| C(x); let ap = |f: fn(x: i64) -> C| Sh::sh(f(3)); \
         saved <- |x: i64| ap(mk); 1 }",
    )
    .await
}

/// The same for an impl a module's interface declares of a trait
/// declared outside the module.
async fn a_function_over_an_interfaces_impl_of_a_dropped_outside_trait_dispatches(
    mode: Mode,
) -> Result<()> {
    trait_dropped_before_its_impl(
        mode,
        "trait Sh { val sh: fn(self) -> i64 }; let app = |v: Sh| Sh::sh(v)",
        r#"mod m dynamic {
               sig { use super::Sh; type C; impl Sh for C; val make: fn(x: i64) -> C };
               source "use super::Sh; type C = Abstract<i64>; impl Sh for C { let sh = |c: C| c.0 * 10 }; let make = |x: i64| C(x)"
           }"#,
        "|x: i64| Sh::sh(m::make(x))",
        30,
    )
    .await
}

/// Impls of heads that are no abstract type, declared again with their
/// trait at the same path: each function calls the impls and the default
/// it was made under, through a union too, whichever declaration stands.
async fn a_function_escaping_dropped_impls_of_plain_heads_dispatches_past_their_redeclaration(
    mode: Mode,
) -> Result<()> {
    let (ctx, mut rx) = fixture_runtime([], crate::TEST_REGISTER, mode, |_| {}).await?;
    let _roots = ctx
        .rt
        .compile(literal!(
            "let old: fn(x: i64) -> i64 = never(); let new: fn(x: i64) -> i64 = never();"
        ))
        .await?;
    let is = |n: i64| move |v: &Value| *v == Value::I64(n);
    let before = held(&ctx).await?;
    let declare = |name: &str, k: i64| {
        format!(
            "trait Sh {{ val sh: fn(self) -> i64; val twice: fn(self) -> i64 = |s| sh(s) * {k} }}; \
             impl Sh for i64 {{ let sh = |c: i64| c * {k} }}; \
             impl Sh for string {{ let sh = |c: string| {k} }}; \
             {name} <- |x: i64| {{ let u: [i64, string] = x; Sh::twice(u) + Sh::sh(x) }}; 1"
        )
    };
    let mut made = Vec::new();
    for (name, k, want) in [("old", 2, 18), ("new", 3, 36)] {
        // the first declaration is gone before the second is made
        made.clear();
        let res = ctx.rt.compile(ArcStr::from(declare(name, k))).await?;
        await_update(&ctx, &mut rx, res.exprs[res.exprs.len() - 1].id, is(1)).await?;
        made.push(res);
        let (id, call) = compile_one(&ctx, format!("{name}(3)")).await?;
        await_update(&ctx, &mut rx, id, is(want)).await?;
        drop(call)
    }
    let (id, call) = compile_one(&ctx, "(old(3), new(3))").await?;
    await_update(&ctx, &mut rx, id, is_pair(18, 36)).await?;
    drop((call, made));
    let (id, call) = compile_one(&ctx, "(old(4), new(4))").await?;
    await_update(&ctx, &mut rx, id, is_pair(24, 48)).await?;
    drop(call);
    released(&ctx, &mut rx, &["old", "new"]).await?;
    let after = held(&ctx).await?;
    ctx.shutdown().await;
    assert_eq!(
        before[5..9],
        after[5..9],
        "{mode:?}: dispatchers, records, impls or kept"
    );
    Ok(())
}

async fn a_function_over_a_dropped_trait_bound_in_a_nested_function_binds_its_calls(
    mode: Mode,
) -> Result<()> {
    escaped_and_called(
        mode,
        [],
        "{ type C = Abstract<i64>; trait Sh { val sh: fn(self) -> i64 }; \
         impl Sh for C { let sh = |c: C| c.0 * 10 }; \
         saved <- |x: i64| { let g = |v: Sh| 30; let f = |w: i64| g(C(w)); f(x) }; 1 }",
    )
    .await
}

async fn a_function_over_a_dropped_trait_bound_in_a_function_it_names_binds_its_calls(
    mode: Mode,
) -> Result<()> {
    escaped_and_called(
        mode,
        [],
        "{ type C = Abstract<i64>; trait Sh { val sh: fn(self) -> i64 }; \
         impl Sh for C { let sh = |c: C| c.0 * 10 }; \
         let g = |v: Sh| 30; saved <- |x: i64| g(C(x)); 1 }",
    )
    .await
}

async fn a_function_over_a_dropped_trait_bound_inferred_through_a_callee_binds_its_calls(
    mode: Mode,
) -> Result<()> {
    escaped_and_called(
        mode,
        [],
        "{ type C = Abstract<i64>; trait Sh { val sh: fn(self) -> i64 }; \
         impl Sh for C { let sh = |c: C| c.0 * 10 }; \
         let g = |v: Sh| 30; saved <- |x: i64| { let h = |w| g(w); h(C(x)) }; 1 }",
    )
    .await
}

/// A trait bounding a typedef's parameter reaches a signature through
/// the argument the typedef is applied to, never through its body.
async fn a_function_over_a_dropped_trait_bounding_a_typedefs_parameter_binds_its_calls(
    mode: Mode,
) -> Result<()> {
    escaped_and_called(
        mode,
        [],
        "{ type C = Abstract<i64>; trait Sh { val sh: fn(self) -> i64 }; \
         impl Sh for C { let sh = |c: C| c.0 * 10 }; type W<'a: Sh> = ('a, i64); \
         saved <- |x: i64| { let f = |w: W<'b>| w.1; f((C(x), 30)) }; 1 }",
    )
    .await
}

modes!(
    a_function_saved_across_reloads_keeps_nothing_once_dropped,
    a_dispatching_function_saved_across_reloads_keeps_nothing_once_dropped,
    a_function_saved_across_reloads_keeps_its_loads_type_no_longer,
    a_retired_loads_type_goes_with_its_last_function,
    a_reloaded_module_keeps_its_retired_loads_type_no_longer,
    a_deleted_modules_type_goes_with_its_last_function,
    a_saved_function_calling_an_outside_function_keeps_nothing_once_dropped,
    a_saved_function_calling_another_modules_function_keeps_nothing_once_dropped,
    a_function_saved_past_a_declared_impl_keeps_nothing_once_dropped,
    a_function_saved_across_reloads_around_an_interface_module_keeps_nothing_once_dropped,
    a_function_escaping_deleted_interface_modules_keeps_nothing_once_dropped,
    both_sides_of_deleted_interface_modules_keep_nothing_once_dropped,
    a_held_function_outlives_a_collected_reader_of_its_val,
    a_function_escaping_a_dropped_block_dispatches_to_its_impl,
    a_function_escaping_a_dropped_block_dispatches_to_its_defaults,
    a_function_escaping_a_dropped_interface_module_dispatches_to_its_impl,
    a_function_over_a_dropped_interface_modules_trait_dispatches_to_its_impl,
    a_function_escaping_a_dropped_dynamic_module_dispatches_to_its_impl,
    a_function_over_a_dropped_dynamic_modules_trait_dispatches_to_its_impl,
    a_function_escaping_a_dropped_load_around_an_interface_module_dispatches,
    a_function_escaping_a_dropped_trait_dispatches_past_its_redeclaration,
    a_function_outliving_another_inputs_trait_dispatches_to_its_impl,
    a_function_made_before_an_impl_dispatches_to_it_once_both_are_dropped,
    a_function_outliving_its_impl_beside_a_standing_trait_dispatches_to_it,
    a_function_escaping_a_dropped_block_dispatches_to_its_impl_of_a_standing_trait,
    a_function_escaping_a_dropped_block_dispatches_to_its_impl_of_a_plain_head,
    a_function_over_a_generic_one_beside_a_dropped_impl_dispatches_to_it,
    a_union_over_two_dropped_impls_of_a_standing_trait_dispatches_to_both,
    a_function_passed_to_another_beside_a_dropped_impl_dispatches_to_it,
    a_function_reaching_a_dropped_impl_through_an_earlier_definition_dispatches_to_it,
    a_function_reaching_the_trait_only_through_a_captured_function_dispatches_to_its_dropped_impl,
    a_run_time_bind_beside_a_dropped_impl_of_a_standing_trait_dispatches_to_it,
    a_method_of_a_dropped_impl_naming_a_later_method_dispatches_by_the_trait,
    a_function_outliving_its_impl_of_a_plain_head_past_a_redeclaration_beside_a_standing_trait,
    a_function_over_an_interfaces_impl_of_a_dropped_outside_trait_dispatches,
    a_function_escaping_dropped_impls_of_plain_heads_dispatches_past_their_redeclaration,
    a_function_over_a_dropped_trait_as_a_parameters_type_binds_its_calls,
    a_function_over_a_dropped_trait_as_a_variables_bound_binds_its_calls,
    a_function_over_a_dropped_trait_with_no_methods_binds_its_calls,
    a_function_over_a_dropped_trait_bound_in_a_nested_function_binds_its_calls,
    a_function_over_a_dropped_trait_bound_in_a_function_it_names_binds_its_calls,
    a_function_over_a_dropped_trait_bound_inferred_through_a_callee_binds_its_calls,
    a_function_over_a_dropped_trait_bounding_a_typedefs_parameter_binds_its_calls,
);
