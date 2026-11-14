// State the runtime makes for a call, an instance or a core-trait hook
// goes when the thing it was made for does: a long session (REPL,
// dynamic modules, a regrowing map) holds what is live, not its history.

use crate::init;
use anyhow::{Context, Result, bail};
use arcstr::{ArcStr, literal};
use graphix_compiler::{
    BindId,
    expr::{ExprId, VfsEntry},
};
use graphix_package_core::testing::{
    Events, Mode, TestCtx, fixture_runtime, next_update,
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
                    sandbox whitelist [core];
                    sig { val h: i64 };
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
async fn held(ctx: &TestCtx) -> Result<[usize; 15]> {
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
/// load's. `get(x)` is `x + v`.
async fn saved_across_reloads(mode: Mode, sig: &str, body: &str) -> Result<()> {
    const RELOADS: i64 = 1000;
    let (ctx, mut rx) = fixture_runtime([], crate::TEST_REGISTER, mode, |_| {}).await?;
    let _tick = ctx.rt.compile(literal!("let tick: i64 = 0;")).await?;
    let program = format!(
        r#"{{
          let src = "let v = [tick]; {body}";
          let s = mod d dynamic {{
              sandbox whitelist [core];
              sig {{ {sig} val v: i64; val get: fn(x: i64) -> i64 }};
              source src
          }};
          let saved: fn(x: i64) -> i64 = never();
          saved <- d::get;
          (d::v, saved(d::v))
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
    saved_across_reloads(mode, "", "let get = |x: i64| x + v").await
}

/// A retired body's dispatchers, defaults and impl methods go with the
/// last function made in it.
async fn a_dispatching_function_saved_across_reloads_keeps_nothing_once_dropped(
    mode: Mode,
) -> Result<()> {
    saved_across_reloads(
        mode,
        "",
        "trait T { val d: fn(self) -> i64; val e: fn(self) -> i64 = |s| d(s) }; \
         type C = Abstract<i64>; impl T for C { let d = |c: C| c.0 + v }; \
         let get = |x: i64| T::e(C(x))",
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
        "let r = mod inner dynamic { sandbox unrestricted; \
         sig { trait T { val d: fn(self) -> i64; val e: fn(self) -> i64 = |s| d(s) }; \
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
                sandbox whitelist [core];
                sig { type C; impl Cast for C; val make: fn(x: i64) -> C };
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
                sandbox whitelist [core];
                sig { trait Sh { val sh: fn(self) -> i64 }; type C; impl Sh for C; val make: fn(x: i64) -> C };
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
        await_update(&mut rx, written, is(2 + k)).await?;
        drop((made, call));
        // the block is gone, and this call is the first to build f's body
        // since
        let (late, call) = compile_one(&ctx, r#"g("abc")"#).await?;
        await_update(&mut rx, late, is(3 + k)).await?;
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
        await_update(&mut rx, id, |v| *v == Value::I64(2)).await?;
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
    await_update(&mut rx, id, |v| *v == want).await?;
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
    await_update(&mut rx, mapped, |v| *v == want).await?;
    for _ in 0..3 {
        let (id, res) = compile_one(&ctx, "{ use str::len; len(\"ab\") }").await?;
        await_update(&mut rx, id, |v| *v == Value::I64(2)).await?;
        drop(res)
    }
    let _grown = ctx.rt.compile(literal!("a <- [1, 2, 3, 4]")).await?;
    let want = array(&[3, 4, 5, 6]);
    await_update(&mut rx, mapped, |v| *v == want).await?;
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
            sandbox whitelist [core];
            sig { trait Sh { val sh: fn(self) -> i64 }; type C; impl Sh for C; val make: fn(x: i64) -> C };
            source "type C = Abstract<i64>; impl Sh for C { let sh = |c: C| c.0 * 10 }; let make = |x: i64| C(x)"
        }"#,
        Value::Null,
    )
    .await
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
    const ROUNDS: usize = 30;
    let files = files.map(|(path, text)| (path, VfsEntry::from(ArcStr::from(text))));
    let (ctx, mut rx) =
        fixture_runtime(files, crate::TEST_REGISTER, mode, |_| {}).await?;
    let _saved =
        ctx.rt.compile(literal!("let saved: fn(x: i64) -> i64 = never();")).await?;
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
        await_update(&mut rx, eid, |v| *v == Value::I64(1)).await?;
        let (landed, read) = compile_one(&ctx, "{ let f = saved; 7 }").await?;
        await_update(&mut rx, landed, |v| *v == Value::I64(7)).await?;
        drop((made, read));
        let (late, call) = compile_one(&ctx, "saved(3)").await?;
        await_update(&mut rx, late, |v| *v == Value::I64(30)).await?;
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
               sandbox whitelist [core];
               sig { trait Sh { val sh: fn(self) -> i64 }; type C; impl Sh for C; val get: fn(x: i64) -> i64 };
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
               sandbox whitelist [core];
               sig { trait Sh { val sh: fn(self) -> i64 }; type C; impl Sh for C; val make: fn(x: i64) -> C };
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
               sandbox unrestricted;
               sig { val make: fn(x: i64) -> i64 };
               source "let r = mod inner dynamic { sandbox unrestricted; sig { trait Sh { val sh: fn(self) -> i64 }; type C; impl Sh for C; val mk: fn(x: i64) -> C }; source \"type C = Abstract<i64>; impl Sh for C { let sh = |c: C| c.0 * 10 }; let mk = |x: i64| C(x)\" }; let make = |x: i64| inner::Sh::sh(inner::mk(x))"
           };
           saved <- d::make;
           select status { error as e => never(dbg(e)), null as _ => 1 } }"#,
    )
    .await
}

/// A module at the root whose interface declares `Sh`, a type and its
/// impl, `sh` its value times `k`; `name` is `(x)`'s `sh` through the
/// module, made outside it.
fn root_module_and_function(name: &str, k: i64) -> String {
    format!(
        r#"mod m dynamic {{
               sandbox whitelist [core];
               sig {{ trait Sh {{ val sh: fn(self) -> i64 }}; type C; impl Sh for C; val make: fn(x: i64) -> C }};
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
        await_update(rx, id, |v| *v == want).await?;
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
        await_update(&mut rx, res.exprs[res.exprs.len() - 1].id, is(1)).await?;
        made.push(res);
        let (id, call) = compile_one(&ctx, format!("{name}(3)")).await?;
        await_update(&mut rx, id, is(3 * k)).await?;
        drop(call)
    }
    let (id, call) = compile_one(&ctx, "(old(3), new(3))").await?;
    await_update(&mut rx, id, is_pair(30, 60)).await?;
    drop((call, made));
    let (id, call) = compile_one(&ctx, "(old(4), new(4))").await?;
    await_update(&mut rx, id, is_pair(40, 80)).await?;
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
    await_update(&mut rx, made, is(1)).await?;
    let (live, call) = compile_one(&ctx, "saved(3)").await?;
    await_update(&mut rx, live, is(30)).await?;
    drop((declared, call));
    let (late, call) = compile_one(&ctx, "saved(3)").await?;
    await_update(&mut rx, late, is(30)).await?;
    drop((kept, call));
    let (later, call) = compile_one(&ctx, "saved(4)").await?;
    await_update(&mut rx, later, is(40)).await?;
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
        await_update(&mut rx, made, is(7)).await?;
        drop(read);
        let implemented = ctx.rt.compile(ArcStr::from(impl_input)).await?;
        let (made, function) =
            compile_one(&ctx, format!("{{ saved <- {function}; 1 }}")).await?;
        await_update(&mut rx, made, is(1)).await?;
        let (landed, read) = compile_one(&ctx, "{ let f = saved; 7 }").await?;
        await_update(&mut rx, landed, is(7)).await?;
        drop(read);
        let mut inputs = [Some(declared), Some(implemented), Some(function)];
        for input in inputs.iter_mut() {
            drop(input.take());
            let (late, call) = compile_one(&ctx, "saved(3)").await?;
            await_update(&mut rx, late, is(want)).await?;
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

/// The same for an impl a module's interface declares of a trait
/// declared outside the module.
async fn a_function_over_an_interfaces_impl_of_a_dropped_outside_trait_dispatches(
    mode: Mode,
) -> Result<()> {
    trait_dropped_before_its_impl(
        mode,
        "trait Sh { val sh: fn(self) -> i64 }; let app = |v: Sh| Sh::sh(v)",
        r#"mod m dynamic {
               sandbox unrestricted;
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
        await_update(&mut rx, res.exprs[res.exprs.len() - 1].id, is(1)).await?;
        made.push(res);
        let (id, call) = compile_one(&ctx, format!("{name}(3)")).await?;
        await_update(&mut rx, id, is(want)).await?;
        drop(call)
    }
    let (id, call) = compile_one(&ctx, "(old(3), new(3))").await?;
    await_update(&mut rx, id, is_pair(18, 36)).await?;
    drop((call, made));
    let (id, call) = compile_one(&ctx, "(old(4), new(4))").await?;
    await_update(&mut rx, id, is_pair(24, 48)).await?;
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

modes!(
    a_function_saved_across_reloads_keeps_nothing_once_dropped,
    a_dispatching_function_saved_across_reloads_keeps_nothing_once_dropped,
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
    a_function_over_an_interfaces_impl_of_a_dropped_outside_trait_dispatches,
    a_function_escaping_dropped_impls_of_plain_heads_dispatches_past_their_redeclaration,
);
