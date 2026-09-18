//! The embedder-callable path (`GXHandle::compile_callable`): a callee
//! instance born lazily at its first dispatch must resolve `*st <- v`
//! from the standing store, like `Deref`'s read side.

use ahash::AHashMap;
use anyhow::{Context, Result, bail};
use graphix_compiler::{env::Env, expr::VfsResolver};
use graphix_package_core::testing;
use graphix_rt::{GXEvent, NoExt};
use netidx::{path::Path, protocol::valarray::ValArray, publisher::Value};
use std::time::Duration;
use tokio::sync::mpsc;

const PROG: &str = r#"
type St = { value: string, cursor: i64 };
let ed: St = { value: "", cursor: 0 };
let poke = |st: &St, tag: string| -> null select tag {
  "" => null,
  t => {
    let s = t ~ *st;
    *st <- { value: "[s.value][t]", cursor: s.cursor + 1 };
    null
  }
};
let handle = |tag: string| -> null poke(&ed, tag);
let result = ed.value
"#;

pub(super) fn find_bind_id(env: &Env, name: &str) -> Result<graphix_compiler::BindId> {
    let parts: Vec<&str> = name.split("::").collect();
    let (module, var) = match parts.as_slice() {
        [module, var] => (*module, *var),
        _ => bail!("expected module::var, got {name}"),
    };
    let suffix = format!("/{module}");
    for (scope, vars) in &env.binds {
        if Path::as_ref(&scope.0).ends_with(&suffix) {
            if let Some(bid) = vars.get(var) {
                return Ok(*bid);
            }
        }
    }
    bail!("no binding {name} found in env")
}

#[tokio::test(flavor = "multi_thread")]
async fn callable_handler_writes_through_ref_param() -> Result<()> {
    let (tx, mut rx) = mpsc::channel(100);
    let tbl = AHashMap::from_iter([(
        Path::from("/test.gx"),
        graphix_compiler::expr::VfsEntry::from(arcstr::ArcStr::from(PROG)),
    )]);
    let resolver = VfsResolver::new(tbl);
    let ctx =
        testing::init_with_resolvers(tx, crate::TEST_REGISTER, vec![resolver]).await?;
    let gx: graphix_rt::GXHandle<NoExt> = ctx.rt.clone();
    let compiled = gx.compile(arcstr::literal!("{ mod test; test::result }")).await?;
    let expr_id = compiled.exprs.last().context("no exprs")?.id;
    let handle_bid = find_bind_id(&compiled.env, "test::handle")?;
    let r = gx.compile_ref(handle_bid).await?;
    let lambda = r.last.clone().context("handle has no value")?;
    let callable = gx.compile_callable(lambda).await?;
    // Cycles between the callable's init and its first dispatch: the
    // reference value delivered at init must survive to the instance.
    for _ in 0..3 {
        let _e = gx.compile(arcstr::literal!("i64:0")).await?;
    }
    callable.call(ValArray::from_iter_exact(["x".into()].into_iter())).await?;
    let deadline = tokio::time::sleep(Duration::from_secs(30));
    tokio::pin!(deadline);
    loop {
        tokio::select! {
            _ = &mut deadline => bail!(
                "the write through the ref param never landed"
            ),
            batch = rx.recv() => match batch {
                None => bail!("runtime died"),
                Some(mut batch) => {
                    for ev in batch.drain(..) {
                        if let GXEvent::Updated(id, v) = ev {
                            if id == expr_id && v == Value::from("x") {
                                return Ok(());
                            }
                        }
                    }
                }
            }
        }
    }
}

/// A handler whose interior select routes by a state variable: flipping
/// the state wakes an arm for the first time, and the handler's standing
/// params must deliver stale, never re-fire a consumed event.
const PHANTOM: &str = r#"
let active: [`A, `B, null] = null;
let submitted = 0;
let fire = |t: Any| -> null {
  submitted <- t ~ (submitted + 1);
  null
};
let set_active = |b: bool| -> null {
  active <- select b {
    true => `B,
    false => `A
  };
  null
};
let handle = |e: string| -> i64 select active {
  null as _ => 0,
  `A => e ~ 1,
  `B => {
    fire(e);
    2
  }
};
let result = submitted
"#;

#[tokio::test(flavor = "multi_thread")]
async fn arm_wake_delivers_standing_args_stale() -> Result<()> {
    let (tx, mut rx) = mpsc::channel(100);
    let tbl = AHashMap::from_iter([(
        Path::from("/test.gx"),
        graphix_compiler::expr::VfsEntry::from(arcstr::ArcStr::from(PHANTOM)),
    )]);
    let resolver = VfsResolver::new(tbl);
    let ctx =
        testing::init_with_resolvers(tx, crate::TEST_REGISTER, vec![resolver]).await?;
    let gx: graphix_rt::GXHandle<NoExt> = ctx.rt.clone();
    let compiled = gx.compile(arcstr::literal!("{ mod test; test::result }")).await?;
    let expr_id = compiled.exprs.last().context("no exprs")?.id;
    let get = |name: &str| find_bind_id(&compiled.env, name);
    let handle_l = {
        let r = gx.compile_ref(get("test::handle")?).await?;
        gx.compile_callable(r.last.clone().context("no handle")?).await?
    };
    let set_active = {
        let r = gx.compile_ref(get("test::set_active")?).await?;
        gx.compile_callable(r.last.clone().context("no set_active")?).await?
    };
    // route to `A, deliver one real event, then flip to `B with no new
    // event: the flip must not fire the `B arm's callee with the standing "x"
    set_active.call(ValArray::from_iter_exact([Value::Bool(false)].into_iter())).await?;
    handle_l.call(ValArray::from_iter_exact(["x".into()].into_iter())).await?;
    set_active.call(ValArray::from_iter_exact([Value::Bool(true)].into_iter())).await?;
    for _ in 0..3 {
        let _e = gx.compile(arcstr::literal!("i64:0")).await?;
    }
    // one REAL event while `B is selected — the only legitimate fire
    handle_l.call(ValArray::from_iter_exact(["y".into()].into_iter())).await?;
    // settle, then read the count: phantom + legit = 2, legit only = 1
    for _ in 0..3 {
        let _e = gx.compile(arcstr::literal!("i64:0")).await?;
    }
    let deadline = tokio::time::sleep(Duration::from_secs(30));
    tokio::pin!(deadline);
    let mut last = None;
    loop {
        tokio::select! {
            _ = &mut deadline => break,
            batch = rx.recv() => match batch {
                None => bail!("runtime died"),
                Some(mut batch) => {
                    for ev in batch.drain(..) {
                        if let GXEvent::Updated(id, v) = ev {
                            if id == expr_id {
                                last = Some(v.clone());
                                if v == Value::I64(2) {
                                    bail!(
                                        "phantom fire: the arm-wake delivered the \
                                         standing event as fired (submitted=2)"
                                    );
                                }
                            }
                        }
                    }
                    if last == Some(Value::I64(1)) {
                        // the legit fire landed and nothing further is
                        // pending behind it — give one more batch a
                        // chance to contradict, then accept
                        tokio::time::sleep(Duration::from_millis(500)).await;
                        while let Ok(mut b) = rx.try_recv() {
                            for ev in b.drain(..) {
                                if let GXEvent::Updated(id, v) = ev {
                                    if id == expr_id && v == Value::I64(2) {
                                        bail!("phantom fire arrived late (submitted=2)");
                                    }
                                }
                            }
                        }
                        return Ok(());
                    }
                }
            }
        }
    }
    bail!("the legitimate fire never landed (last={last:?})")
}

/// A callable's body flips its own routing state from a key it consumed:
/// the newly selected arm's callee must read the standing key stale,
/// never dispatch on it.
const FLIP: &str = r#"
type Key = { code: [`Enter, `Other], kind: [`Press, `Release] };
type Event = [`Key(Key), `Mouse];
let screen = 0;
let fired = 0;
let req: [i64, null] = null;
let land = {
  handle: |e: Event| -> [`Stop, `Continue] select e {
    `Key(k) => select k.code {
      kk@ `Enter => { req <- kk ~ 1; `Stop },
      `Other => `Continue
    },
    `Mouse => `Continue
  }
};
select req {
  null as _ => never(),
  _ => screen <- 1
};
let connect_keys = |k: Key| -> [`Stop, `Continue] select k.code {
  kk@ `Enter => { fired <- (kk ~ fired) + 1; `Stop },
  `Other => `Continue
};
let handle = |e: Event| -> [`Stop, `Continue] select e {
  ev@ `Key(k) => select k.kind {
    `Press => select screen {
      0 => land.handle(ev),
      _ => connect_keys(k)
    },
    `Release => `Continue
  },
  `Mouse => `Continue
};
let result = (screen, fired)
"#;

#[tokio::test(flavor = "multi_thread")]
async fn callable_body_flip_reads_standing_key_stale() -> Result<()> {
    let (tx, mut rx) = mpsc::channel(100);
    let tbl = AHashMap::from_iter([(
        Path::from("/test.gx"),
        graphix_compiler::expr::VfsEntry::from(arcstr::ArcStr::from(FLIP)),
    )]);
    let resolver = VfsResolver::new(tbl);
    let ctx =
        testing::init_with_resolvers(tx, crate::TEST_REGISTER, vec![resolver]).await?;
    let gx: graphix_rt::GXHandle<NoExt> = ctx.rt.clone();
    let compiled = gx.compile(arcstr::literal!("{ mod test; test::result }")).await?;
    let expr_id = compiled.exprs.last().context("no exprs")?.id;
    let handle_l = {
        let r = gx.compile_ref(find_bind_id(&compiled.env, "test::handle")?).await?;
        gx.compile_callable(r.last.clone().context("no handle")?).await?
    };
    handle_l
        .call(ValArray::from_iter_exact(
            [Value::Array(ValArray::from_iter_exact(
                [
                    Value::from("Key"),
                    Value::Array(ValArray::from_iter_exact(
                        [
                            Value::Array(ValArray::from_iter_exact(
                                [Value::from("code"), Value::from("Enter")].into_iter(),
                            )),
                            Value::Array(ValArray::from_iter_exact(
                                [Value::from("kind"), Value::from("Press")].into_iter(),
                            )),
                        ]
                        .into_iter(),
                    )),
                ]
                .into_iter(),
            ))]
            .into_iter(),
        ))
        .await?;
    // settle: the request flips the screen with no further key
    for _ in 0..4 {
        let _e = gx.compile(arcstr::literal!("i64:0")).await?;
    }
    let deadline = tokio::time::sleep(Duration::from_secs(20));
    tokio::pin!(deadline);
    let mut last = None;
    loop {
        tokio::select! {
            _ = &mut deadline => break,
            batch = rx.recv() => match batch {
                None => bail!("runtime died"),
                Some(mut batch) => {
                    for ev in batch.drain(..) {
                        if let GXEvent::Updated(id, v) = ev
                            && id == expr_id
                        {
                            last = Some(v);
                        }
                    }
                    if let Some(Value::Array(pair)) = &last
                        && pair[0] == Value::I64(1)
                    {
                        // the screen flipped; one more batch for a phantom
                        let _e = gx.compile(arcstr::literal!("i64:0")).await?;
                        if let Ok(Some(mut b)) = tokio::time::timeout(
                            Duration::from_millis(500),
                            rx.recv(),
                        )
                        .await
                        {
                            for ev in b.drain(..) {
                                if let GXEvent::Updated(id, v) = ev && id == expr_id {
                                    last = Some(v);
                                }
                            }
                        }
                        break;
                    }
                }
            }
        }
    }
    match last {
        Some(Value::Array(pair))
            if pair[0] == Value::I64(1) && pair[1] == Value::I64(0) =>
        {
            Ok(())
        }
        Some(Value::Array(pair)) if pair[0] == Value::I64(1) => bail!(
            "phantom fire: the screen flip dispatched the connect arm's callee on \
             the standing key (fired={})",
            pair[1]
        ),
        v => bail!("the screen never flipped: {v:?}"),
    }
}

/// The screen dispatcher shape of a TUI: the handler routes the key to
/// one of two callees by a screen variable that the callee itself
/// moves. The key that moved the screen must not be delivered again to
/// the callee the move woke. The woken callee reads its formal stale
/// (`opened <- e ~ ..` stays quiet), but a select arm that binds that
/// stale formal delivers the binding fresh, and `kk ~ ..` fires.
const DISPATCH: &str = r#"
let screen: [`Landing, `Panels] = `Landing;
let opened = 0;
let pan_handle = |e: string| -> i64 select e {
  kk if kk == "enter" => { opened <- kk ~ opened + 1; 1 },
  _ => 0
};
let land_handle = |e: string| -> i64 select e {
  kk if kk == "enter" => { screen <- kk ~ `Panels; 1 },
  _ => 0
};
let handle = |e: string| -> i64 select e {
  "" => 0,
  ev => select screen {
    `Landing => land_handle(ev),
    `Panels => pan_handle(ev)
  }
};
let result = opened
"#;

#[tokio::test(flavor = "multi_thread")]
async fn arm_wake_does_not_redeliver_the_key_to_the_woken_callee() -> Result<()> {
    let (tx, mut rx) = mpsc::channel(100);
    let tbl = AHashMap::from_iter([(
        Path::from("/test.gx"),
        graphix_compiler::expr::VfsEntry::from(arcstr::ArcStr::from(DISPATCH)),
    )]);
    let resolver = VfsResolver::new(tbl);
    let ctx =
        testing::init_with_resolvers(tx, crate::TEST_REGISTER, vec![resolver]).await?;
    let gx: graphix_rt::GXHandle<NoExt> = ctx.rt.clone();
    let compiled = gx.compile(arcstr::literal!("{ mod test; test::result }")).await?;
    let expr_id = compiled.exprs.last().context("no exprs")?.id;
    let handle_l = {
        let r = gx.compile_ref(find_bind_id(&compiled.env, "test::handle")?).await?;
        gx.compile_callable(r.last.clone().context("no handle")?).await?
    };
    // Enter on the landing moves the screen; the panels callee it wakes
    // must not see that Enter. A second Enter is the one legitimate count.
    handle_l.call(ValArray::from_iter_exact(["enter".into()].into_iter())).await?;
    for _ in 0..3 {
        let _e = gx.compile(arcstr::literal!("i64:0")).await?;
    }
    handle_l.call(ValArray::from_iter_exact(["enter".into()].into_iter())).await?;
    let deadline = tokio::time::sleep(Duration::from_secs(30));
    tokio::pin!(deadline);
    let mut last = None;
    loop {
        tokio::select! {
            _ = &mut deadline => break,
            batch = rx.recv() => match batch {
                None => bail!("runtime died"),
                Some(mut batch) => {
                    for ev in batch.drain(..) {
                        if let GXEvent::Updated(id, v) = ev {
                            if id == expr_id {
                                last = Some(v.clone());
                                if v == Value::I64(2) {
                                    bail!("the woken callee counted the key that woke it");
                                }
                            }
                        }
                    }
                    if last == Some(Value::I64(1)) {
                        tokio::time::sleep(Duration::from_millis(500)).await;
                        while let Ok(mut b) = rx.try_recv() {
                            for ev in b.drain(..) {
                                if let GXEvent::Updated(id, v) = ev {
                                    if id == expr_id && v == Value::I64(2) {
                                        bail!("the woken callee counted the key late");
                                    }
                                }
                            }
                        }
                        return Ok(());
                    }
                }
            }
        }
    }
    bail!("the second Enter never counted (last={last:?})")
}

/// The dispatcher hands one callee the pattern bind `ev` and the other
/// the formal `e` itself. `ev` is a facet of `e`, so the landing arm's
/// read of `ev` consumes `e`'s fire, and the panels callee woken by the
/// screen change does not catch the key up.
const ALIAS: &str = r#"
let screen: [`Landing, `Panels] = `Landing;
let opened = 0;
let pan_handle = |e: string| -> i64 select e {
  "enter" => { opened <- e ~ opened + 1; 1 },
  _ => 0
};
let land_handle = |e: string| -> i64 select e {
  "enter" => { screen <- e ~ `Panels; 1 },
  _ => 0
};
let handle = |e: string| -> i64 select e {
  "" => 0,
  ev => select screen {
    `Landing => land_handle(ev),
    `Panels => pan_handle(e)
  }
};
let result = opened
"#;

#[tokio::test(flavor = "multi_thread")]
async fn alias_read_consumes_the_formals_fire() -> Result<()> {
    let (tx, mut rx) = mpsc::channel(100);
    let tbl = AHashMap::from_iter([(
        Path::from("/test.gx"),
        graphix_compiler::expr::VfsEntry::from(arcstr::ArcStr::from(ALIAS)),
    )]);
    let resolver = VfsResolver::new(tbl);
    let ctx =
        testing::init_with_resolvers(tx, crate::TEST_REGISTER, vec![resolver]).await?;
    let gx: graphix_rt::GXHandle<NoExt> = ctx.rt.clone();
    let compiled = gx.compile(arcstr::literal!("{ mod test; test::result }")).await?;
    let expr_id = compiled.exprs.last().context("no exprs")?.id;
    let handle_l = {
        let r = gx.compile_ref(find_bind_id(&compiled.env, "test::handle")?).await?;
        gx.compile_callable(r.last.clone().context("no handle")?).await?
    };
    handle_l.call(ValArray::from_iter_exact(["enter".into()].into_iter())).await?;
    for _ in 0..3 {
        let _e = gx.compile(arcstr::literal!("i64:0")).await?;
    }
    handle_l.call(ValArray::from_iter_exact(["enter".into()].into_iter())).await?;
    let deadline = tokio::time::sleep(Duration::from_secs(30));
    tokio::pin!(deadline);
    let mut last = None;
    loop {
        tokio::select! {
            _ = &mut deadline => break,
            batch = rx.recv() => match batch {
                None => bail!("runtime died"),
                Some(mut batch) => {
                    for ev in batch.drain(..) {
                        if let GXEvent::Updated(id, v) = ev {
                            if id == expr_id {
                                last = Some(v.clone());
                                if v == Value::I64(2) {
                                    bail!("the woken callee caught up a key its alias had consumed");
                                }
                            }
                        }
                    }
                    if last == Some(Value::I64(1)) {
                        tokio::time::sleep(Duration::from_millis(500)).await;
                        while let Ok(mut b) = rx.try_recv() {
                            for ev in b.drain(..) {
                                if let GXEvent::Updated(id, v) = ev {
                                    if id == expr_id && v == Value::I64(2) {
                                        bail!("the woken callee caught up the key late");
                                    }
                                }
                            }
                        }
                        return Ok(());
                    }
                }
            }
        }
    }
    bail!("the second Enter never counted (last={last:?})")
}

#[tokio::test(flavor = "multi_thread")]
async fn update_callable_keeps_the_site_for_the_same_lambda() -> Result<()> {
    let (tx, _rx) = mpsc::channel(100);
    let tbl = AHashMap::from_iter([(
        Path::from("/test.gx"),
        graphix_compiler::expr::VfsEntry::from(arcstr::ArcStr::from(PROG)),
    )]);
    let resolver = VfsResolver::new(tbl);
    let ctx =
        testing::init_with_resolvers(tx, crate::TEST_REGISTER, vec![resolver]).await?;
    let gx: graphix_rt::GXHandle<NoExt> = ctx.rt.clone();
    let compiled = gx.compile(arcstr::literal!("{ mod test; test::result }")).await?;
    let lambda = |name: &str| -> Result<Value> {
        let bid = find_bind_id(&compiled.env, name)?;
        let gx = gx.clone();
        Ok(tokio::task::block_in_place(|| {
            tokio::runtime::Handle::current()
                .block_on(async move { gx.compile_ref(bid).await })
        })?
        .last
        .clone()
        .context("no value")?)
    };
    let handle = lambda("test::handle")?;
    let poke = lambda("test::poke")?;
    let mut current = None;
    gx.update_callable(&mut current, handle.clone()).await?;
    let first = current.as_ref().context("no callable")?.id();
    gx.update_callable(&mut current, handle).await?;
    assert_eq!(current.as_ref().context("no callable")?.id(), first);
    gx.update_callable(&mut current, poke).await?;
    assert_ne!(current.as_ref().context("no callable")?.id(), first);
    Ok(())
}

/// A handler's select that slept while another screen had the keys
/// wakes to a key it never saw. It selects the arm for that key, but the
/// key is a past event: the arm's bind is stale and `kk ~ ..` stays quiet.
const WAKE_SWITCH: &str = r#"
type Key = [`Enter, `Esc, `Other];
let screen: [`Landing, `Panels] = `Panels;
let closed = 0;
let pan_handle = |e: Key| -> i64 select e {
  kk@ `Enter => { screen <- kk ~ `Landing; 1 },
  kk@ `Esc => { closed <- kk ~ closed + 1; 2 },
  `Other => 0
};
let land_handle = |e: Key| -> i64 select e {
  kk@ `Esc => { screen <- kk ~ `Panels; 1 },
  _ => 0
};
let handle = |e: Key| -> i64 select e {
  `Other => 0,
  ev => select screen {
    `Landing => land_handle(ev),
    `Panels => pan_handle(ev)
  }
};
let result = closed
"#;

#[tokio::test(flavor = "multi_thread")]
async fn arm_wake_switch_binds_the_standing_key_stale() -> Result<()> {
    let (tx, mut rx) = mpsc::channel(100);
    let tbl = AHashMap::from_iter([(
        Path::from("/test.gx"),
        graphix_compiler::expr::VfsEntry::from(arcstr::ArcStr::from(WAKE_SWITCH)),
    )]);
    let resolver = VfsResolver::new(tbl);
    let ctx =
        testing::init_with_resolvers(tx, crate::TEST_REGISTER, vec![resolver]).await?;
    let gx: graphix_rt::GXHandle<NoExt> = ctx.rt.clone();
    let compiled = gx.compile(arcstr::literal!("{ mod test; test::result }")).await?;
    let expr_id = compiled.exprs.last().context("no exprs")?.id;
    let handle_l = {
        let r = gx.compile_ref(find_bind_id(&compiled.env, "test::handle")?).await?;
        gx.compile_callable(r.last.clone().context("no handle")?).await?
    };
    // Enter on the panels selects its Enter arm and moves to the landing,
    // so the panels handler sleeps. Esc on the landing moves back: the
    // panels handler wakes to Esc and must not count it. A second Esc is
    // the one legitimate count.
    for k in ["Enter", "Esc"] {
        handle_l.call(ValArray::from_iter_exact([k.into()].into_iter())).await?;
        for _ in 0..3 {
            let _e = gx.compile(arcstr::literal!("i64:0")).await?;
        }
    }
    handle_l.call(ValArray::from_iter_exact(["Esc".into()].into_iter())).await?;
    let deadline = tokio::time::sleep(Duration::from_secs(30));
    tokio::pin!(deadline);
    let mut last = None;
    loop {
        tokio::select! {
            _ = &mut deadline => break,
            batch = rx.recv() => match batch {
                None => bail!("runtime died"),
                Some(mut batch) => {
                    for ev in batch.drain(..) {
                        if let GXEvent::Updated(id, v) = ev {
                            if id == expr_id {
                                last = Some(v.clone());
                                if v == Value::I64(2) {
                                    bail!("the woken handler counted the key that woke it");
                                }
                            }
                        }
                    }
                    if last == Some(Value::I64(1)) {
                        tokio::time::sleep(Duration::from_millis(500)).await;
                        while let Ok(mut b) = rx.try_recv() {
                            for ev in b.drain(..) {
                                if let GXEvent::Updated(id, v) = ev {
                                    if id == expr_id && v == Value::I64(2) {
                                        bail!("the woken handler counted the key late");
                                    }
                                }
                            }
                        }
                        return Ok(());
                    }
                }
            }
        }
    }
    bail!("the second Esc never counted (last={last:?})")
}
