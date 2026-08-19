//! The embedder-callable path (`GXHandle::compile_callable`) — how a
//! GUI/TUI package dispatches a graphix handler per external event.
//! The callable's callee instances are born lazily on the cycle of
//! their first real dispatch, AFTER any `&state` reference value in
//! the enclosing body was delivered — so the write side
//! (`ConnectDeref`) must resolve its target from the standing store,
//! exactly as `Deref`'s read side does. Before the 2026-08-19 fix the
//! target stayed unresolved and every `*st <- v` reached through the
//! callable was silently dropped (found by graphix-package-tui's
//! line_edit under TuiTestHarness).

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

fn find_bind_id(env: &Env, name: &str) -> Result<graphix_compiler::BindId> {
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
    // Interpose cycles between the callable's init and its first
    // dispatch — the embedder shape (a TUI renders between building
    // the handler and the first key). The reference value delivered
    // at init must survive to the instance the dispatch creates.
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
