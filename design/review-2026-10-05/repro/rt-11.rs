//! rt-11: delete_callable (graphix-rt/src/gx.rs:1003-1009) deletes the
//! call site but never removes the store entries its argument ids got
//! when Call delivered them (push_var_event, gx.rs:455), unlike
//! SynthCall::delete (graphix-compiler/src/node/genn.rs), which does.
//!
//! command: copy to stdlib/graphix-tests/tests/review_rt_11.rs, then
//!   timeout -s KILL 2400 cargo test -p graphix-tests --test review_rt_11 -- --nocapture
//!
//! expected: store_len returns to its baseline once each callable is
//!   dropped, called or not.
//! observed (HEAD c722befe, dev profile):
//!   20 x compile_callable + drop (never called): store_len +0
//!   20 x compile_callable + call + drop:         store_len +20
//!   (one stranded entry per dropped callable, holding its last argument)

use ahash::AHashMap;
use anyhow::{Context, Result, bail};
use arcstr::{ArcStr, literal};
use graphix_compiler::{
    BindId,
    env::Env,
    expr::{VfsEntry, VfsResolver},
};
use graphix_package_core::testing::{self, PackageRef};
use netidx::{path::Path, protocol::valarray::ValArray, publisher::Value};
use std::time::Duration;
use tokio::sync::mpsc;

const PROG: &str = r#"
let h = |x: i64| -> i64 x + 1;
let result = 0
"#;

fn find_bind_id(env: &Env, module: &str, var: &str) -> Result<BindId> {
    let suffix = format!("/{module}");
    for (scope, vars) in &env.binds {
        if Path::as_ref(&scope.0).ends_with(&suffix) {
            if let Some(bid) = vars.get(var) {
                return Ok(*bid);
            }
        }
    }
    bail!("no binding {module}::{var}")
}

#[tokio::test(flavor = "multi_thread")]
async fn dropped_callables_strand_their_args() -> Result<()> {
    let (tx, mut rx) = mpsc::channel(1000);
    let packages: &[PackageRef] = graphix_package::package_refs!();
    let tbl = AHashMap::from_iter([(
        Path::from("/test.gx"),
        VfsEntry::from(ArcStr::from(PROG)),
    )]);
    let ctx =
        testing::init_with_resolvers(tx, packages, vec![VfsResolver::new(tbl)]).await?;
    let gx = ctx.rt.clone();
    let drain = tokio::spawn(async move { while rx.recv().await.is_some() {} });
    let m = gx.compile(literal!("mod test")).await?;
    let r = gx.compile_ref(find_bind_id(&m.env, "test", "h")?).await?;
    let lambda = r.last.clone().context("h has no value")?;
    const N: i64 = 20;
    let mut growth = vec![];
    for call in [false, true] {
        tokio::time::sleep(Duration::from_millis(100)).await;
        let s0 = gx.env_stats().await?.store_len;
        for i in 0..N {
            let c = gx.compile_callable(lambda.clone()).await?;
            if call {
                c.call(ValArray::from_iter_exact([Value::I64(i)].into_iter())).await?;
            }
            tokio::time::sleep(Duration::from_millis(20)).await;
            drop(c);
        }
        tokio::time::sleep(Duration::from_millis(100)).await;
        let s1 = gx.env_stats().await?.store_len;
        let d = s1 as i64 - s0 as i64;
        eprintln!("rt-11: {N} x compile_callable{} + drop: store_len {s0} -> {s1} ({d:+})",
            if call { " + call" } else { "" });
        growth.push(d);
    }
    drop(r);
    drop(m);
    ctx.shutdown().await;
    drain.abort();
    if growth.iter().any(|d| *d != 0) {
        bail!("dropped callables left store entries: {growth:?}");
    }
    Ok(())
}
