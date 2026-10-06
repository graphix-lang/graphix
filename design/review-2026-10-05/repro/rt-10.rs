//! rt-10: compile_callable types its argument references with the
//! lambda's own scheme cells (graphix-rt/src/gx.rs:938-941) while
//! genn::apply checks the call against an instantiated copy; the
//! callable's check merges the copy into the definition's cells and its
//! settle binds them, so the definition's scheme changes for every call
//! compiled after it.
//!
//! command: copy to stdlib/graphix-tests/tests/review_rt_10.rs, then
//!   timeout -s KILL 2400 cargo test -p graphix-tests --test review_rt_10 -- --nocapture
//!
//! expected: compiling a callable for a generic definition changes nothing
//!   later call sites of it check.
//! observed (HEAD c722befe, dev profile):
//!   h = 'a: Number |x: 'a| -> 'a x
//!     before: `test::h(1) + 1`, `test::h(1.5) + 1.5` compile
//!     after compile_ref: `test::h(3) + 3` compiles
//!     after compile_callable: `test::h(2) + 2` REFUSED "Number + i64:
//!       arithmetic is fn('a: Number, 'a) -> 'a", and `test::h(2.5) + 2.5`
//!       likewise (h's 'a is settled to Number)
//!   g = |x| x
//!     before: `test::g(1) + 1`, `str::len(test::g("s"))` compile
//!     after compile_callable: both REFUSED "type mismatch '_: _ does not
//!       contain i64" / "... string" (g's parameter is settled to bottom);
//!       the callable's own call(1) is still accepted
//!   k = |x| x, also passed as `let kref: &fn(x: i64) -> i64 = &k` (the
//!   shape of a widget's handler argument): `test::k("s")` still compiles
//!   before the callable, both calls are REFUSED after it.
//!   A running TUI's instances are unaffected: a generic handler also
//!   called inside a growing array::map keeps binding new slots.

use ahash::AHashMap;
use anyhow::{Context, Result, bail};
use arcstr::{ArcStr, literal};
use graphix_compiler::{
    BindId,
    env::Env,
    expr::{VfsEntry, VfsResolver},
};
use graphix_package_core::testing::{self, PackageRef};
use graphix_rt::{GXHandle, NoExt};
use netidx::path::Path;
use tokio::sync::mpsc;

const PROG: &str = r#"
let h = 'a: Number |x: 'a| -> 'a x;
let g = |x| x;
let k = |x| x;
let kref: &fn(x: i64) -> i64 = &k;
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

async fn check(gx: &GXHandle<NoExt>, what: &str, text: &'static str) -> bool {
    match gx.compile(ArcStr::from(text)).await {
        Ok(_r) => {
            eprintln!("  {what}: `{text}` compiles");
            true
        }
        Err(e) => {
            let e = format!("{e:#}");
            eprintln!("  {what}: `{text}` REFUSED: {}", e.lines().last().unwrap_or(""));
            false
        }
    }
}

#[tokio::test(flavor = "multi_thread")]
async fn generic_callable_changes_later_checks() -> Result<()> {
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
    let h_bid = find_bind_id(&m.env, "test", "h")?;
    let b1 = check(&gx, "before", "test::h(1) + 1").await;
    let b2 = check(&gx, "before", "test::h(1.5) + 1.5").await;
    let r = gx.compile_ref(h_bid).await?;
    let m1 = check(&gx, "after compile_ref", "test::h(3) + 3").await;
    let lambda = r.last.clone().context("h has no value")?;
    let c = gx.compile_callable(lambda).await?;
    let a1 = check(&gx, "after compile_callable", "test::h(2) + 2").await;
    let a2 = check(&gx, "after compile_callable", "test::h(2.5) + 2.5").await;
    eprintln!("rt-10 verdict: before ({b1}, {b2}) after compile_ref {m1} after compile_callable ({a1}, {a2})");
    let gb1 = check(&gx, "g before", "test::g(1) + 1").await;
    let gb2 = check(&gx, "g before", "str::len(test::g(\"s\"))").await;
    let gr = gx.compile_ref(find_bind_id(&m.env, "test", "g")?).await?;
    let gc = gx.compile_callable(gr.last.clone().context("g has no value")?).await?;
    let ga1 = check(&gx, "g after compile_callable", "test::g(2) + 2").await;
    let ga2 = check(&gx, "g after compile_callable", "str::len(test::g(\"t\"))").await;
    eprintln!("rt-10 unconstrained g: before ({gb1}, {gb2}) after ({ga1}, {ga2})");
    match gc.call(netidx::protocol::valarray::ValArray::from_iter_exact(
        [netidx::publisher::Value::I64(1)].into_iter(),
    )).await {
        Ok(()) => eprintln!("rt-10 g's callable: call(1) accepted"),
        Err(e) => eprintln!("rt-10 g's callable: call(1) REFUSED: {e:#}"),
    }
    let kb1 = check(&gx, "k (behind &fn(i64)) before", "test::k(1) + 1").await;
    let kb2 = check(&gx, "k (behind &fn(i64)) before", "str::len(test::k(\"s\"))").await;
    let kr = gx.compile_ref(find_bind_id(&m.env, "test", "k")?).await?;
    let kc = gx.compile_callable(kr.last.clone().context("k has no value")?).await?;
    let ka1 = check(&gx, "k after compile_callable", "test::k(2) + 2").await;
    let ka2 = check(&gx, "k after compile_callable", "str::len(test::k(\"t\"))").await;
    eprintln!("rt-10 k behind a typed ref: before ({kb1}, {kb2}) after ({ka1}, {ka2})");
    drop(kc);
    drop(kr);
    drop(gc);
    drop(gr);
    drop(c);
    drop(r);
    drop(m);
    ctx.shutdown().await;
    drain.abort();
    if !(b1 && b2 && m1 && a1 && a2 && ga1 && ga2 && ka1 && ka2) {
        bail!("a later check changed after compile_callable");
    }
    Ok(())
}
