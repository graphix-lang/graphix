//! tests-ui-06: GUI test harness duplicates the TUI harness and kept the
//! 100 ms quiet-window drain the TUI replaced with wait_idle (2180ab7a).
//!
//! GuiTestHarness::drain (stdlib/graphix-package-gui/src/test/mod.rs:91-115)
//! returns 100 ms after it starts when no batch comes, else 50 ms after the
//! last batch. `quiet_drain` below is that loop verbatim minus the widget
//! update; `idle_drain` is TuiTestHarness::drain_timed's loop (wait_idle).
//! `stall` holds the runtime's thread 300 ms before it handles the call, as
//! a descheduled thread would. `late_reply` has on_select_fires_on_click's
//! shape: watch, drain, call, one drain, read the watched value.
//!
//! Command, with this file copied to
//! stdlib/graphix-tests/tests/review_tests_ui_06.rs:
//!   timeout -s KILL 2400 cargo test -p graphix-tests --test review_tests_ui_06 -- --nocapture
//!
//! Expected: every drain delivers the reply to the call.
//! Observed at c722befe (3 runs, identical):
//!   current_thread_quiet_drain_keeps_late_reply  ok      drain 351 ms, last_clicked "r0/c0"
//!   multi_thread_quiet_drain_keeps_late_reply    FAILED  drain 101 ms, last_clicked ""
//!   multi_thread_idle_drain_keeps_late_reply     ok      drain 306 ms, last_clicked "r0/c0"
//!   drain_cost: an idle drain 101 ms quiet vs 3.2 ms wait_idle; after a call
//!               51 ms quiet vs 6.2 ms wait_idle
//! Under current_thread (157 of the 158 GUI tokio tests) the runtime runs on
//! the drain's thread and finishes its cycles before the drain can see its
//! timer, so a late synchronous reply is not lost there; under multi_thread
//! (interaction_test::stack_children_follow_rotation) it is. The GUI unit
//! test binary at HEAD: viewport_metrics_update_on_resize (20 idle drains)
//! takes 2.09 s, on_select_fires_on_click 0.34 s, mostly asleep.

use ahash::AHashMap;
use anyhow::{Context, Result, bail};
use graphix_compiler::{
    BindId,
    expr::{ExprId, VfsEntry, VfsResolver},
};
use graphix_package_core::testing::{self, TestCtx};
use graphix_rt::{Callable, CallableId, CompRes, GXEvent, GXHandle, NoExt, Ref};
use netidx::{protocol::valarray::ValArray, publisher::Value};
use nohash::IntMap;
use poolshark::global::GPooled;
use std::{
    future::Future,
    pin::pin,
    task::{Context as TaskContext, Waker},
    time::{Duration, Instant},
};
use tokio::sync::mpsc;

const REGISTER: &[&dyn graphix_package::Package<NoExt>] = &[
    &graphix_package_core::P,
    &graphix_package_array::P,
    &graphix_package_str::P,
    &graphix_package_sys::P,
];

const CODE: &str = r#"
let last_clicked = "";
let on_select = |path: string| last_clicked <- path;
let result = 0
"#;

struct H {
    _ctx: TestCtx,
    gx: GXHandle<NoExt>,
    compiled: CompRes<NoExt>,
    rx: mpsc::Receiver<GPooled<Vec<GXEvent>>>,
    watched: IntMap<ExprId, Value>,
    watch_names: AHashMap<String, ExprId>,
    _refs: Vec<Ref<NoExt>>,
    _callables: Vec<Callable<NoExt>>,
}

impl H {
    async fn new() -> Result<Self> {
        let (tx, mut rx) = mpsc::channel(100);
        let tbl = AHashMap::from_iter([(
            netidx_core::path::Path::from("/test.gx"),
            VfsEntry::from(arcstr::ArcStr::from(CODE)),
        )]);
        let ctx =
            testing::init_with_resolvers(tx, REGISTER, vec![VfsResolver::new(tbl)]).await?;
        let gx = ctx.rt.clone();
        let compiled = gx
            .compile(arcstr::literal!("{ mod test; test::result }"))
            .await
            .context("compile graphix code")?;
        let expr_id = compiled.exprs[0].id;
        let _ = wait_for_update(&mut rx, expr_id).await?;
        Ok(Self {
            _ctx: ctx,
            gx,
            compiled,
            rx,
            watched: IntMap::default(),
            watch_names: AHashMap::default(),
            _refs: Vec::new(),
            _callables: Vec::new(),
        })
    }

    /// GuiTestHarness::drain, minus `update_widget`.
    async fn quiet_drain(&mut self) -> Result<()> {
        let timeout = tokio::time::sleep(Duration::from_millis(100));
        tokio::pin!(timeout);
        loop {
            tokio::select! {
                biased;
                Some(mut batch) = self.rx.recv() => {
                    for event in batch.drain(..) {
                        if let GXEvent::Updated(id, v) = event {
                            if self.watched.contains_key(&id) {
                                self.watched.insert(id, v.clone());
                            }
                        }
                    }
                    timeout.as_mut().reset(
                        tokio::time::Instant::now() + Duration::from_millis(50)
                    );
                }
                _ = &mut timeout => break,
            }
        }
        Ok(())
    }

    /// TuiTestHarness::drain_timed's loop.
    async fn idle_drain(&mut self) -> Result<()> {
        loop {
            let idle = self.gx.wait_idle();
            tokio::pin!(idle);
            let mut delivered = false;
            loop {
                tokio::select! {
                    biased;
                    Some(mut batch) = self.rx.recv() => {
                        for event in batch.drain(..) {
                            if let GXEvent::Updated(id, v) = event {
                                delivered = true;
                                if self.watched.contains_key(&id) {
                                    self.watched.insert(id, v);
                                }
                            }
                        }
                    }
                    r = &mut idle => break r?,
                }
            }
            if !delivered {
                return Ok(());
            }
        }
    }

    async fn watch(&mut self, name: &str) -> Result<Value> {
        let bid = find_bind_id(&self.compiled.env, name)?;
        let r = self.gx.compile_ref(bid).await?;
        let initial = r.last.clone().unwrap_or(Value::Null);
        self.watched.insert(r.id, initial.clone());
        self.watch_names.insert(name.to_string(), r.id);
        self._refs.push(r);
        self.quiet_drain().await?;
        Ok(initial)
    }

    fn get_watched(&self, name: &str) -> Option<&Value> {
        self.watch_names.get(name).and_then(|eid| self.watched.get(eid))
    }

    async fn compile_named_callable(&mut self, name: &str) -> Result<CallableId> {
        let bid = find_bind_id(&self.compiled.env, name)?;
        let r = self.gx.compile_ref(bid).await?;
        let val = r.last.clone().with_context(|| format!("no value for {name}"))?;
        let cb = self.gx.compile_callable(val).await?;
        let id = cb.id();
        self._refs.push(r);
        self._callables.push(cb);
        Ok(id)
    }

    /// Queue a 300 ms hold of the runtime's thread ahead of what is sent
    /// next.
    fn stall(&self) {
        let fut = self.gx.with_ctx(|_| std::thread::sleep(Duration::from_millis(300)));
        let mut fut = pin!(fut);
        let _ = fut.as_mut().poll(&mut TaskContext::from_waker(Waker::noop()));
    }
}

async fn wait_for_update(
    rx: &mut mpsc::Receiver<GPooled<Vec<GXEvent>>>,
    target_id: ExprId,
) -> Result<Value> {
    let timeout = tokio::time::sleep(Duration::from_secs(5));
    tokio::pin!(timeout);
    loop {
        tokio::select! {
            biased;
            Some(mut batch) = rx.recv() => {
                for event in batch.drain(..) {
                    if let GXEvent::Updated(id, v) = event {
                        if id == target_id {
                            return Ok(v);
                        }
                    }
                }
            }
            _ = &mut timeout => bail!("timeout waiting for initial value"),
        }
    }
}

fn find_bind_id(env: &graphix_compiler::env::Env, name: &str) -> Result<BindId> {
    use netidx::path::Path;
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

/// on_select_fires_on_click's shape: watch, drain, call, one drain, read.
async fn late_reply(label: &str, idle: bool) -> Result<Option<Value>> {
    let mut h = H::new().await?;
    let _ = h.watch("test::last_clicked").await?;
    let cb = h.compile_named_callable("test::on_select").await?;
    h.quiet_drain().await?;
    h.stall();
    h.gx.call(cb, ValArray::from_iter([Value::String(arcstr::literal!("r0/c0"))]))?;
    let st = Instant::now();
    if idle {
        h.idle_drain().await?;
    } else {
        h.quiet_drain().await?;
    }
    let v = h.get_watched("test::last_clicked").cloned();
    eprintln!("[{label}] drain returned after {:?}; last_clicked = {v:?}", st.elapsed());
    Ok(v)
}

fn clicked() -> Option<Value> {
    Some(Value::String(arcstr::literal!("r0/c0")))
}

#[tokio::test(flavor = "current_thread")]
async fn current_thread_quiet_drain_keeps_late_reply() -> Result<()> {
    assert_eq!(late_reply("current_thread quiet", false).await?, clicked());
    Ok(())
}

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn multi_thread_quiet_drain_keeps_late_reply() -> Result<()> {
    assert_eq!(
        late_reply("multi_thread quiet", false).await?,
        clicked(),
        "the quiet-window drain returned before the reply to the call"
    );
    Ok(())
}

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn multi_thread_idle_drain_keeps_late_reply() -> Result<()> {
    assert_eq!(late_reply("multi_thread wait_idle", true).await?, clicked());
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn drain_cost() -> Result<()> {
    let mut h = H::new().await?;
    let _ = h.watch("test::last_clicked").await?;
    let cb = h.compile_named_callable("test::on_select").await?;
    h.idle_drain().await?;
    for i in 0..3 {
        let st = Instant::now();
        h.quiet_drain().await?;
        let quiet_idle = st.elapsed();
        let st = Instant::now();
        h.idle_drain().await?;
        let wait_idle_idle = st.elapsed();
        let path = Value::String(format!("p{i}").into());
        h.gx.call(cb, ValArray::from_iter([path.clone()]))?;
        let st = Instant::now();
        h.quiet_drain().await?;
        let quiet_call = st.elapsed();
        assert_eq!(h.get_watched("test::last_clicked"), Some(&path));
        let path = Value::String(format!("q{i}").into());
        h.gx.call(cb, ValArray::from_iter([path.clone()]))?;
        let st = Instant::now();
        h.idle_drain().await?;
        let wait_idle_call = st.elapsed();
        assert_eq!(h.get_watched("test::last_clicked"), Some(&path));
        eprintln!(
            "[drain_cost {i}] idle: quiet {quiet_idle:?} wait_idle {wait_idle_idle:?}; \
             after a call: quiet {quiet_call:?} wait_idle {wait_idle_call:?}"
        );
    }
    Ok(())
}
