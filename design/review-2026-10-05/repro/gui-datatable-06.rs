//! gui-datatable-06: no Grid role when every subscribed column is a sort
//! column, so on_update and sparkline history never fire.
//!
//! subscriptions.rs: `apply_table_sync` (:348-352) calls
//! `subscribe_sort_column` (:479-501) before `update_subscriptions`; that
//! inserts `cells[(row, col)]` with a `SortMarker` role only, and
//! `update_subscriptions` counts a row as subscribed when `cells` has an
//! entry for every subscribed displayed column (:565-571), so
//! `subscribe_row` never runs. The dispatch task fires `on_update` and
//! records sparkline points only for `Grid` roles (:193-235).
//!
//! Command (with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_datatable_06.rs):
//!   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_gui_datatable_06 -- --nocapture
//!
//! Each case compiles a `data_table` as the GUI harness does
//! (`widgets::compile`), feeds it every runtime update (`handle_update`)
//! and calls `before_view` the way the event loop does before a render.
//! Row order is read back by clicking (row 0, "c0"): `on_select` reports
//! the path of the row drawn first, which shows the published values
//! reached the widget and sorted it.
//!
//! Expected: on_update fires for every update of a displayed subscribed
//! cell, whatever `sort_by` holds (all four cases pass).
//! Observed at c722befe (dev profile): a and c pass, b and d FAIL:
//!   a_control_no_sort (columns ["c0"], no sort_by): log r0/c0=1,
//!     r1/c0=2 at 9 ms, r0/c0=3 at 2509 ms.
//!   c_control_sort_with_second_column (columns ["c0", "c1"], sort_by c0):
//!     log r0/c0=1, r1/c0=2, r0/c1=x0, r1/c1=x1, then r0/c0=3 at 2509 ms.
//!   b_sort_on_only_column (case a plus sort_by c0 descending): r1 is drawn
//!     first at 263 ms and r0 after its bump to 3 at 2677 ms, so the values
//!     arrive and sort the table; on_update entries: 0.
//!   d_table_update_after_runtime_sort (sort_by set at 800 ms, table
//!     replaced with a third row at 2400 ms, r0 bumped to 0 at 3200 ms):
//!     log r0/c0=1, r1/c0=2, then r0/c0=3 at 1610 ms (the Grid role from
//!     compile survives the runtime sort_by change); after the table
//!     update nothing more is logged, though r1 is drawn first at 3358 ms
//!     (r0's 0 arrived): r0/c0=0 logged false, r2/c0=1 logged false.

use ahash::AHashMap;
use anyhow::{Context, Result, bail};
use graphix_compiler::{
    BindId,
    env::Env,
    expr::{ExprId, VfsEntry, VfsResolver},
};
use graphix_package_core::testing::{self, PackageRef, TestCtx};
use graphix_package_gui::widgets::{self, GuiW, Message, MessageShell};
use graphix_rt::{CompRes, GXEvent, GXHandle, NoExt, Ref};
use netidx::publisher::Value;
use poolshark::global::GPooled;
use std::time::{Duration, Instant};
use tokio::sync::mpsc;

const REGISTER: &[PackageRef] = &[
    &graphix_package_core::P,
    &graphix_package_array::P,
    &graphix_package_map::P,
    &graphix_package_str::P,
    &graphix_package_sys::P,
    &graphix_package_gui::P,
];

const CASE_A: &str = r#"
use gui::*; use gui::data_table::{self, *}; use sys::*; use sys::time::{self, *};
let log = "";
let sel = "";
let v0 = 1;
let t1 = time::timer(duration:2500.ms, false);
v0 <- t1 ~ 3;
sys::net::publish("/local/dt06a/r0/c0", v0);
sys::net::publish("/local/dt06a/r1/c0", 2);
let tbl = { rows: ["/local/dt06a/r0", "/local/dt06a/r1"], columns: ["c0"] };
let result = data_table(
    #on_update: |#path: string, #value: Primitive| log <- "[path]=[value]",
    #on_select: |#path: string| sel <- path,
    #table: &tbl
)
"#;

const CASE_B: &str = r#"
use gui::*; use gui::data_table::{self, *}; use sys::*; use sys::time::{self, *};
let log = "";
let sel = "";
let v0 = 1;
let t1 = time::timer(duration:2500.ms, false);
v0 <- t1 ~ 3;
sys::net::publish("/local/dt06b/r0/c0", v0);
sys::net::publish("/local/dt06b/r1/c0", 2);
let tbl = { rows: ["/local/dt06b/r0", "/local/dt06b/r1"], columns: ["c0"] };
let result = data_table(
    #sort_by: &[{ column: "c0", direction: `Descending }],
    #on_update: |#path: string, #value: Primitive| log <- "[path]=[value]",
    #on_select: |#path: string| sel <- path,
    #table: &tbl
)
"#;

const CASE_C: &str = r#"
use gui::*; use gui::data_table::{self, *}; use sys::*; use sys::time::{self, *};
let log = "";
let sel = "";
let v0 = 1;
let t1 = time::timer(duration:2500.ms, false);
v0 <- t1 ~ 3;
sys::net::publish("/local/dt06c/r0/c0", v0);
sys::net::publish("/local/dt06c/r1/c0", 2);
sys::net::publish("/local/dt06c/r0/c1", "x0");
sys::net::publish("/local/dt06c/r1/c1", "x1");
let tbl = { rows: ["/local/dt06c/r0", "/local/dt06c/r1"], columns: ["c0", "c1"] };
let result = data_table(
    #sort_by: &[{ column: "c0", direction: `Descending }],
    #on_update: |#path: string, #value: Primitive| log <- "[path]=[value]",
    #on_select: |#path: string| sel <- path,
    #table: &tbl
)
"#;

const CASE_D: &str = r#"
use gui::*; use gui::data_table::{self, *}; use sys::*; use sys::time::{self, *};
let log = "";
let sel = "";
let v0 = 1;
let t1 = time::timer(duration:800.ms, false);
let t2 = time::timer(duration:1600.ms, false);
let t3 = time::timer(duration:2400.ms, false);
let t4 = time::timer(duration:3200.ms, false);
let sb: Array<SortBy> = [];
sb <- t1 ~ [{ column: "c0", direction: `Descending }];
v0 <- t2 ~ 3;
v0 <- t4 ~ 0;
sys::net::publish("/local/dt06d/r0/c0", v0);
sys::net::publish("/local/dt06d/r1/c0", 2);
sys::net::publish("/local/dt06d/r2/c0", 1);
let tbl = { rows: ["/local/dt06d/r0", "/local/dt06d/r1"], columns: ["c0"] };
tbl <- t3 ~ { rows: ["/local/dt06d/r0", "/local/dt06d/r1", "/local/dt06d/r2"], columns: ["c0"] };
let result = data_table(
    #sort_by: &sb,
    #on_update: |#path: string, #value: Primitive| log <- "[path]=[value]",
    #on_select: |#path: string| sel <- path,
    #table: &tbl
)
"#;

struct H {
    _ctx: TestCtx,
    gx: GXHandle<NoExt>,
    /// Dropping the compile result unloads the program.
    _compiled: CompRes<NoExt>,
    rx: mpsc::Receiver<GPooled<Vec<GXEvent>>>,
    rt: tokio::runtime::Handle,
    widget: GuiW<NoExt>,
    start: Instant,
    log_id: ExprId,
    sel_id: ExprId,
    /// Every value `log` took, with the ms since start it arrived.
    log: Vec<(u128, String)>,
    sel: String,
    _refs: Vec<Ref<NoExt>>,
}

fn find_bind_id(env: &Env, name: &str) -> Result<BindId> {
    use netidx::path::Path;
    let (module, var) = name.split_once("::").context("module::var")?;
    let suffix = format!("/{module}");
    for (scope, vars) in &env.binds {
        if Path::as_ref(&scope.0).ends_with(&suffix) {
            if let Some(bid) = vars.get(var) {
                return Ok(*bid);
            }
        }
    }
    bail!("no binding {name}")
}

fn string_of(v: &Value) -> String {
    match v {
        Value::String(s) => s.to_string(),
        v => format!("{v}"),
    }
}

impl H {
    async fn new(code: &str) -> Result<Self> {
        let (tx, mut rx) = mpsc::channel(100);
        let tbl = AHashMap::from_iter([(
            netidx_core::path::Path::from("/test.gx"),
            VfsEntry::from(arcstr::ArcStr::from(code)),
        )]);
        let ctx =
            testing::init_with_resolvers(tx, REGISTER, vec![VfsResolver::new(tbl)]).await?;
        let gx = ctx.rt.clone();
        let start = Instant::now();
        let compiled: CompRes<NoExt> = gx
            .compile(arcstr::literal!("{ mod test; test::result }"))
            .await
            .context("compile")?;
        let root_id = compiled.exprs[0].id;
        let timeout = tokio::time::sleep(Duration::from_secs(30));
        tokio::pin!(timeout);
        let root = loop {
            tokio::select! {
                biased;
                Some(mut batch) = rx.recv() => {
                    if let Some(v) = batch.drain(..).find_map(|e| match e {
                        GXEvent::Updated(id, v) if id == root_id => Some(v),
                        _ => None,
                    }) {
                        break v;
                    }
                }
                _ = &mut timeout => bail!("timeout waiting for the root value"),
            }
        };
        let log_ref = gx.compile_ref(find_bind_id(&compiled.env, "test::log")?).await?;
        let sel_ref = gx.compile_ref(find_bind_id(&compiled.env, "test::sel")?).await?;
        let log0 = log_ref.last.as_ref().map(string_of).unwrap_or_default();
        let widget = widgets::compile(gx.clone(), root).await.context("compile widget")?;
        Ok(Self {
            _ctx: ctx,
            gx,
            _compiled: compiled,
            rx,
            rt: tokio::runtime::Handle::current(),
            widget,
            start,
            log_id: log_ref.id,
            sel_id: sel_ref.id,
            log: vec![(start.elapsed().as_millis(), log0)],
            sel: String::new(),
            _refs: vec![log_ref, sel_ref],
        })
    }

    /// Deliver updates for `d` as the event loop does: every update to
    /// the widget, then `before_view` before the next frame.
    async fn pump(&mut self, d: Duration) -> Result<()> {
        let deadline = tokio::time::Instant::now() + d;
        loop {
            let tick = tokio::time::sleep(Duration::from_millis(20));
            tokio::select! {
                biased;
                Some(mut batch) = self.rx.recv() => {
                    for event in batch.drain(..) {
                        if let GXEvent::Updated(id, v) = event {
                            if id == self.log_id {
                                self.log.push((self.start.elapsed().as_millis(), string_of(&v)));
                            }
                            if id == self.sel_id {
                                self.sel = string_of(&v);
                            }
                            let (w, rt) = (&mut self.widget, &self.rt);
                            match rt.runtime_flavor() {
                                tokio::runtime::RuntimeFlavor::MultiThread => {
                                    tokio::task::block_in_place(|| w.handle_update(rt, id, &v))?;
                                }
                                _ => {
                                    w.handle_update(rt, id, &v)?;
                                }
                            }
                        }
                    }
                }
                _ = tick => {}
            }
            self.widget.before_view();
            if tokio::time::Instant::now() >= deadline {
                return Ok(());
            }
        }
    }

    /// The path of the cell drawn at (row 0, "c0"), via `on_select`.
    async fn first_row(&mut self) -> Result<String> {
        self.sel.clear();
        let mut shell = MessageShell::new(iced_core::Point::ORIGIN);
        self.widget.on_message(&Message::CellClick(0, arcstr::literal!("c0")), &mut shell);
        for m in shell.out.drain(..) {
            if let Message::Call(id, args) = m {
                self.gx.call(id, args)?;
            }
        }
        self.pump(Duration::from_millis(120)).await?;
        Ok(self.sel.clone())
    }

    /// Click-poll until `want` is drawn first or `within` elapses.
    async fn wait_first_row(&mut self, want: &str, within: Duration) -> Result<bool> {
        let deadline = Instant::now() + within;
        while Instant::now() < deadline {
            if self.first_row().await? == want {
                println!("  {} ms: {want} is drawn first", self.start.elapsed().as_millis());
                return Ok(true);
            }
        }
        println!("  {} ms: {want} never drawn first", self.start.elapsed().as_millis());
        Ok(false)
    }

    async fn wait_log(&mut self, entry: &str, within: Duration) -> Result<bool> {
        let deadline = Instant::now() + within;
        while Instant::now() < deadline {
            if self.logged(entry) {
                return Ok(true);
            }
            self.pump(Duration::from_millis(100)).await?;
        }
        Ok(self.logged(entry))
    }

    fn logged(&self, entry: &str) -> bool {
        self.log.iter().any(|(_, s)| s == entry)
    }

    fn report(&self, case: &str) {
        println!("{case}: on_update log history (ms since compile: value):");
        for (t, s) in self.log.iter() {
            println!("  {t:>5}: {s:?}");
        }
    }
}

#[tokio::test(flavor = "multi_thread")]
async fn a_control_no_sort() -> Result<()> {
    let mut h = H::new(CASE_A).await?;
    let saw_bump = h.wait_log("/local/dt06a/r0/c0=3", Duration::from_secs(6)).await?;
    h.report("a_control_no_sort");
    assert!(h.logged("/local/dt06a/r0/c0=1") && h.logged("/local/dt06a/r1/c0=2") && saw_bump);
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn b_sort_on_only_column() -> Result<()> {
    let mut h = H::new(CASE_B).await?;
    println!("b_sort_on_only_column: row order through on_select:");
    let sorted = h.wait_first_row("/local/dt06b/r1/c0", Duration::from_secs(2)).await?;
    let bumped = h.wait_first_row("/local/dt06b/r0/c0", Duration::from_secs(6)).await?;
    h.pump(Duration::from_millis(500)).await?;
    h.report("b_sort_on_only_column");
    println!(
        "EXPECTED: as case a, on_update logs r0=1, r1=2, r0=3\n\
         OBSERVED: the values reached the widget (r1 drawn first: {sorted}, r0 drawn first \
         after its bump to 3: {bumped}); on_update entries: {}",
        h.log.len() - 1
    );
    assert!(sorted && bumped, "harness: the values did not reach the widget");
    assert!(
        h.logged("/local/dt06b/r0/c0=1")
            && h.logged("/local/dt06b/r1/c0=2")
            && h.logged("/local/dt06b/r0/c0=3"),
        "on_update never fired for a column that is both the only subscribed column and the sort column"
    );
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn c_control_sort_with_second_column() -> Result<()> {
    let mut h = H::new(CASE_C).await?;
    let saw_bump = h.wait_log("/local/dt06c/r0/c0=3", Duration::from_secs(6)).await?;
    h.report("c_control_sort_with_second_column");
    assert!(h.logged("/local/dt06c/r0/c0=1") && h.logged("/local/dt06c/r1/c0=2") && saw_bump);
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn d_table_update_after_runtime_sort() -> Result<()> {
    let mut h = H::new(CASE_D).await?;
    let saw3 = h.wait_log("/local/dt06d/r0/c0=3", Duration::from_secs(6)).await?;
    println!("d_table_update_after_runtime_sort: row order through on_select:");
    let resorted = h.wait_first_row("/local/dt06d/r1/c0", Duration::from_secs(6)).await?;
    h.pump(Duration::from_millis(500)).await?;
    h.report("d_table_update_after_runtime_sort");
    println!(
        "EXPECTED: r0=3 logged after sort_by is set at 800 ms, then after the table update \
         at 2400 ms the cells logged again and r0=0 at 3200 ms\n\
         OBSERVED: r0=3 logged: {saw3}; r1 drawn first after r0's bump to 0 (the value \
         reached the widget): {resorted}; r0=0 logged: {}; r2 logged: {}",
        h.logged("/local/dt06d/r0/c0=0"),
        h.logged("/local/dt06d/r2/c0=1")
    );
    assert!(saw3 && resorted, "harness: the values did not reach the widget");
    assert!(
        h.logged("/local/dt06d/r0/c0=0") && h.logged("/local/dt06d/r2/c0=1"),
        "on_update stopped firing after a table update while a sort covers every subscribed column"
    );
    Ok(())
}
