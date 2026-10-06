//! gui-widgets-a.r2-11: GuiWidget child traversal is one slice; Table and
//! Tooltip hide children from before_view.
//!
//! `GuiWidget::before_view` (widgets/mod.rs:225) reaches only what
//! `children_mut()` returns. `TableW` (headers + cells) overrides
//! `on_message` but neither `before_view` nor `children_mut`, so a
//! `data_table` inside a `table` cell or header never runs its
//! `before_view` (data_table/mod.rs:292), the only per-frame consumer of
//! `sort_col_dirty`: a live change of its sort column is never re-sorted
//! until some unrelated widget ref update reaches `handle_update`.
//! `TooltipW::children_mut` returns only `child`, so a `tip` is cut off
//! the same way (not exercised here: iced's tooltip overlay delivers no
//! events, so a click cannot read a tip's row order).
//!
//! Command (with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_widgets_a_r2_11.rs):
//!   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_gui_widgets_a_r2_11 -- --nocapture
//!
//! Each case compiles the same `data_table` (rows r0=1, r1=2, r2=3 on
//! netidx column c0, sort_by c0 descending; r1 becomes 9 at 3000 ms)
//! under a different parent and drives it as event_loop.rs does: every
//! runtime update the widget tree subscribed to goes to the root's
//! `handle_update` (the harness's own `sel` ref is not forwarded, no
//! widget reads it), and the root's `before_view` runs every 20 ms, as
//! before each render. Row order is read by sending `CellClick(0, "c0")`
//! through the root's `on_message` (TableW forwards it): `on_select`
//! reports the path drawn first.
//!
//! Expected in every case: r2/c0 first, then r1/c0 after the bump.
//! Observed at c722befe (3 passed, 2 FAILED):
//!   a_root:            115 ms r2/c0 first; 3115 ms r1/c0 first      ok
//!   b_in_column:       118 ms r2/c0 first; 3117 ms r1/c0 first      ok
//!   c_in_table_cell:   r0/c0 first throughout (never sorted)       FAILED
//!   d_in_table_header: r0/c0 first throughout (never sorted)       FAILED
//!   e_in_table_cell_then_unrelated_update: as c until 7000 ms, then one
//!     `handle_update` for an id no widget owns sorts it (r1/c0 first) ok
//! No update reached `handle_update` after the bump in any case.

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

const TEMPLATE: &str = r#"
use gui::{column::column, data_table::data_table, table::{table, table_column}, text::text};
let sel = "";
let v1 = 2;
let t1 = sys::time::timer(duration:3000.ms, false);
v1 <- t1 ~ 9;
sys::net::publish("/local/PFX/r0/c0", 1);
sys::net::publish("/local/PFX/r1/c0", v1);
sys::net::publish("/local/PFX/r2/c0", 3);
let tbl = { rows: ["/local/PFX/r0", "/local/PFX/r1", "/local/PFX/r2"], columns: ["c0"] };
let dt = data_table(
    #sort_by: &[{ column: "c0", direction: `Descending }],
    #on_select: |#path: string| sel <- path,
    #table: &tbl
);
let result = WRAP
"#;

fn program(prefix: &str, wrap: &str) -> String {
    TEMPLATE.replace("PFX", prefix).replace("WRAP", wrap)
}

struct H {
    _ctx: TestCtx,
    /// Dropping it deletes the program (publishes and timer included).
    _compiled: CompRes<NoExt>,
    gx: GXHandle<NoExt>,
    rx: mpsc::Receiver<GPooled<Vec<GXEvent>>>,
    rt: tokio::runtime::Handle,
    widget: GuiW<NoExt>,
    start: Instant,
    root_id: ExprId,
    sel_id: ExprId,
    sel: String,
    sel_updates: usize,
    /// (ms since start) of every update forwarded to `handle_update`.
    forwarded: Vec<u128>,
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
        let sel_ref = gx.compile_ref(find_bind_id(&compiled.env, "test::sel")?).await?;
        let widget = widgets::compile(gx.clone(), root).await.context("compile widget")?;
        Ok(Self {
            _ctx: ctx,
            _compiled: compiled,
            gx,
            rx,
            rt: tokio::runtime::Handle::current(),
            widget,
            start,
            root_id,
            sel_id: sel_ref.id,
            sel: String::new(),
            sel_updates: 0,
            forwarded: vec![],
            _refs: vec![sel_ref],
        })
    }

    fn ms(&self) -> u128 {
        self.start.elapsed().as_millis()
    }

    /// One event-loop turn: forward pending updates, then `before_view`.
    async fn turn(&mut self) -> Result<()> {
        let tick = tokio::time::sleep(Duration::from_millis(20));
        tokio::select! {
            biased;
            Some(mut batch) = self.rx.recv() => {
                for event in batch.drain(..) {
                    if let GXEvent::Updated(id, v) = event {
                        if id == self.sel_id {
                            self.sel = string_of(&v);
                            self.sel_updates += 1;
                        } else if id != self.root_id {
                            let t = self.ms();
                            self.forwarded.push(t);
                            let (w, rt) = (&mut self.widget, &self.rt);
                            tokio::task::block_in_place(|| w.handle_update(rt, id, &v))?;
                        }
                    }
                }
            }
            _ = tick => {}
        }
        self.widget.before_view();
        Ok(())
    }

    async fn pump_until_ms(&mut self, t: u128) -> Result<()> {
        while self.ms() < t {
            self.turn().await?;
        }
        Ok(())
    }

    /// The path `on_select` reports for a click on (row 0, "c0").
    async fn first_row(&mut self) -> Result<String> {
        let n = self.sel_updates;
        let mut shell = MessageShell::new(iced_core::Point::ORIGIN);
        self.widget.on_message(&Message::CellClick(0, arcstr::literal!("c0")), &mut shell);
        for m in shell.out.drain(..) {
            if let Message::Call(id, args) = m {
                self.gx.call(id, args)?;
            }
        }
        let deadline = self.ms() + 1000;
        while self.sel_updates == n && self.ms() < deadline {
            self.turn().await?;
        }
        if self.sel_updates == n { Ok("(no on_select)".into()) } else { Ok(self.sel.clone()) }
    }

    /// Click-poll until `want` is drawn first or the clock passes `until`.
    async fn wait_first_row(&mut self, want: &str, until: u128) -> Result<(bool, String)> {
        let mut last = String::new();
        while self.ms() < until {
            last = self.first_row().await?;
            if last == want {
                return Ok((true, last));
            }
            self.pump_until_ms(self.ms() + 100).await?;
        }
        Ok((false, last))
    }
}

async fn run_case(name: &str, prefix: &str, wrap: &str) -> Result<()> {
    let mut h = H::new(&program(prefix, wrap)).await?;
    observe(name, prefix, &mut h).await
}

async fn observe(name: &str, prefix: &str, h: &mut H) -> Result<()> {
    let r1 = format!("/local/{prefix}/r1/c0");
    let r2 = format!("/local/{prefix}/r2/c0");
    let (sorted, before) = h.wait_first_row(&r2, 2900).await?;
    println!("{name}: {} ms: first row before the bump: {before} (sorted: {sorted})", h.ms());
    h.pump_until_ms(3000).await?;
    let fwd_before_bump = h.forwarded.len();
    let (resorted, after) = h.wait_first_row(&r1, 7000).await?;
    let fwd_after: Vec<u128> = h.forwarded[fwd_before_bump..].to_vec();
    println!(
        "{name}: {} ms: first row after r1 -> 9 at 3000 ms: {after} (re-sorted: {resorted}); \
         updates forwarded to handle_update after the bump: {fwd_after:?}",
        h.ms()
    );
    if !resorted {
        bail!("{name}: EXPECTED {r1} drawn first after its sort value became 9; OBSERVED {after}");
    }
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn a_root() -> Result<()> {
    run_case("a_root", "wa11a", "dt").await
}

#[tokio::test(flavor = "multi_thread")]
async fn b_in_column() -> Result<()> {
    run_case("b_in_column", "wa11b", "column(&[dt])").await
}

#[tokio::test(flavor = "multi_thread")]
async fn c_in_table_cell() -> Result<()> {
    run_case("c_in_table_cell", "wa11c", "table(&[table_column(&text(&\"t\"))], &[[dt]])")
        .await
}

#[tokio::test(flavor = "multi_thread")]
async fn d_in_table_header() -> Result<()> {
    run_case("d_in_table_header", "wa11d", "table(&[table_column(&dt)], &[[text(&\"x\")]])")
        .await
}

/// Case c, then one `handle_update` for an id no widget owns (what any
/// widget ref update does): the cell re-sorts at once, so the values had
/// arrived and only `before_view` never reached the cell.
#[tokio::test(flavor = "multi_thread")]
async fn e_in_table_cell_then_unrelated_update() -> Result<()> {
    let (name, prefix) = ("e_in_table_cell_then_unrelated_update", "wa11e");
    let mut h =
        H::new(&program(prefix, "table(&[table_column(&text(&\"t\"))], &[[dt]])")).await?;
    let stale = observe(name, prefix, &mut h).await;
    let (w, rt) = (&mut h.widget, &h.rt);
    tokio::task::block_in_place(|| w.handle_update(rt, ExprId::new(), &Value::Null))?;
    let first = h.first_row().await?;
    println!("{name}: {} ms: first row after one unrelated handle_update: {first}", h.ms());
    assert!(stale.is_err(), "{name}: the cell re-sorted without help");
    assert_eq!(first, format!("/local/{prefix}/r1/c0"));
    Ok(())
}
