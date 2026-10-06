//! gui-widgets-b-08: `TableW` forwards `on_message` to its cells by hand
//! but not `before_view` (src/widgets/table.rs:174; it keeps the default
//! empty `children_mut`, which the default `before_view` at
//! src/widgets/mod.rs:225 walks), and `TooltipW` reports only `child`
//! (src/widgets/tooltip.rs:53), so its `tip` gets neither. A data_table's
//! live re-sort is deferred work done in `before_view`
//! (src/widgets/data_table/mod.rs:292-298): inside a `table` cell it is
//! never done at a frame, only when some unrelated graphix update happens
//! to reach the widget (`handle_update`, mod.rs:429).
//!
//! Command (from the repository root), with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_widgets_b_08.rs:
//!   env -u WAYLAND_DISPLAY -u DISPLAY timeout -s KILL 2400 cargo test \
//!     -p graphix-package-gui --test review_gui_widgets_b_08 -- --nocapture
//!
//! The program publishes /local/<p>/r<i>/cpu (r0 = 30, r1 = 10, r2 = 20)
//! and shows `data_table(#sort_by: cpu ascending)` over the three rows,
//! wrapped as each case says. The harness drives the widget as the event
//! loop does: every runtime update except the root's goes to
//! `handle_update`, and each frame is `before_view()` then `view()`
//! (event_loop.rs:317-318). The displayed order is read by sending
//! `Message::CellClick(row, ..)` for rows 0..3 through `on_message`: the
//! table's `#on_select` writes the clicked row's path to `sel`.
//!   phase A: the subscriptions answered, one frame.
//!   phase B: r1's published value set to 100 (nothing the GUI holds
//!            changes), one frame.
//!   phase C: the header/label text `poke` changes (an unrelated graphix
//!            update reaching the widget), one frame.
//! Expected in every case: A = [r1, r2, r0], B = C = [r2, r0, r1].
//! Observed at c722befe (dev profile): a_column_control passes,
//! b_table_cell and c_tooltip_tip fail ("-" = the click fired no
//! on_select):
//!   RESULT column: phase A (subscriptions answered, 0 GUI updates, 1 frame): ["r1", "r2", "r0"] (0 GUI updates while probing)
//!   RESULT column: phase B (r1's cpu published as 100, 0 GUI updates, 1 frame): ["r2", "r0", "r1"] (0 GUI updates while probing)
//!   RESULT column: phase C (unrelated text changed, 1 GUI updates, 1 frame): ["r2", "r0", "r1"] (0 GUI updates while probing)
//!   RESULT table: phase A (subscriptions answered, 0 GUI updates, 1 frame): ["r0", "r1", "r2"] (0 GUI updates while probing)
//!   RESULT table: phase B (r1's cpu published as 100, 0 GUI updates, 1 frame): ["r0", "r1", "r2"] (0 GUI updates while probing)
//!   RESULT table: phase C (unrelated text changed, 1 GUI updates, 1 frame): ["r2", "r0", "r1"] (0 GUI updates while probing)
//!   RESULT tooltip: phase A (subscriptions answered, 0 GUI updates, 1 frame): ["-", "-", "-"] (0 GUI updates while probing)
//!   RESULT tooltip: phase B (r1's cpu published as 100, 0 GUI updates, 1 frame): ["-", "-", "-"] (0 GUI updates while probing)
//!   RESULT tooltip: phase C (unrelated text changed, 1 GUI updates, 1 frame): ["-", "-", "-"] (0 GUI updates while probing)
//!   panicked: table: phase A order  left: ["r0", "r1", "r2"]  right: ["r1", "r2", "r0"]
//!   panicked: tooltip: phase A order  left: ["-", "-", "-"]  right: ["r1", "r2", "r0"]
//! The `column` control is sorted at every frame. In the `table` cell the
//! live sort is never applied at a frame: the rows stay in table order
//! through A and B and sort only in C, when the unrelated header text
//! update reaches the data table through `handle_update`. The tooltip's
//! tip never receives the clicks (nor, by the same `children_mut`, any
//! `before_view`).

use ahash::AHashMap;
use anyhow::{Context, Result, bail};
use arcstr::ArcStr;
use graphix_compiler::{
    BindId,
    env::Env,
    expr::{ExprId, VfsEntry, VfsResolver},
};
use graphix_package_core::testing::{self, PackageRef, TestCtx};
use graphix_package_gui::widgets::{self, GuiW, Message, MessageShell};
use graphix_rt::{CompRes, GXEvent, GXHandle, NoExt, Ref};
use netidx::{path::Path, publisher::Value};
use poolshark::global::GPooled;
use std::time::Duration;
use tokio::{sync::mpsc, time::Instant};

const REGISTER: &[PackageRef] = &[
    &graphix_package_core::P,
    &graphix_package_array::P,
    &graphix_package_map::P,
    &graphix_package_str::P,
    &graphix_package_sys::P,
    &graphix_package_gui::P,
];

fn program(p: &str, wrap: &str) -> String {
    format!(
        r#"
use gui::*;
use gui::text::{{self, *}};
use gui::table::{{self, *}};
use gui::column::{{self, *}};
use gui::tooltip::{{self, *}};
use gui::data_table::{{self, *}};
let v0 = f64:30.0;
let v1 = f64:10.0;
let v2 = f64:20.0;
sys::net::publish("/local/{p}/r0/cpu", v0);
sys::net::publish("/local/{p}/r1/cpu", v1);
sys::net::publish("/local/{p}/r2/cpu", v2);
let tbl = {{
    rows: ["/local/{p}/r0", "/local/{p}/r1", "/local/{p}/r2"],
    columns: ["cpu"]
}};
let sel = "none";
let poke = "Live";
let dt = data_table(
    #sort_by: &[{{ column: "cpu", direction: `Ascending }}],
    #on_select: |#path: string| sel <- path,
    #table: &tbl
);
let result = {wrap}
"#
    )
}

fn find_bind_id(env: &Env, name: &str) -> Result<BindId> {
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

struct H {
    _ctx: TestCtx,
    gx: GXHandle<NoExt>,
    compiled: CompRes<NoExt>,
    root_id: ExprId,
    rx: mpsc::Receiver<GPooled<Vec<GXEvent>>>,
    widget: GuiW<NoExt>,
    sel: Ref<NoExt>,
    clicks: usize,
}

impl H {
    async fn new(code: &str) -> Result<Self> {
        let (tx, mut rx) = mpsc::channel(100);
        let tbl = AHashMap::from_iter([(
            Path::from("/test.gx"),
            VfsEntry::from(ArcStr::from(code)),
        )]);
        let ctx =
            testing::init_with_resolvers(tx, REGISTER, vec![VfsResolver::new(tbl)]).await?;
        let gx = ctx.rt.clone();
        let compiled: CompRes<NoExt> = gx
            .compile(arcstr::literal!("{ mod test; test::result }"))
            .await
            .context("compile")?;
        let root_id = compiled.exprs[0].id;
        let root = tokio::time::timeout(Duration::from_secs(30), async {
            loop {
                let mut batch = rx.recv().await.context("channel closed")?;
                if let Some(v) = batch.drain(..).find_map(|e| match e {
                    GXEvent::Updated(id, v) if id == root_id => Some(v),
                    _ => None,
                }) {
                    return Ok::<Value, anyhow::Error>(v);
                }
            }
        })
        .await
        .context("timeout waiting for the root value")??;
        let widget = widgets::compile(gx.clone(), root).await.context("compile widget")?;
        let sel_bid = find_bind_id(&compiled.env, "test::sel")?;
        let sel = gx.compile_ref(sel_bid).await?;
        Ok(Self { _ctx: ctx, gx, compiled, root_id, rx, widget, sel, clicks: 0 })
    }

    /// One batch of runtime events, delivered as the shell and the event
    /// loop deliver them: the root's update goes to window reconciliation
    /// (not here), everything else to the content's `handle_update`. The
    /// harness's own `sel` ref is not part of the GUI. Returns how many
    /// updates reached the widget and the last `sel` value seen.
    fn deliver(&mut self, mut batch: GPooled<Vec<GXEvent>>) -> Result<(usize, Option<Value>)> {
        let rt = tokio::runtime::Handle::current();
        let mut forwarded = 0;
        let mut sel = None;
        for e in batch.drain(..) {
            if let GXEvent::Updated(id, v) = e {
                if id == self.sel.id {
                    sel = Some(v);
                } else if id != self.root_id {
                    forwarded += 1;
                    self.widget.handle_update(&rt, id, &v)?;
                }
            }
        }
        Ok((forwarded, sel))
    }

    /// Deliver everything that arrives for `d`.
    async fn settle(&mut self, d: Duration) -> Result<usize> {
        let deadline = Instant::now() + d;
        let mut forwarded = 0;
        loop {
            match tokio::time::timeout_at(deadline, self.rx.recv()).await {
                Ok(Some(batch)) => forwarded += self.deliver(batch)?.0,
                Ok(None) => bail!("runtime channel closed"),
                Err(_) => return Ok(forwarded),
            }
        }
    }

    /// A frame as `about_to_wait` builds it.
    fn frame(&mut self) {
        self.widget.before_view();
        let _ = self.widget.view();
    }

    /// The row basename displayed at each of rows 0..3, read by clicking
    /// each row; "-" when the click fired no on_select.
    async fn order(&mut self) -> Result<(Vec<String>, usize)> {
        let mut out = vec![];
        let mut forwarded = 0;
        for row in 0..3usize {
            self.clicks += 1;
            let tag: ArcStr = format!("probe{}", self.clicks).into();
            let mut shell = MessageShell::new(iced_core::Point::ORIGIN);
            self.widget.on_message(&Message::CellClick(row, tag.clone()), &mut shell);
            let suffix = format!("/{tag}");
            let deadline = Instant::now() + Duration::from_secs(2);
            let mut got = None;
            while got.is_none() {
                match tokio::time::timeout_at(deadline, self.rx.recv()).await {
                    Ok(Some(batch)) => {
                        let (f, sel) = self.deliver(batch)?;
                        forwarded += f;
                        if let Some(Value::String(s)) = sel {
                            if s.ends_with(&suffix) {
                                got = Some(s);
                            }
                        }
                    }
                    Ok(None) => bail!("runtime channel closed"),
                    Err(_) => break,
                }
            }
            out.push(match got {
                None => "-".to_string(),
                Some(s) => {
                    let parts: Vec<&str> = s.split('/').collect();
                    parts[parts.len() - 2].to_string()
                }
            });
        }
        Ok((out, forwarded))
    }
}

async fn scenario(case: &str, wrap: &str) -> Result<()> {
    let p = format!("review_b08_{case}");
    let mut h = H::new(&program(&p, wrap)).await?;
    let f0 = h.settle(Duration::from_millis(1500)).await?;
    h.frame();
    let (a, fa) = h.order().await?;
    println!(
        "RESULT {case}: phase A (subscriptions answered, {f0} GUI updates, 1 frame): {a:?} ({fa} GUI updates while probing)"
    );
    let v1 = find_bind_id(&h.compiled.env, "test::v1")?;
    h.gx.set(v1, Value::F64(100.0))?;
    let fb0 = h.settle(Duration::from_millis(1500)).await?;
    h.frame();
    let (b, fb) = h.order().await?;
    println!(
        "RESULT {case}: phase B (r1's cpu published as 100, {fb0} GUI updates, 1 frame): {b:?} ({fb} GUI updates while probing)"
    );
    let poke = find_bind_id(&h.compiled.env, "test::poke")?;
    h.gx.set(poke, Value::String(arcstr::literal!("Live!")))?;
    let fc0 = h.settle(Duration::from_millis(1500)).await?;
    h.frame();
    let (c, fc) = h.order().await?;
    println!(
        "RESULT {case}: phase C (unrelated text changed, {fc0} GUI updates, 1 frame): {c:?} ({fc} GUI updates while probing)"
    );
    assert_eq!(a, ["r1", "r2", "r0"], "{case}: phase A order");
    assert_eq!(b, ["r2", "r0", "r1"], "{case}: phase B order after r1's cpu became 100");
    assert_eq!(c, ["r2", "r0", "r1"], "{case}: phase C order");
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn a_column_control() -> Result<()> {
    scenario("column", "column(&[text(&poke), dt])").await
}

#[tokio::test(flavor = "current_thread")]
async fn b_table_cell() -> Result<()> {
    scenario("table", "table(&[table_column(&text(&poke))], &[[dt]])").await
}

#[tokio::test(flavor = "current_thread")]
async fn c_tooltip_tip() -> Result<()> {
    scenario("tooltip", "tooltip(#tip: &dt, &text(&poke))").await
}
