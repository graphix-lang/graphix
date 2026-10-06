//! gui-datatable-04: clearing `sort_by` keeps the last sorted row order,
//! and each resort starts from the previous order, so the rows a
//! `(table, sort_by)` pair displays depend on the sort history.
//!
//! Command (from the repository root), with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_datatable_04.rs:
//!   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_gui_datatable_04 -- --nocapture
//!
//! The display order is read through the widget's public API: a
//! `Message::CellClick(i, "name")` fires `#on_activate` with the path of
//! display row `i`, and the program appends it to `clicked`.
//!
//! `header_cycle`: the book's data_table_filter_sort.gx header cycle
//! (absent -> Ascending -> Descending -> absent) driven by calling the
//! program's `#on_header_click` lambda with "p", which is what a header
//! click's `Message::Call` does. Rows r0, r1, r2 have p = 3, 1, 2.
//! `tie_order_depends_on_history`: two widgets over one table, both
//! finally showing `sort_by = [{k, Ascending}]` (k = b, a, a); the first
//! got there from `[{m, Ascending}]` (m = 1, 3, 2).
//!
//! Expected (data_table.gxi:99-103 and the book: "Empty list (the
//! default) preserves the Table's row order"):
//!   header_cycle: after click 3, sort_by = [] and rows [r0, r1, r2].
//!   tie_order_depends_on_history: both widgets show the same rows.
//! Observed at c722befe (dev profile), both tests fail:
//!   RESULT header_cycle: initial sort_by=[] rows=["r0", "r1", "r2"]
//!   RESULT header_cycle: after header click 1 on p: sort_by=[[["column", "p"], ["direction", "Ascending"]]] rows=["r1", "r2", "r0"]
//!   RESULT header_cycle: after header click 2 on p: sort_by=[[["column", "p"], ["direction", "Descending"]]] rows=["r0", "r2", "r1"]
//!   RESULT header_cycle: after header click 3 on p: sort_by=[] rows=["r0", "r2", "r1"]
//!   RESULT ties A: sort_by=[[["column", "m"], ["direction", "Ascending"]]] rows=["r0", "r2", "r1"]
//!   RESULT ties A: sort_by=[[["column", "k"], ["direction", "Ascending"]]] rows=["r2", "r1", "r0"]
//!   RESULT ties B (fresh): sort_by=[[["column", "k"], ["direction", "Ascending"]]] rows=["r1", "r2", "r0"]
//! With sort_by back to [] the rows keep the descending order; the same
//! table and sort_by show [r2, r1, r0] or [r1, r2, r0] by history.

use ahash::AHashMap;
use anyhow::{Context, Result, bail};
use graphix_compiler::{
    BindId,
    env::Env,
    expr::{ExprId, VfsEntry, VfsResolver},
};
use graphix_package_core::testing::{self, PackageRef, TestCtx};
use graphix_package_gui::widgets::{self, GuiW, Message, MessageShell};
use graphix_rt::{Callable, CompRes, GXEvent, NoExt, Ref};
use netidx::{path::Path, protocol::valarray::ValArray, publisher::Value};
use poolshark::global::GPooled;
use std::time::Duration;
use tokio::sync::mpsc;

const REGISTER: &[PackageRef] = &[
    &graphix_package_core::P,
    &graphix_package_array::P,
    &graphix_package_map::P,
    &graphix_package_str::P,
    &graphix_package_sys::P,
    &graphix_package_gui::P,
];

const CYCLE: &str = r#"
use gui::data_table::{SortBy, data_table};

let tbl = {
  rows: ["r0", "r1", "r2"],
  columns: [{
    name: "p",
    typ: `Text({ on_edit: null }),
    display_name: null,
    source: &{"r0" => "3", "r1" => "1", "r2" => "2"},
    on_resize: &null,
    width: &null
  }]
};

let sort_by: Array<SortBy> = [];

let cycle_sort = |#column: string|
    sort_by <- column ~ select array::find(sort_by, |s| s.column == column) {
        null => array::push(sort_by, { column, direction: `Ascending }),
        { direction: `Ascending, .. } => array::map(sort_by, |s| select s.column == column {
            true => { s with direction: `Descending },
            false => s
        }),
        { direction: `Descending, .. } => array::filter(sort_by, |s| s.column != column)
    };

let header_click = |#column: string| cycle_sort(#column: column);

let clicked: Array<string> = [];

let result = data_table(
  #sort_by: &sort_by,
  #on_header_click: header_click,
  #on_activate: |#path: string| clicked <- path ~ array::push(clicked, path),
  #table: &tbl
)
"#;

fn ties(step: i64) -> String {
    format!(
        r#"
use gui::data_table::{{SortBy, data_table}};

let tbl = {{
  rows: ["r0", "r1", "r2"],
  columns: [
    {{ name: "k", typ: `Text({{ on_edit: null }}), display_name: null,
      source: &{{"r0" => "b", "r1" => "a", "r2" => "a"}}, on_resize: &null, width: &null }},
    {{ name: "m", typ: `Text({{ on_edit: null }}), display_name: null,
      source: &{{"r0" => "1", "r1" => "3", "r2" => "2"}}, on_resize: &null, width: &null }}
  ]
}};

let step = {step};

let sort_by: Array<SortBy> = select step {{
  0 => [{{ column: "m", direction: `Ascending }}],
  _ => [{{ column: "k", direction: `Ascending }}]
}};

let clicked: Array<string> = [];

let result = data_table(
  #sort_by: &sort_by,
  #on_activate: |#path: string| clicked <- path ~ array::push(clicked, path),
  #table: &tbl
)
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

struct Session {
    ctx: TestCtx,
    compiled: CompRes<NoExt>,
    rx: mpsc::Receiver<GPooled<Vec<GXEvent>>>,
    widget: GuiW<NoExt>,
    watched: AHashMap<String, (Ref<NoExt>, Value)>,
    by_id: AHashMap<ExprId, String>,
    refs: Vec<Ref<NoExt>>,
    callables: Vec<Callable<NoExt>>,
}

impl Session {
    async fn new(code: &str) -> Result<Self> {
        let (tx, mut rx) = mpsc::channel(100);
        let vfs = AHashMap::from_iter([(
            netidx_core::path::Path::from("/test.gx"),
            VfsEntry::from(arcstr::ArcStr::from(code)),
        )]);
        let ctx =
            testing::init_with_resolvers(tx, REGISTER, vec![VfsResolver::new(vfs)])
                .await?;
        let compiled = ctx
            .rt
            .compile(arcstr::literal!("{ mod test; test::result }"))
            .await
            .context("compile graphix code")?;
        let id = compiled.exprs[0].id;
        let root = loop {
            let mut batch = tokio::time::timeout(Duration::from_secs(5), rx.recv())
                .await
                .context("timeout waiting for the widget value")?
                .context("event channel closed")?;
            let found = batch.drain(..).find_map(|e| match e {
                GXEvent::Updated(i, v) if i == id => Some(v),
                _ => None,
            });
            if let Some(v) = found {
                break v;
            }
        };
        let widget =
            widgets::compile(ctx.rt.clone(), root).await.context("compile widget")?;
        let mut s = Self {
            ctx,
            compiled,
            rx,
            widget,
            watched: AHashMap::default(),
            by_id: AHashMap::default(),
            refs: vec![],
            callables: vec![],
        };
        s.watch("clicked").await?;
        s.watch("sort_by").await?;
        s.drain().await?;
        Ok(s)
    }

    async fn watch(&mut self, var: &str) -> Result<()> {
        let bid = find_bind_id(&self.compiled.env, &format!("test::{var}"))?;
        let r = self.ctx.rt.compile_ref(bid).await?;
        let v = r.last.clone().unwrap_or(Value::Null);
        self.by_id.insert(r.id, var.to_string());
        self.watched.insert(var.to_string(), (r, v));
        Ok(())
    }

    fn value(&self, var: &str) -> Value {
        self.watched[var].1.clone()
    }

    /// Deliver every pending update to the widget as the event loop
    /// does, then flush deferred state as before a render.
    async fn drain(&mut self) -> Result<()> {
        let rt = tokio::runtime::Handle::current();
        while let Ok(Some(mut batch)) =
            tokio::time::timeout(Duration::from_millis(200), self.rx.recv()).await
        {
            for e in batch.drain(..) {
                if let GXEvent::Updated(i, v) = e {
                    if let Some(var) = self.by_id.get(&i) {
                        if let Some(slot) = self.watched.get_mut(var) {
                            slot.1 = v.clone();
                        }
                    }
                    let widget = &mut self.widget;
                    tokio::task::block_in_place(|| widget.handle_update(&rt, i, &v))?;
                }
            }
        }
        self.widget.before_view();
        Ok(())
    }

    /// Display order: click each row's name cell; `on_activate` reports
    /// the path of that display row.
    async fn order(&mut self, n: usize) -> Result<Vec<String>> {
        for i in 0..n {
            let mut shell = MessageShell::new(iced_core::Point::ORIGIN);
            self.widget
                .on_message(&Message::CellClick(i, arcstr::literal!("name")), &mut shell);
            self.drain().await?;
        }
        let all = self.value("clicked").cast_to::<Vec<Value>>()?;
        if all.len() < n {
            bail!("only {} clicks recorded", all.len());
        }
        Ok(all[all.len() - n..]
            .iter()
            .map(|v| match v {
                Value::String(s) => s.to_string(),
                v => format!("{v}"),
            })
            .collect())
    }

    /// What a header click does: `Message::Call(on_header_click, [col])`.
    async fn header_click(&mut self, col: &str) -> Result<()> {
        let bid = find_bind_id(&self.compiled.env, "test::header_click")?;
        let r = self.ctx.rt.compile_ref(bid).await?;
        let f = r.last.clone().context("header_click has no value")?;
        let cb = self.ctx.rt.compile_callable(f).await?;
        self.ctx.rt.call(cb.id(), ValArray::from_iter([Value::String(col.into())]))?;
        self.refs.push(r);
        self.callables.push(cb);
        self.drain().await
    }

    async fn set(&mut self, var: &str, v: Value) -> Result<()> {
        let bid = find_bind_id(&self.compiled.env, &format!("test::{var}"))?;
        let mut r = self.ctx.rt.compile_ref(bid).await?;
        r.set(v)?;
        self.refs.push(r);
        self.drain().await
    }
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn header_cycle() -> Result<()> {
    let mut s = Session::new(CYCLE).await?;
    let initial = s.order(3).await?;
    eprintln!("RESULT header_cycle: initial sort_by={} rows={initial:?}", s.value("sort_by"));
    let mut last = initial.clone();
    for click in 1..=3 {
        s.header_click("p").await?;
        last = s.order(3).await?;
        eprintln!(
            "RESULT header_cycle: after header click {click} on p: sort_by={} rows={last:?}",
            s.value("sort_by")
        );
    }
    assert_eq!(initial, vec!["r0", "r1", "r2"]);
    assert_eq!(
        last,
        vec!["r0", "r1", "r2"],
        "sort_by is [] again: the Table's row order should be back"
    );
    Ok(())
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn tie_order_depends_on_history() -> Result<()> {
    let mut a = Session::new(&ties(0)).await?;
    let a0 = a.order(3).await?;
    eprintln!("RESULT ties A: sort_by={} rows={a0:?}", a.value("sort_by"));
    a.set("step", Value::I64(1)).await?;
    let a1 = a.order(3).await?;
    eprintln!("RESULT ties A: sort_by={} rows={a1:?}", a.value("sort_by"));
    let mut b = Session::new(&ties(1)).await?;
    let b1 = b.order(3).await?;
    eprintln!("RESULT ties B (fresh): sort_by={} rows={b1:?}", b.value("sort_by"));
    assert_eq!(a1, b1, "one table and one sort_by should show one row order");
    Ok(())
}
