//! gui-datatable-11: a data column named "name" is treated as the
//! row-name column on click.
//!
//! Command (from the repository root), with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_datatable_11.rs:
//!   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_gui_datatable_11 -- --nocapture
//!
//! A table with columns ["name", "pid"] and both #on_select and
//! #on_activate set is driven headlessly as the event loop does
//! (UserInterface::build/update over a kept cache, then
//! `GuiWidget::on_message` for what the click published). Row 0 is
//! scanned to find which column key each x position's click sends,
//! then the row-0 cell of each column is clicked on a fresh widget.
//!
//! Expected (book/src/ui/gui/data_table.md: on_select "Fired whenever a
//! cell is clicked ... `row_path/col_name` for data cells"; on_activate
//! "Fired when the user clicks a row-name cell"): a click on the "name"
//! DATA cell selects "r0/name" and activates nothing.
//! Observed at c722befe (test FAILS):
//!   RESULT show_row_name=true: row 0, x 2..=78, y 35 publishes CellClick(0, "\0__rowname__")
//!   RESULT show_row_name=true: row 0, x 82..=158, y 35 publishes CellClick(0, "name")
//!   RESULT show_row_name=true: row 0, x 162..=238, y 35 publishes CellClick(0, "pid")
//!   RESULT show_row_name=true: click at (40, 35) published [(0, "\"\\0__rowname__\"")] -> selected="" activated="r0"
//!   RESULT show_row_name=true: click at (120, 35) published [(0, "\"name\"")] -> selected="" activated="r0"
//!   RESULT show_row_name=true: click at (200, 35) published [(0, "\"pid\"")] -> selected="r0/pid" activated=""
//!   RESULT show_row_name=false: row 0, x 2..=78, y 35 publishes CellClick(0, "name")
//!   RESULT show_row_name=false: row 0, x 82..=158, y 35 publishes CellClick(0, "pid")
//!   RESULT show_row_name=false: click at (40, 35) published [(0, "\"name\"")] -> selected="" activated="r0"
//!   RESULT show_row_name=false: click at (120, 35) published [(0, "\"pid\"")] -> selected="r0/pid" activated=""
//! The "name" data cell sends its real key, `handle_cell_click`
//! (events.rs:207) takes "name" for the row-name sentinel, fires
//! on_activate and returns before on_select; with show_row_name=false
//! there is no row-name column at all and the click still activates.
//! (The "could not send batch" log lines are dropped runtimes of the
//! earlier widgets in the run.)

use ahash::AHashMap;
use anyhow::{Context, Result, bail};
use arcstr::{ArcStr, literal};
use graphix_compiler::{
    BindId,
    env::Env,
    expr::{ExprId, VfsEntry, VfsResolver},
};
use graphix_package_core::testing::{self, PackageRef, TestCtx};
use graphix_package_gui::widgets::{self, GuiW, Message, MessageShell};
use graphix_rt::{CompRes, GXEvent, NoExt, Ref};
use iced_core::{Event, Font, Pixels, Point, Size, clipboard, mouse};
use iced_runtime::user_interface::{self, UserInterface};
use iced_wgpu::{graphics::Shell, wgpu};
use netidx::{path::Path, publisher::Value};
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

const VIEWPORT: Size = Size::new(500.0, 200.0);

fn program(show_row_name: bool) -> String {
    format!(
        r#"
use gui::data_table::data_table;
let tbl = {{ rows: ["r0", "r1"], columns: ["name", "pid"] }};
let selected = "";
let activated = "";
let result = data_table(
  #show_row_name: &{show_row_name},
  #on_select: |#path: string| selected <- path,
  #on_activate: |#path: string| activated <- path,
  #table: &tbl
)
"#
    )
}

async fn renderer() -> widgets::Renderer {
    let instance = wgpu::Instance::new(&wgpu::InstanceDescriptor {
        backends: wgpu::Backends::from_env().unwrap_or(wgpu::Backends::PRIMARY),
        ..Default::default()
    });
    let adapter = match instance
        .request_adapter(&wgpu::RequestAdapterOptions {
            compatible_surface: None,
            force_fallback_adapter: false,
            ..Default::default()
        })
        .await
    {
        Ok(a) => a,
        Err(_) => instance
            .request_adapter(&wgpu::RequestAdapterOptions {
                compatible_surface: None,
                force_fallback_adapter: true,
                ..Default::default()
            })
            .await
            .expect("no GPU adapter"),
    };
    let (device, queue) = adapter
        .request_device(&wgpu::DeviceDescriptor::default())
        .await
        .expect("GPU device");
    let engine = iced_wgpu::Engine::new(
        &adapter,
        device,
        queue,
        wgpu::TextureFormat::Rgba8UnormSrgb,
        None,
        Shell::headless(),
    );
    iced_wgpu::Renderer::new(engine, Font::DEFAULT, Pixels(16.0))
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
    ctx: TestCtx,
    compiled: CompRes<NoExt>,
    rx: mpsc::Receiver<GPooled<Vec<GXEvent>>>,
    widget: GuiW<NoExt>,
    renderer: widgets::Renderer,
    cache: user_interface::Cache,
    cursor: Point,
    watched: AHashMap<ExprId, (String, Value)>,
    refs: Vec<Ref<NoExt>>,
}

impl H {
    async fn new(code: &str) -> Result<Self> {
        let (tx, mut rx) = mpsc::channel(100);
        let vfs = AHashMap::from_iter([(
            netidx_core::path::Path::from("/test.gx"),
            VfsEntry::from(ArcStr::from(code)),
        )]);
        let ctx =
            testing::init_with_resolvers(tx, REGISTER, vec![VfsResolver::new(vfs)])
                .await?;
        let compiled = ctx
            .rt
            .compile(literal!("{ mod test; test::result }"))
            .await
            .context("compile graphix code")?;
        let id = compiled.exprs[0].id;
        let root = loop {
            let mut batch = tokio::time::timeout(Duration::from_secs(10), rx.recv())
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
        let mut h = Self {
            ctx,
            compiled,
            rx,
            widget,
            renderer: renderer().await,
            cache: user_interface::Cache::default(),
            cursor: Point::ORIGIN,
            watched: AHashMap::default(),
            refs: vec![],
        };
        h.watch("selected").await?;
        h.watch("activated").await?;
        h.drain().await?;
        let _ = h.events(&[]);
        Ok(h)
    }

    async fn watch(&mut self, var: &str) -> Result<()> {
        let bid = find_bind_id(&self.compiled.env, &format!("test::{var}"))?;
        let r = self.ctx.rt.compile_ref(bid).await?;
        let v = r.last.clone().unwrap_or(Value::Null);
        self.watched.insert(r.id, (var.to_string(), v));
        self.refs.push(r);
        Ok(())
    }

    fn value(&self, var: &str) -> Value {
        self.watched
            .values()
            .find(|(n, _)| n == var)
            .map(|(_, v)| v.clone())
            .unwrap_or(Value::Null)
    }

    async fn drain(&mut self) -> Result<()> {
        let rt = tokio::runtime::Handle::current();
        while let Ok(Some(mut batch)) =
            tokio::time::timeout(Duration::from_millis(200), self.rx.recv()).await
        {
            for e in batch.drain(..) {
                if let GXEvent::Updated(i, v) = e {
                    if let Some(slot) = self.watched.get_mut(&i) {
                        slot.1 = v.clone();
                    }
                    let widget = &mut self.widget;
                    tokio::task::block_in_place(|| widget.handle_update(&rt, i, &v))?;
                }
            }
        }
        self.widget.before_view();
        Ok(())
    }

    fn events(&mut self, events: &[Event]) -> Vec<Message> {
        let element = self.widget.view();
        let cache = std::mem::take(&mut self.cache);
        let mut ui = UserInterface::build(element, VIEWPORT, cache, &mut self.renderer);
        let mut msgs = Vec::new();
        let _ = ui.update(
            events,
            mouse::Cursor::Available(self.cursor),
            &mut self.renderer,
            &mut clipboard::Null,
            &mut msgs,
        );
        self.cache = ui.into_cache();
        msgs
    }

    /// What a left click at `pos` publishes, one UI frame per event.
    fn click(&mut self, pos: Point) -> Vec<Message> {
        self.cursor = pos;
        let mut all = self.events(&[Event::Mouse(mouse::Event::CursorMoved { position: pos })]);
        all.extend(self.events(&[Event::Mouse(mouse::Event::ButtonPressed(
            mouse::Button::Left,
        ))]));
        all.extend(self.events(&[Event::Mouse(mouse::Event::ButtonReleased(
            mouse::Button::Left,
        ))]));
        all
    }

    /// Deliver published messages as `GuiHandler` does, then drain.
    async fn dispatch(&mut self, msgs: Vec<Message>) -> Result<()> {
        let mut pending: std::collections::VecDeque<Message> = msgs.into();
        while let Some(msg) = pending.pop_front() {
            match msg {
                Message::Nop
                | Message::ColumnResizeStart(_)
                | Message::ColumnResizeMove(_)
                | Message::ColumnResizeEnd => {}
                Message::Call(id, args) => self.ctx.rt.call(id, args)?,
                other => {
                    let mut shell = MessageShell::new(Point::ORIGIN);
                    self.widget.on_message(&other, &mut shell);
                    pending.extend(shell.out.drain(..));
                }
            }
        }
        self.drain().await
    }
}

fn cell_clicks(msgs: &[Message]) -> Vec<(usize, String)> {
    msgs.iter()
        .filter_map(|m| match m {
            Message::CellClick(r, c) => Some((*r, format!("{:?}", c.as_str()))),
            _ => None,
        })
        .collect()
}

/// x of the first click in row 0 that publishes `CellClick(0, key)`, for
/// each distinct key, scanning left to right.
async fn column_positions(show_row_name: bool) -> Result<(f32, Vec<(String, f32, f32)>)> {
    let mut h = H::new(&program(show_row_name)).await?;
    let mut spans: Vec<(String, f32, f32)> = vec![];
    let mut row_y = None;
    for y in [35.0_f32, 39.0, 42.0, 45.0] {
        let msgs = h.click(Point::new(40.0, y));
        if cell_clicks(&msgs).iter().any(|(r, _)| *r == 0) {
            row_y = Some(y);
            break;
        }
    }
    let y = row_y.context("no CellClick(0, _) found in row 0")?;
    let mut x = 2.0_f32;
    while x < VIEWPORT.width {
        let msgs = h.click(Point::new(x, y));
        for (r, key) in cell_clicks(&msgs) {
            if r != 0 {
                continue;
            }
            match spans.last_mut() {
                Some((k, _, hi)) if *k == key => *hi = x,
                _ => spans.push((key, x, x)),
            }
        }
        x += 4.0;
    }
    Ok((y, spans))
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn name_data_column_click() -> Result<()> {
    let mut failures = vec![];
    for show_row_name in [true, false] {
        let (y, spans) = column_positions(show_row_name).await?;
        for (key, lo, hi) in &spans {
            eprintln!(
                "RESULT show_row_name={show_row_name}: row 0, x {lo}..={hi}, y {y} publishes CellClick(0, {key})"
            );
        }
        for (key, lo, hi) in &spans {
            let mut h = H::new(&program(show_row_name)).await?;
            let p = Point::new((lo + hi) / 2.0, y);
            let msgs = h.click(p);
            let published = cell_clicks(&msgs);
            h.dispatch(msgs).await?;
            let selected = h.value("selected");
            let activated = h.value("activated");
            eprintln!(
                "RESULT show_row_name={show_row_name}: click at ({}, {y}) published {published:?} -> selected={selected} activated={activated}",
                p.x
            );
            if key == "\"name\"" {
                let ok = selected == Value::String(literal!("r0/name"))
                    && activated == Value::String(literal!(""));
                if !ok {
                    failures.push(format!(
                        "show_row_name={show_row_name}: click on the \"name\" DATA cell gave selected={selected} activated={activated}, expected selected=\"r0/name\" activated=\"\""
                    ));
                }
            }
        }
        if !spans.iter().any(|(k, _, _)| k == "\"name\"") {
            failures.push(format!(
                "show_row_name={show_row_name}: no click published CellClick(0, \"name\")"
            ));
        }
    }
    assert!(failures.is_empty(), "{failures:#?}");
    Ok(())
}
