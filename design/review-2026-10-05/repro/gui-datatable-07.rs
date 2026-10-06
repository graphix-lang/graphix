//! gui-datatable-07: every update of a data table's `#table` ref tears
//! down all of the widget's subscriptions (`apply_table_sync` calls
//! `clear_subs`, subscriptions.rs:277), zeroes `first_row`/`first_col`
//! and drops the cell being edited (:282-284), even when the new table
//! is identical or only gained a row.
//!
//! Command (from the repository root), with this file copied to
//! stdlib/graphix-package-gui/tests/review_gui_datatable_07.rs:
//!   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_gui_datatable_07 -- --nocapture
//!
//! Frames are built as `GuiHandler::about_to_wait` builds them
//! (`before_view`, `view`, `UserInterface::build`, `update`; the
//! resulting messages go to `on_message` afterwards) on a headless wgpu
//! renderer, 400x330 logical (14 rows in view). What a frame shows is
//! read with an iced `Operation`: every `text` (row names r<i>, cell
//! values v<i>), every `text_input` (an open cell editor) and the overlay
//! scrollable's translation (where its scrollbar thumb is). The program
//! publishes /local/<p>/r<i>/c0 = "v<i>" for i in 0..=600; `rows` holds
//! 600 row paths; `#on_update` writes "<path>=<value>" to `updates`, and
//! the harness counts every write. During a write of `rows` a frame is
//! rendered after each batch of updates the widget receives.
//!
//! `identical_refire`: `rows` is written with the same 600 paths.
//! `one_row_added`: a real wheel scroll moves to row 500, the editor is
//! opened on r502 and "typed" entered, then `rows` gains r600 at the
//! end, then Enter (CellEditSubmit).
//!
//! Expected (data_table.md: "Every change to this ref is reconciled
//! against the current subscription set"; mod.rs:125 "The cell being
//! edited, keyed by path so it survives scroll and sort changes"):
//! cells keep their values, no on_update fires for unchanged cells, the
//! view stays at row 500 with the editor open, and Enter commits
//! "/local/<p>/r502/c0=typed".
//! Observed at c722befe (dev profile), both tests fail:
//!   RESULT identical: before the write: names r0..r13 (14 rows), cells "v0".."v13" (0 blank of 14), editors 0, scrollbar at row 0
//!   RESULT identical: frame 0 after the write: names r0..r13 (14 rows), cells "v0".."v13" (0 blank of 14), editors 0, scrollbar at row 0
//!   RESULT identical: frame 1 after the write: names r0..r13 (14 rows), cells "".."" (14 blank of 14), editors 0, scrollbar at row 0
//!   RESULT identical: frame 2 after the write: names r0..r13 (14 rows), cells "v0".."v13" (0 blank of 14), editors 0, scrollbar at row 0
//!   RESULT identical: on_update calls caused by the identical write: 64 (first Some("/local/dt07a/r12/c0=v12"), last Some("/local/dt07a/r26/c0=v26"))
//!   panicked: frame 1 after an identical table write: the cells it shows went blank
//!   RESULT one_row: after a wheel scroll of 500 rows: names r500..r513 (14 rows), cells "v500".."v513" (0 blank of 14), editors 0, scrollbar at row 500
//!   RESULT one_row: editor opened on r502, "typed" entered: names r500..r513 (14 rows), cells "v500".."v513" (0 blank of 13), editors 1, scrollbar at row 500
//!   RESULT one_row: frame 0 after r600 is added: names r500..r513 (14 rows), cells "v500".."v513" (0 blank of 13), editors 1, scrollbar at row 500
//!   RESULT one_row: frame 1 after r600 is added: names r0..r13 (14 rows), cells "".."" (14 blank of 14), editors 0, scrollbar at row 500
//!   RESULT one_row: frame 2 after r600 is added: names r0..r13 (14 rows), cells "v0".."v13" (0 blank of 14), editors 0, scrollbar at row 500
//!   RESULT one_row: after settling: names r0..r13 (14 rows), cells "v0".."v13" (0 blank of 14), editors 0, scrollbar at row 500
//!   RESULT one_row: edited after Enter: ""
//!   panicked: frame 1 after r600 is added at the end: the view moved (scrollbar at row 500)
//!     left: Some("r0")  right: Some("r500")
//! Frame 0 is the batch before the table update reaches the widget. The
//! table update blanks every visible cell until the new subscriptions
//! answer, re-fires on_update for all 64 subscribed cells (14 in view +
//! the 50-row buffer) with unchanged values, shows rows from r0 while
//! the scrollbar thumb stays at row 500 (until the user scrolls again),
//! and closes the editor: the typed text is lost and Enter commits
//! nothing.

use ahash::AHashMap;
use anyhow::{Context, Result, bail};
use graphix_compiler::{
    BindId,
    env::Env,
    expr::{ExprId, VfsEntry, VfsResolver},
};
use graphix_package_core::testing::{self, PackageRef, TestCtx};
use graphix_package_gui::{
    render::GpuState,
    widgets::{self, GuiW, Message, MessageShell},
};
use graphix_rt::{CompRes, GXEvent, NoExt, Ref};
use iced_core::{
    Event, Point, Rectangle, Size, Vector, clipboard, mouse,
    widget::{
        Id, Operation,
        operation::{Scrollable, TextInput},
    },
};
use iced_runtime::user_interface::{self, UserInterface};
use iced_wgpu::wgpu;
use netidx::{path::Path, protocol::valarray::ValArray, publisher::Value};
use poolshark::global::GPooled;
use std::{collections::VecDeque, time::Duration};
use tokio::sync::mpsc;

const REGISTER: &[PackageRef] = &[
    &graphix_package_core::P,
    &graphix_package_array::P,
    &graphix_package_map::P,
    &graphix_package_str::P,
    &graphix_package_sys::P,
    &graphix_package_gui::P,
];

const VIEWPORT: Size = Size::new(400.0, 330.0);
const CURSOR_AT: Point = Point::new(200.0, 150.0);
const ROW_H: f32 = 22.0;

fn program(p: &str) -> String {
    format!(
        r#"
use gui::data_table::{{data_table, text_column}};

let published = array::init(601, |i| sys::net::publish("/local/{p}/r[i]/c0", "v[i]"));

let rows: Array<string> = array::init(600, |i| "/local/{p}/r[i]");

let edited = "";

let updates = "";

let tbl = {{
  rows,
  columns: [text_column(#name: "c0", #on_edit: |#path: string, #value: Any| edited <- "[path]=[value]")]
}};

let result = data_table(
  #on_update: |#path: string, #value: Primitive| updates <- "[path]=[value]",
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

async fn gpu() -> GpuState {
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
            .expect("no GPU adapter available"),
    };
    let (device, queue) = adapter
        .request_device(&wgpu::DeviceDescriptor::default())
        .await
        .expect("failed to create GPU device");
    GpuState { instance, adapter, device, queue, format: wgpu::TextureFormat::Rgba8UnormSrgb }
}

/// What one frame shows.
#[derive(Default, Debug)]
struct Seen {
    texts: Vec<(f32, String)>,
    inputs: usize,
    scroll: Option<Vector>,
}

impl Operation for Seen {
    fn traverse(&mut self, operate: &mut dyn FnMut(&mut dyn Operation<()>)) {
        operate(self);
    }

    fn text(&mut self, _id: Option<&Id>, bounds: Rectangle, text: &str) {
        self.texts.push((bounds.y, text.to_string()));
    }

    fn text_input(
        &mut self,
        _id: Option<&Id>,
        _bounds: Rectangle,
        _state: &mut dyn TextInput,
    ) {
        self.inputs += 1;
    }

    fn scrollable(
        &mut self,
        _id: Option<&Id>,
        _bounds: Rectangle,
        _content_bounds: Rectangle,
        translation: Vector,
        _state: &mut dyn Scrollable,
    ) {
        self.scroll = Some(translation);
    }
}

impl Seen {
    fn sorted(&self) -> Vec<&str> {
        let mut t: Vec<&(f32, String)> = self.texts.iter().collect();
        t.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap());
        t.into_iter().map(|(_, s)| s.as_str()).collect()
    }

    /// Row names shown, top to bottom.
    fn names(&self) -> Vec<String> {
        self.sorted()
            .into_iter()
            .filter(|s| s.starts_with('r') && s[1..].parse::<usize>().is_ok())
            .map(|s| s.to_string())
            .collect()
    }

    /// Cell texts shown, top to bottom (header and names excluded).
    fn cells(&self) -> Vec<String> {
        self.sorted()
            .into_iter()
            .filter(|s| !(s.starts_with('r') && s[1..].parse::<usize>().is_ok()))
            .filter(|s| *s != "name" && *s != "c0")
            .map(|s| s.to_string())
            .collect()
    }

    fn thumb_row(&self) -> f32 {
        self.scroll.map(|v| v.y / ROW_H).unwrap_or(f32::NAN)
    }

    fn line(&self) -> String {
        let names = self.names();
        let cells = self.cells();
        format!(
            "names {}..{} ({} rows), cells {:?}..{:?} ({} blank of {}), editors {}, scrollbar at row {}",
            names.first().map(|s| s.as_str()).unwrap_or("-"),
            names.last().map(|s| s.as_str()).unwrap_or("-"),
            names.len(),
            cells.first().map(|s| s.as_str()).unwrap_or("-"),
            cells.last().map(|s| s.as_str()).unwrap_or("-"),
            cells.iter().filter(|s| s.is_empty()).count(),
            cells.len(),
            self.inputs,
            self.thumb_row()
        )
    }
}

struct Session {
    p: &'static str,
    ctx: TestCtx,
    compiled: CompRes<NoExt>,
    rx: mpsc::Receiver<GPooled<Vec<GXEvent>>>,
    widget: GuiW<NoExt>,
    renderer: widgets::Renderer,
    cache: user_interface::Cache,
    updates_id: ExprId,
    edited_id: ExprId,
    updates: Vec<String>,
    edited: Value,
    refs: Vec<Ref<NoExt>>,
}

impl Session {
    async fn new(p: &'static str) -> Result<Self> {
        let (tx, mut rx) = mpsc::channel(100);
        let vfs = AHashMap::from_iter([(
            netidx_core::path::Path::from("/test.gx"),
            VfsEntry::from(arcstr::ArcStr::from(program(p))),
        )]);
        let ctx = testing::init_with_resolvers(tx, REGISTER, vec![VfsResolver::new(vfs)])
            .await?;
        let compiled = ctx
            .rt
            .compile(arcstr::literal!("{ mod test; test::result }"))
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
        let renderer = gpu().await.create_renderer();
        let updates_ref =
            ctx.rt.compile_ref(find_bind_id(&compiled.env, "test::updates")?).await?;
        let edited_ref =
            ctx.rt.compile_ref(find_bind_id(&compiled.env, "test::edited")?).await?;
        let updates_id = updates_ref.id;
        let edited_id = edited_ref.id;
        let edited = edited_ref.last.clone().unwrap_or(Value::Null);
        Ok(Self {
            p,
            ctx,
            compiled,
            rx,
            widget,
            renderer,
            cache: user_interface::Cache::default(),
            updates_id,
            edited_id,
            updates: vec![],
            edited,
            refs: vec![updates_ref, edited_ref],
        })
    }

    /// One frame as `about_to_wait` renders it; messages the frame
    /// produced are dispatched afterwards, as the event loop does.
    fn frame(&mut self, events: &[Event]) -> Seen {
        self.widget.before_view();
        let element = self.widget.view();
        let mut ui = UserInterface::build(
            element,
            VIEWPORT,
            std::mem::take(&mut self.cache),
            &mut self.renderer,
        );
        let mut messages: Vec<Message> = vec![];
        let mut clip = clipboard::Null;
        let cursor = mouse::Cursor::Available(CURSOR_AT);
        let _ = ui.update(events, cursor, &mut self.renderer, &mut clip, &mut messages);
        let mut seen = Seen::default();
        ui.operate(&self.renderer, &mut seen);
        self.cache = ui.into_cache();
        let mut pending: VecDeque<Message> = messages.into_iter().collect();
        while let Some(m) = pending.pop_front() {
            match m {
                Message::Nop => {}
                Message::Call(id, args) => {
                    let _ = self.ctx.rt.call(id, args);
                }
                other => {
                    let mut shell = MessageShell::new(CURSOR_AT);
                    self.widget.on_message(&other, &mut shell);
                    pending.extend(shell.out.drain(..));
                }
            }
        }
        seen
    }

    fn message(&mut self, m: Message) {
        let mut shell = MessageShell::new(CURSOR_AT);
        self.widget.on_message(&m, &mut shell);
    }

    /// Deliver every update to the widget as the event loop does, until
    /// nothing arrives for `idle`. With `frames`, a frame is rendered
    /// right after each batch, before anything else runs.
    async fn drain(&mut self, idle: Duration, mut frames: Option<&mut Vec<Seen>>) -> Result<()> {
        let rt = tokio::runtime::Handle::current();
        while let Ok(Some(mut batch)) = tokio::time::timeout(idle, self.rx.recv()).await {
            for e in batch.drain(..) {
                if let GXEvent::Updated(i, v) = e {
                    if i == self.updates_id {
                        if let Value::String(s) = &v {
                            self.updates.push(s.to_string());
                        }
                    }
                    if i == self.edited_id {
                        self.edited = v.clone();
                    }
                    self.widget.handle_update(&rt, i, &v)?;
                }
            }
            if let Some(f) = frames.as_deref_mut() {
                f.push(self.frame(&[]));
            }
        }
        Ok(())
    }

    async fn settle(&mut self) -> Result<()> {
        self.drain(Duration::from_millis(1500), None).await
    }

    async fn set_rows(&mut self, n: usize) -> Result<Vec<Seen>> {
        let p = self.p;
        let rows = Value::Array(ValArray::from_iter(
            (0..n).map(|i| Value::String(format!("/local/{p}/r{i}").into())),
        ));
        let bid = find_bind_id(&self.compiled.env, "test::rows")?;
        let mut r = self.ctx.rt.compile_ref(bid).await?;
        r.set(rows)?;
        self.refs.push(r);
        let mut frames = vec![];
        self.drain(Duration::from_millis(1500), Some(&mut frames)).await?;
        Ok(frames)
    }
}

/// The table is written with exactly the rows it already has.
#[tokio::test(flavor = "current_thread")]
async fn identical_refire() -> Result<()> {
    let mut s = Session::new("dt07a").await?;
    s.frame(&[]);
    s.settle().await?;
    let before = s.frame(&[]);
    let n_before = s.updates.len();
    eprintln!("RESULT identical: before the write: {}", before.line());
    eprintln!("RESULT identical: on_update calls so far: {n_before}");
    let frames = s.set_rows(600).await?;
    s.settle().await?;
    let after = s.frame(&[]);
    let refired: Vec<&String> = s.updates[n_before..].iter().collect();
    for (i, f) in frames.iter().take(3).enumerate() {
        eprintln!("RESULT identical: frame {i} after the write: {}", f.line());
    }
    eprintln!("RESULT identical: after settling: {}", after.line());
    eprintln!(
        "RESULT identical: on_update calls caused by the identical write: {} (first {:?}, last {:?})",
        refired.len(),
        refired.first(),
        refired.last()
    );
    assert_eq!(
        before.cells().iter().take(5).map(|s| s.as_str()).collect::<Vec<_>>(),
        vec!["v0", "v1", "v2", "v3", "v4"]
    );
    for (i, f) in frames.iter().enumerate() {
        assert_eq!(
            f.cells(),
            before.cells(),
            "frame {i} after an identical table write: the cells it shows went blank"
        );
    }
    assert!(
        refired.is_empty(),
        "an identical table changed no cell, yet on_update fired {} times",
        refired.len()
    );
    Ok(())
}

/// Scrolled to row 500 with an edit open; the table gains one row.
#[tokio::test(flavor = "current_thread")]
async fn one_row_added() -> Result<()> {
    let mut s = Session::new("dt07b").await?;
    s.frame(&[]);
    s.settle().await?;
    let wheel = Event::Mouse(mouse::Event::WheelScrolled {
        delta: mouse::ScrollDelta::Pixels { x: 0.0, y: -500.0 * ROW_H },
    });
    s.frame(&[wheel]);
    s.settle().await?;
    let scrolled = s.frame(&[]);
    eprintln!("RESULT one_row: after a wheel scroll of 500 rows: {}", scrolled.line());
    s.message(Message::CellEdit(502, arcstr::literal!("c0")));
    s.message(Message::CellEditInput("typed".into()));
    let editing = s.frame(&[]);
    eprintln!("RESULT one_row: editor opened on r502, \"typed\" entered: {}", editing.line());
    let frames = s.set_rows(601).await?;
    for (i, f) in frames.iter().take(3).enumerate() {
        eprintln!("RESULT one_row: frame {i} after r600 is added: {}", f.line());
    }
    s.settle().await?;
    let settled = s.frame(&[]);
    eprintln!("RESULT one_row: after settling: {}", settled.line());
    s.message(Message::CellEditSubmit);
    s.settle().await?;
    eprintln!("RESULT one_row: edited after Enter: {}", s.edited);
    assert_eq!(scrolled.names().first().map(|s| s.as_str()), Some("r500"));
    assert_eq!(editing.inputs, 1, "the editor is open before the table changes");
    for (i, f) in frames.iter().chain(std::iter::once(&settled)).enumerate() {
        assert_eq!(
            f.names().first().map(|s| s.as_str()),
            Some("r500"),
            "frame {i} after r600 is added at the end: the view moved (scrollbar at row {})",
            f.thumb_row()
        );
        assert_eq!(f.inputs, 1, "frame {i}: the edit in progress should survive");
    }
    assert_eq!(
        s.edited,
        Value::String(format!("/local/{}/r502/c0=typed", s.p).into()),
        "Enter should commit the edit"
    );
    Ok(())
}
