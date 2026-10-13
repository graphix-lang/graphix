use anyhow::{Context, Result, bail};
use arcstr::ArcStr;
use compact_str::CompactString;
use graphix_compiler::expr::ExprId;
use graphix_rt::{Callable, CallableId, GXExt, GXHandle, Ref};
use netidx::{protocol::valarray::ValArray, publisher::Value};
use poolshark::local::LPooled;
use serde_derive::{Deserialize, Serialize};
use std::{future::Future, pin::Pin};

use crate::types::{HAlignV, LengthV, PaddingV, VAlignV};

pub mod button;
pub mod canvas;
pub mod chart;
pub mod combo_box;
pub mod container;
pub mod context_menu;
pub mod context_menu_widget;
pub mod data_table;
pub mod grid;
pub mod iced_keyboard_area;
pub mod image;
pub mod keyboard_area;
pub mod markdown;
pub mod menu_bar;
pub mod menu_bar_widget;
pub mod mouse_area;
pub mod pick_list;
pub mod progress_bar;
pub mod qr_code;
pub mod radio;
pub mod rule;
pub mod scrollable;
pub mod slider;
pub mod space;
pub mod stack;
pub mod table;
pub mod text;
pub mod text_editor;
pub mod text_input;
pub mod toggle;
pub mod tooltip;

/// Concrete iced renderer type used throughout the GUI package.
/// Must match iced_widget's default Renderer parameter.
pub type Renderer = iced_renderer::Renderer;

/// Concrete iced Element type with our Message/Theme/Renderer.
pub type IcedElement<'a> =
    iced_core::Element<'a, Message, crate::theme::GraphixTheme, Renderer>;

/// Message type for iced widget interactions.
#[derive(Debug, Clone)]
pub enum Message {
    Nop,
    Call(CallableId, ValArray),
    EditorAction(ExprId, iced_widget::text_editor::Action),
    /// A data table's own input, addressed to that table alone.
    Table(TableId, TableMsg),
}

netidx_core::atomic_id!(TableId);
netidx_core::atomic_id!(ChartId);

/// A data table's input.
#[derive(Debug, Clone)]
pub enum TableMsg {
    /// Virtual scroll position changed: (offset_x, offset_y, viewport_w, viewport_h)
    /// All values in logical pixels.
    Scroll(f32, f32, f32, f32),
    /// A cell was clicked (row index, column name).
    CellClick(usize, ArcStr),
    /// A cell was clicked to begin editing (row index, column name).
    CellEdit(usize, ArcStr),
    /// Cell edit text changed (new text).
    CellEditInput(CompactString),
    /// Cell edit submitted (Enter pressed).
    CellEditSubmit,
    /// Cell edit cancelled (Escape or click elsewhere).
    CellEditCancel,
    /// Column resize drag started (col_meta index).
    ColumnResizeStart(usize),
    /// Cursor moved while a column resize drag might be active
    /// (cursor x in widget-local coordinates); only a dragging table consumes it.
    ColumnResizeMove(f32),
    /// Column resize drag ended.
    ColumnResizeEnd,
    /// Keyboard navigation.
    Key(TableKeyAction),
}

/// Keyboard actions for data table navigation.
#[derive(Debug, Clone)]
pub enum TableKeyAction {
    Up,
    Down,
    Left,
    Right,
    /// Enter: drill down (fire on_activate)
    Enter,
    /// Space: start editing selected cell
    Space,
    /// Escape: cancel editing
    Escape,
}

/// The queue `GuiWidget::on_message` publishes follow-up messages to.
#[derive(Default)]
pub struct MessageShell {
    pub out: LPooled<Vec<Message>>,
}

impl MessageShell {
    pub fn publish(&mut self, msg: Message) {
        self.out.push(msg);
    }
}

/// Trait for GUI widgets. `handle_update` is synchronous; `view`
/// builds an iced Element tree.
pub trait GuiWidget<X: GXExt>: Send + 'static {
    /// Process a value update from graphix. Widgets that own child
    /// refs use `rt` to `block_on` recompilation of their subtree.
    /// Returns `true` if the widget changed and the window should redraw.
    fn handle_update(
        &mut self,
        rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool>;

    /// Build the iced Element tree for rendering.
    fn view(&self) -> IcedElement<'_>;

    /// Visit each child widget, every group of them; `on_message`,
    /// `before_view` and the other defaults forward through it. Leaves
    /// have none (the default); containers override.
    fn for_each_child_mut(&mut self, _f: &mut dyn FnMut(&mut GuiW<X>)) {}

    fn for_each_child(&self, _f: &mut dyn FnMut(&GuiW<X>)) {}

    /// Dispatch a message to the widget. Returns `true` if a redraw
    /// is needed. Follow-up messages go through `shell`. The default
    /// forwards to children.
    fn on_message(&mut self, msg: &Message, shell: &mut MessageShell) -> bool {
        let mut changed = false;
        self.for_each_child_mut(&mut |c| changed |= c.on_message(msg, shell));
        changed
    }

    /// True if this widget or any descendant is tracking a
    /// column-resize drag.
    fn is_column_resizing(&self) -> bool {
        let mut resizing = false;
        self.for_each_child(&mut |c| resizing |= c.is_column_resizing());
        resizing
    }

    /// Return a DataTableSnapshot if this widget is a data table.
    #[cfg(test)]
    fn data_table_snapshot(&self) -> Option<DataTableSnapshot> {
        None
    }

    /// Downcast escape hatch for tests. The default panics; only
    /// widgets with test-inspected state override it.
    #[cfg(test)]
    fn as_any(&self) -> &dyn std::any::Any {
        unimplemented!("as_any not implemented for this widget")
    }

    #[cfg(test)]
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        unimplemented!("as_any_mut not implemented for this widget")
    }

    /// What this widget asks of its iced widgets (a scroll it made itself,
    /// a focus), for the frame to apply before the next events.
    fn take_ops(&mut self, out: &mut Vec<WidgetOp>) {
        self.for_each_child_mut(&mut |c| c.take_ops(out));
    }

    /// Called immediately before `view()` to flush state that arrived
    /// from background tasks. Returns `true` if the window should
    /// redraw. The default forwards to children.
    fn before_view(&mut self) -> bool {
        let mut changed = false;
        self.for_each_child_mut(&mut |c| changed |= c.before_view());
        changed
    }
}

pub type GuiW<X> = Box<dyn GuiWidget<X>>;

/// What a widget asks of its iced widgets between frames.
pub enum WidgetOp {
    /// Put a scrollable at an offset.
    ScrollTo(
        iced_core::widget::Id,
        iced_core::widget::operation::scrollable::AbsoluteOffset<Option<f32>>,
    ),
    /// Give a widget the keyboard focus.
    Focus(iced_core::widget::Id),
}

/// Point `slot` at the callable `v` names, `None` for null; a slot already
/// compiled for `v` keeps its call site. Returns the callable it replaced.
pub(crate) async fn set_callable<X: GXExt>(
    gx: &GXHandle<X>,
    slot: &mut Option<Callable<X>>,
    v: &Value,
) -> Result<Option<Callable<X>>> {
    match v {
        Value::Null => Ok(slot.take()),
        v if slot.as_ref().is_some_and(|c| c.is_for(v)) => Ok(None),
        v => Ok(slot.replace(gx.compile_callable(v.clone()).await?)),
    }
}

/// `set_callable` from synchronous code outside the runtime.
pub(crate) fn update_callable_blocking<X: GXExt>(
    rt: &tokio::runtime::Handle,
    gx: &GXHandle<X>,
    slot: &mut Option<Callable<X>>,
    v: &Value,
) -> Result<Option<Callable<X>>> {
    match v {
        Value::Null => Ok(slot.take()),
        v if slot.as_ref().is_some_and(|c| c.is_for(v)) => Ok(None),
        v => rt.block_on(set_callable(gx, slot, v)),
    }
}

/// A child widget property: its ref and the widget its value compiled
/// to.
pub struct Child<X: GXExt> {
    pub(crate) r: Ref<X>,
    pub(crate) w: GuiW<X>,
}

impl<X: GXExt> Child<X> {
    /// The child the widget value `v`'s field `name` refers to.
    pub(crate) async fn field(gx: &GXHandle<X>, v: &Value, name: &str) -> Result<Self> {
        let r = gx.compile_field(v, name).await?;
        Self::compile(gx, r).await.with_context(|| format!("child {name}"))
    }

    pub(crate) async fn compile(gx: &GXHandle<X>, r: Ref<X>) -> Result<Self> {
        let w = match r.last.as_ref() {
            None => Box::new(EmptyW) as GuiW<X>,
            Some(v) => compile(gx.clone(), v.clone()).await?,
        };
        Ok(Self { r, w })
    }

    /// Take an update for the ref, recompiling only a changed value, or for
    /// a descendant. Returns whether the window should redraw.
    pub(crate) fn update(
        &mut self,
        rt: &tokio::runtime::Handle,
        gx: &GXHandle<X>,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let mut changed = false;
        if id == self.r.id && self.r.last.as_ref() != Some(v) {
            self.w = rt.block_on(compile(gx.clone(), v.clone()))?;
            self.r.last = Some(v.clone());
            changed = true;
        }
        Ok(self.w.handle_update(rt, id, v)? || changed)
    }
}

/// A list of child widgets: its ref, and each widget beside the value it
/// compiled from, so a new list keeps the children it still holds.
pub struct Children<X: GXExt> {
    pub(crate) r: Ref<X>,
    pub(crate) ws: Vec<(Value, GuiW<X>)>,
}

impl<X: GXExt> Children<X> {
    /// The children the widget value `v`'s field `name` refers to.
    pub(crate) async fn field(gx: &GXHandle<X>, v: &Value, name: &str) -> Result<Self> {
        let r = gx.compile_field(v, name).await?;
        Self::compile(gx, r).await.with_context(|| format!("children {name}"))
    }

    pub(crate) async fn compile(gx: &GXHandle<X>, r: Ref<X>) -> Result<Self> {
        let vals = match r.last.as_ref() {
            None => LPooled::take(),
            Some(v) => v.clone().cast_to::<LPooled<Vec<Value>>>()?,
        };
        let mut ws = Vec::new();
        reconcile(&mut ws, vals, |v| compile(gx.clone(), v)).await?;
        Ok(Self { r, ws })
    }

    /// Take an update for the ref or for a descendant. A new list keeps
    /// every child whose value it still holds, wherever it moved, and
    /// compiles only the values it did not. Returns whether the window
    /// should redraw.
    pub(crate) fn update(
        &mut self,
        rt: &tokio::runtime::Handle,
        gx: &GXHandle<X>,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let mut changed = false;
        if id == self.r.id && self.r.last.as_ref() != Some(v) {
            let vals = v.clone().cast_to::<LPooled<Vec<Value>>>()?;
            // CR claude for claude: [structure/low] `Children::update` is `reconcile`
            // written out again: keep_equal, compile the fresh values, interleave. It
            // differs only in keeping the widgets and values in two parallel vectors.
            // `Children::compile` and `compile_children` each repeat the initial
            // try_join_all. Store `Vec<(Value, GuiW<X>)>` in Children and route compile
            // (as `reconcile([], ..)`) and update (as `rt.block_on(reconcile(..))`)
            // through `reconcile`. Then a reconciliation fix (gui-widgets-a-07, and the
            // one for the drain-before-compile bug) lands once. Likewise
            // `update_callable_blocking` repeats `set_callable`'s match only to skip a
            // `block_on`, and `GXHandle::update_callable` is a third copy of the same
            // keep-if-`is_for` rule. Keep one, which handles null and returns the
            // callable it replaced (Handler::update needs it). (gui-widgets-a-09)
            // 2026-10-13 codex: children_survive_a_failed_recompile_and_retry and
            // reconciliation_compiles_fresh_entries_concurrently pin the shared
            // collection path. The callable rule still needs graphix-rt/src/lib.rs's
            // GXHandle::update_callable to handle null and return the replaced
            // Callable. That API change is outside this worktree's scope.
            rt.block_on(reconcile(&mut self.ws, vals, |v| compile(gx.clone(), v)))?;
            self.r.last = Some(v.clone());
            changed = true;
        }
        for (_, w) in &mut self.ws {
            changed |= w.handle_update(rt, id, v)?;
        }
        Ok(changed)
    }
}

/// Pair each of the `new` values with an old item made from an equal
/// value, the one at the same index first, else any; `None` where no old
/// item is left.
pub(crate) fn keep_equal<T>(
    old: impl IntoIterator<Item = (Value, T)>,
    new: &[Value],
) -> LPooled<Vec<Option<T>>> {
    let mut old: LPooled<Vec<Option<(Value, T)>>> = old.into_iter().map(Some).collect();
    new.iter()
        .enumerate()
        .map(|(i, val)| {
            let same = |o: &Option<(Value, T)>| o.as_ref().is_some_and(|(v, _)| v == val);
            let at = match old.get(i) {
                Some(o) if same(o) => Some(i),
                // CR claude for claude: [perf/low] On a miss at index i, `keep_equal`
                // scans `old` from 0, so a list whose widget values were reordered (a
                // reversed or re-sorted array of widgets) makes about n^2/2 Value
                // comparisons on the GUI thread: about 12.5M for a 5000-item column,
                // with nothing to compile. A prepend costs n^2/2 checks of taken slots
                // on top. The common array::map case keeps values at the same index and
                // never gets here. Fix: keep the old[i] check first, and on the first
                // miss build an index once (a pooled map from &Value to a queue of old
                // positions), then take positions from it, so a reorder is linear and
                // an in-place update allocates nothing. (gui-widgets-a-07)
                _ => old.iter().position(same),
            };
            at.and_then(|j| old[j].take()).map(|(_, t)| t)
        })
        .collect()
}

/// Old entries stay intact until every fresh item compiles. Fresh items
/// compile concurrently; equal values keep their existing item.
pub(crate) async fn reconcile<T, F: Future<Output = Result<T>>>(
    old: &mut Vec<(Value, T)>,
    mut items: LPooled<Vec<Value>>,
    compile: impl Fn(Value) -> F,
) -> Result<()> {
    let mut kept =
        keep_equal(old.iter().enumerate().map(|(i, (v, _))| (v.clone(), i)), &items);
    let fresh = items
        .iter()
        .zip(kept.iter())
        .filter(|(_, t)| t.is_none())
        .map(|(v, _)| compile(v.clone()));
    let mut fresh = futures::future::try_join_all(fresh).await?.into_iter();
    let mut slots: LPooled<Vec<Option<T>>> =
        old.drain(..).map(|(_, t)| Some(t)).collect();
    old.extend(items.drain(..).zip(kept.drain(..)).map(|(v, at)| {
        let t = match at {
            Some(i) => slots[i].take().expect("each kept item is used once"),
            None => fresh.next().expect("each fresh item has a result"),
        };
        (v, t)
    }));
    Ok(())
}

/// The width `text` lays out to at `size` in `font`, unwrapped. Widths
/// are remembered per thread, up to `MEASURED` of them, since a layout
/// measures the same cells again every frame.
pub(crate) fn measure_text(text: &str, size: f32, font: iced_core::Font) -> f32 {
    const MEASURED: usize = 8192;
    type Key = (CompactString, u32, iced_core::Font);
    thread_local! {
        static WIDTHS: std::cell::RefCell<ahash::AHashMap<Key, f32>> =
            std::cell::RefCell::new(ahash::AHashMap::default());
    }
    let key = (CompactString::from(text), size.to_bits(), font);
    if let Some(w) = WIDTHS.with(|m| m.borrow().get(&key).copied()) {
        return w;
    }
    let w = shape_width(text, size, font);
    WIDTHS.with(|m| {
        let mut m = m.borrow_mut();
        // CR claude for claude: [perf/low] When the width cache fills it clears
        // everything. A data_table frame measures every visible cell, plus about
        // log2(len) prefixes for each truncated one through truncate_to_width. Once a
        // frame needs more than MEASURED widths, each frame clears the cache partway
        // through and re-shapes nearly every string with cosmic-text: a cliff, not
        // gradual decay. Separately, every lookup builds a CompactString key, which
        // heap-allocates on a hit for any text over 24 bytes. Fix: keep two generations
        // (on overflow the current map becomes the old one, and a hit in the old one is
        // promoted) so the last frame's working set survives. Look up without building
        // an owned key, e.g. by a precomputed hash of (text, size, font) or a hashbrown
        // map with an Equivalent borrowed key. (gui-widgets-a-08)
        if m.len() >= MEASURED {
            m.clear();
        }
        m.insert(key, w);
    });
    w
}

fn shape_width(text: &str, size: f32, font: iced_core::Font) -> f32 {
    use iced_core::text::Paragraph as _;
    type Paragraph = <Renderer as iced_core::text::Renderer>::Paragraph;
    Paragraph::with_text(iced_core::Text {
        content: text,
        bounds: iced_core::Size::new(f32::INFINITY, f32::INFINITY),
        size: iced_core::Pixels(size),
        line_height: iced_core::text::LineHeight::default(),
        font,
        align_x: iced_core::alignment::Horizontal::Left.into(),
        align_y: iced_core::alignment::Vertical::Top,
        shaping: iced_core::text::Shaping::Advanced,
        wrapping: iced_core::text::Wrapping::None,
    })
    .min_bounds()
    .width
}

/// What a control sent through its handler and the runtime has not echoed
/// yet. The control shows its newest send, so input that comes before an
/// echo builds on what the user did, not on the stale value.
pub(crate) struct Echoes<T>(std::collections::VecDeque<T>);

// CR claude for claude: [bug/medium] Only `delivered` retires a send, and it runs only
// when the value ref updates. A send the handler never writes back is shown forever and
// is never freed. This happens when the handler rejects the change, as input validation
// does, or writes another variable. A checkbox over `false` whose handler writes
// nothing stays drawn checked, and its second click sends `false`. A text_input over ""
// sends "ab" for the key b. Both contradict the book, which says the widget displays
// the reference. `TextEditorW::pending` has the same logic and keeps a full copy of the
// document per keystroke: 150 keys into a 64 KiB doc retain about 10 MB until `content`
// changes. Fix: retire a send that is still unechoed once its call's cycle has ended
// (`Callable::call_answered` reports that point), so the control falls back to the
// program's value. Have text_editor use `Echoes<ArcStr>` instead of its own queue.
// Probe: design/review-2026-10-12/repro/gui-widgets-b-02.rs. (gui-widgets-b-02)
impl<T: PartialEq> Echoes<T> {
    pub(crate) fn new() -> Self {
        Self(std::collections::VecDeque::new())
    }

    pub(crate) fn sent(&mut self, v: T) {
        self.0.push_back(v)
    }

    /// The runtime delivered `v`: an echo retires itself and every earlier
    /// send; any other value is the program's own and drops them all.
    pub(crate) fn delivered(&mut self, v: &T) {
        match self.0.iter().position(|p| p == v) {
            Some(i) => drop(self.0.drain(..=i)),
            None => self.0.clear(),
        }
    }

    /// What the control shows: its newest send, else the runtime's value.
    pub(crate) fn shown<'a>(&'a self, runtime: Option<&'a T>) -> Option<&'a T> {
        self.0.back().or(runtime)
    }
}

/// A disabled choice widget: its selection in a text input that takes
/// no input, which iced draws as disabled.
pub(crate) fn disabled_choice<'a>(
    placeholder: &'a str,
    selected: &'a str,
    width: Option<&crate::types::LengthV>,
) -> IcedElement<'a> {
    let mut ti = iced_widget::TextInput::new(placeholder, selected);
    if let Some(w) = width {
        ti = ti.width(w.0);
    }
    ti.into()
}

/// The first argument of a call `msg` makes to `id`, if it makes one.
pub(crate) fn call_arg(msg: &Message, id: Option<CallableId>) -> Option<&Value> {
    match msg {
        Message::Call(cid, args) if Some(*cid) == id => args.first(),
        _ => None,
    }
}

/// A callback property: its ref and what it compiled to; null is no
/// handler.
pub(crate) struct Handler<X: GXExt> {
    pub(crate) r: Ref<X>,
    pub(crate) f: Option<Callable<X>>,
}

impl<X: GXExt> Handler<X> {
    /// The handler the widget value `v`'s field `name` refers to.
    pub(crate) async fn field(gx: &GXHandle<X>, v: &Value, name: &str) -> Result<Self> {
        let r = gx.compile_field(v, name).await?;
        Self::compile(gx, r).await.with_context(|| format!("handler {name}"))
    }

    pub(crate) async fn compile(gx: &GXHandle<X>, r: Ref<X>) -> Result<Self> {
        let mut f = None;
        if let Some(v) = r.last.as_ref() {
            set_callable(gx, &mut f, v).await?;
        }
        Ok(Self { r, f })
    }

    /// Take `v` when it is this handler's ref's. Returns the callable it
    /// replaced, for a caller that must outlive its id's last use.
    // CR claude for claude: [bug/low] Handler::update does not report whether the
    // handler changed, and no widget counts a handler update in the `changed` it
    // returns, so TrackedWindow never sets needs_redraw for one. iced draws a
    // text_input without on_input, and a button, checkbox or toggler without its
    // handler, as disabled. When only the handler goes between null and a function
    // (`#on_input: select locked { true => null, false => |v| x <- v }`), the control
    // keeps its old enabled or disabled look until some unrelated input event forces a
    // frame. Fix: have handler updates feed `changed`, either by returning whether this
    // ref took the value (keeping the retired callable for data_table's on_update) or
    // by callers OR-ing in `id == handler.r.id`. Do it in every widget that holds a
    // Handler. (gui-widgets-b-07)
    // CR claude for claude: [bug/medium] A handler update never asks for a redraw. This
    // returns the callable it replaced, and no caller turns the result into `changed`;
    // data_table only uses it to refresh its cells' on_update id. Yet a widget's view
    // depends on whether a handler is present: an iced Button or TextInput with no
    // on_press/on_input draws disabled, and checkboxes and menu actions behave the same
    // way. So when a handler goes from null to a function or back,
    // `GuiWindow::handle_update` leaves `needs_redraw` false, and the window keeps the
    // old look until some unrelated event renders a frame. Fix: return whether the
    // handler's presence or id changed, keep any callable that must outlive a pending
    // message inside Handler (data_table's on_update), and make each caller write
    // `changed |= self.on_x.update(..)?`. probe:
    // design/review-2026-10-12/repro/gui-widgets-a-06.rs (on_press null -> fn: the
    // click goes from 0 calls to 1, but handle_update returns false).
    // (gui-widgets-a-06)
    pub(crate) fn update(
        &mut self,
        rt: &tokio::runtime::Handle,
        gx: &GXHandle<X>,
        id: ExprId,
        v: &Value,
    ) -> Result<Option<Callable<X>>> {
        if id != self.r.id {
            return Ok(None);
        }
        self.r.last = Some(v.clone());
        update_callable_blocking(rt, gx, &mut self.f, v)
    }

    pub(crate) fn id(&self) -> Option<CallableId> {
        self.f.as_ref().map(|c| c.id())
    }
}

/// Snapshot of data table state for test assertions.
#[cfg(test)]
#[derive(Debug, Clone, PartialEq)]
pub struct DataTableSnapshot {
    pub col_names: Vec<String>,
    pub row_basenames: Vec<String>,
    pub grid: Vec<Vec<String>>,
    pub is_value_mode: bool,
    pub selection: Vec<String>,
}

/// Future type for widget compilation (avoids infinite-size async fn).
pub type CompileFut<X> = Pin<Box<dyn Future<Output = Result<GuiW<X>>> + Send + 'static>>;

/// Empty widget placeholder.
pub struct EmptyW;

impl<X: GXExt> GuiWidget<X> for EmptyW {
    fn handle_update(
        &mut self,
        _rt: &tokio::runtime::Handle,
        _id: ExprId,
        _v: &Value,
    ) -> Result<bool> {
        Ok(false)
    }

    fn view(&self) -> IcedElement<'_> {
        iced_widget::Space::new().into()
    }
}

/// Generate a flex layout widget (Row or Column).
macro_rules! flex_widget {
    ($name:ident, $props:ident, $label:literal, $align_ty:ty, $align:ident, $Widget:ident,
     $align_set:ident) => {
        graphix_rt::props! {
            struct $props {
                height: LengthV,
                padding: PaddingV,
                spacing: f64,
                width: LengthV,
                $align: $align_ty,
            }
        }

        pub(crate) struct $name<X: GXExt> {
            gx: GXHandle<X>,
            p: $props<X>,
            children: Children<X>,
        }

        impl<X: GXExt> $name<X> {
            pub(crate) async fn compile(
                gx: GXHandle<X>,
                source: Value,
            ) -> Result<GuiW<X>> {
                let (p, children) = futures::try_join!(
                    $props::compile(&gx, &source),
                    Children::field(&gx, &source, "children"),
                )
                .context($label)?;
                Ok(Box::new(Self { gx, p, children }))
            }
        }

        impl<X: GXExt> GuiWidget<X> for $name<X> {
            fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut GuiW<X>)) {
                self.children.ws.iter_mut().for_each(|(_, w)| f(w))
            }

            fn for_each_child(&self, f: &mut dyn FnMut(&GuiW<X>)) {
                self.children.ws.iter().for_each(|(_, w)| f(w))
            }

            fn handle_update(
                &mut self,
                rt: &tokio::runtime::Handle,
                id: ExprId,
                v: &Value,
            ) -> Result<bool> {
                let changed = self.p.update(id, v).context($label)?;
                Ok(self
                    .children
                    .update(rt, &self.gx, id, v)
                    .context(concat!($label, " children"))?
                    || changed)
            }

            fn view(&self) -> IcedElement<'_> {
                let mut w = iced_widget::$Widget::new();
                if let Some(sp) = self.p.spacing.t {
                    w = w.spacing(sp as f32);
                }
                if let Some(p) = self.p.padding.t.as_ref() {
                    w = w.padding(p.0);
                }
                if let Some(wi) = self.p.width.t.as_ref() {
                    w = w.width(wi.0);
                }
                if let Some(h) = self.p.height.t.as_ref() {
                    w = w.height(h.0);
                }
                if let Some(a) = self.p.$align.t.as_ref() {
                    w = w.$align_set(a.0);
                }
                for (_, child) in &self.children.ws {
                    w = w.push(child.view());
                }
                w.into()
            }
        }
    };
}

flex_widget!(RowW, RowProps, "row", VAlignV, valign, Row, align_y);

flex_widget!(ColumnW, ColumnProps, "column", HAlignV, halign, Column, align_x);

/// Compile a widget value into a GuiW. Returns a boxed future to
/// avoid infinite-size futures from recursive async calls.
pub fn compile<X: GXExt>(gx: GXHandle<X>, source: Value) -> CompileFut<X> {
    Box::pin(async move {
        let (s, v) = source.cast_to::<(ArcStr, Value)>()?;
        match s.as_str() {
            "Text" => text::TextW::compile(gx, v).await,
            "Column" => ColumnW::compile(gx, v).await,
            "Row" => RowW::compile(gx, v).await,
            "Container" => container::ContainerW::compile(gx, v).await,
            "Grid" => grid::GridW::compile(gx, v).await,
            "Button" => button::ButtonW::compile(gx, v).await,
            "Space" => space::SpaceW::compile(gx, v).await,
            "TextInput" => text_input::TextInputW::compile(gx, v).await,
            "Checkbox" => toggle::CheckboxW::compile(gx, v).await,
            "Toggler" => toggle::TogglerW::compile(gx, v).await,
            "Slider" => slider::SliderW::compile(gx, v).await,
            "ProgressBar" => progress_bar::ProgressBarW::compile(gx, v).await,
            "Scrollable" => scrollable::ScrollableW::compile(gx, v).await,
            "HorizontalRule" => rule::HorizontalRuleW::compile(gx, v).await,
            "VerticalRule" => rule::VerticalRuleW::compile(gx, v).await,
            "Tooltip" => tooltip::TooltipW::compile(gx, v).await,
            "PickList" => pick_list::PickListW::compile(gx, v).await,
            "Stack" => stack::StackW::compile(gx, v).await,
            "Radio" => radio::RadioW::compile(gx, v).await,
            "VerticalSlider" => slider::VerticalSliderW::compile(gx, v).await,
            "ComboBox" => combo_box::ComboBoxW::compile(gx, v).await,
            "TextEditor" => text_editor::TextEditorW::compile(gx, v).await,
            "KeyboardArea" => keyboard_area::KeyboardAreaW::compile(gx, v).await,
            "MouseArea" => mouse_area::MouseAreaW::compile(gx, v).await,
            "Image" => image::ImageW::compile(gx, v).await,
            "Canvas" => canvas::CanvasW::compile(gx, v).await,
            "ContextMenu" => context_menu::ContextMenuW::compile(gx, v).await,
            "Chart" => chart::ChartW::compile(gx, v).await,
            "Markdown" => markdown::MarkdownW::compile(gx, v).await,
            "MenuBar" => menu_bar::MenuBarW::compile(gx, v).await,
            "QrCode" => qr_code::QrCodeW::compile(gx, v).await,
            "Table" => table::TableW::compile(gx, v).await,
            "DataTable" => data_table::DataTableW::compile(gx, v).await,
            _ => bail!("invalid gui widget type `{s}({v})"),
        }
    })
}

/// Compile an array of widget values into a Vec of GuiW.
pub async fn compile_children<X: GXExt>(
    gx: GXHandle<X>,
    v: Value,
) -> Result<Vec<GuiW<X>>> {
    let items = v.cast_to::<LPooled<Vec<Value>>>()?;
    let mut pairs: LPooled<Vec<(Value, GuiW<X>)>> = LPooled::take();
    reconcile(&mut pairs, items, |item| compile(gx.clone(), item)).await?;
    Ok(pairs.drain(..).map(|(_, w)| w).collect())
}
