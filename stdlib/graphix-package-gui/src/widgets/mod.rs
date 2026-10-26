use anyhow::{Context, Result, bail};
use arcstr::ArcStr;
use compact_str::CompactString;
use graphix_compiler::{expr::ExprId, rep};
use graphix_rt::{Callable, CallableId, GXExt, GXHandle, Ref, reconcile};
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
    /// The text now typed into the combo box keyed by its options' ref;
    /// empty after a choice.
    ComboText(ExprId, String),
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
    pub call_handled: bool,
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
    // CR claude for claude: [dead/low] Nothing calls this through the trait. Its one
    // caller is DataTableW::on_message, on itself, and DataTableW overrides it as
    // resize_drag.is_some(), so the recursive default is dead. It is also the only
    // non-test user of the immutable for_each_child, which 11 containers implement
    // beside for_each_child_mut. Make it an inline read of resize_drag in the data
    // table. Then make for_each_child test-only, or move its three test callers to
    // for_each_child_mut and delete it. Found by reading; not run. (gui-widgets-a-03)
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

    /// The runtime is idle after a widget asked ([`ask_idle`]): a send it
    /// saw answered and not echoed was rejected. Returns `true` if the
    /// window should redraw. The default forwards to children.
    fn idle(&mut self) -> bool {
        let mut changed = false;
        self.for_each_child_mut(&mut |c| changed |= c.idle());
        changed
    }
}

/// Ask the runtime to say when it is next idle, which judges a send
/// [`Echoes::answered`] left waiting.
pub(crate) fn ask_idle<X: GXExt>(gx: &GXHandle<X>) {
    if let Err(e) = gx.notify_idle() {
        log::error!("could not ask for idle: {e:?}")
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
        // CR claude for claude: [structure/low] This recompile-on-change block is
        // copied by hand in MenuItems::update, MenuBarW::handle_update and twice in
        // TableW::handle_update. The matching None-or-cast at compile time is copied in
        // MenuItems::compile, MenuBarW::compile and TableW::compile. Children is
        // already this pair for GuiW items, and MenuItems is the same struct for menu
        // items. Make Children generic over its item (Children<X, T = GuiW<X>>, with
        // compile and update taking the item's compile function) and use it for menu
        // items, menu groups and table columns and rows. That deletes the copies, which
        // today differ in their error context. Found by reading; not run.
        // (gui-widgets-b-03)
        if id == self.r.id && self.r.last.as_ref() != Some(v) {
            let vals = v.clone().cast_to::<LPooled<Vec<Value>>>()?;
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

const MEASURED: usize = 8192;
type WidthMap =
    indexmap::IndexMap<(ArcStr, u32, iced_core::Font), f32, ahash::RandomState>;

#[derive(Hash)]
struct WidthKey<'a>(&'a str, u32, iced_core::Font);

impl indexmap::Equivalent<(ArcStr, u32, iced_core::Font)> for WidthKey<'_> {
    fn equivalent(&self, key: &(ArcStr, u32, iced_core::Font)) -> bool {
        self.0 == key.0.as_str() && self.1 == key.1 && self.2 == key.2
    }
}

#[derive(Default)]
struct WidthCache {
    current: WidthMap,
    old: WidthMap,
}

impl WidthCache {
    fn measure(&mut self, key: WidthKey<'_>, shape: impl FnOnce() -> f32) -> f32 {
        if let Some(w) = self.current.get(&key) {
            return *w;
        }
        let (key, w) = self
            .old
            .swap_remove_entry(&key)
            .unwrap_or_else(|| ((key.0.into(), key.1, key.2), shape()));
        if self.current.len() >= MEASURED {
            self.old.clear();
            std::mem::swap(&mut self.current, &mut self.old);
        }
        self.current.insert(key, w);
        w
    }
}

/// The unwrapped width of `text`, cached per thread in two bounded generations.
pub(crate) fn measure_text(text: &str, size: f32, font: iced_core::Font) -> f32 {
    thread_local! {
        static WIDTHS: std::cell::RefCell<WidthCache> = std::cell::RefCell::default();
    }
    WIDTHS.with(|m| {
        m.borrow_mut().measure(WidthKey(text, size.to_bits(), font), || {
            shape_width(text, size, font)
        })
    })
}

#[cfg(test)]
mod width_tests {
    use super::*;
    use compact_str::format_compact;

    #[test]
    fn width_cache_retains_more_than_one_generation() {
        let mut cache = WidthCache::default();
        let font = iced_core::Font::default();
        for i in 0..=MEASURED {
            let text = format_compact!("a long cell value over twenty-four bytes {i}");
            assert_eq!(cache.measure(WidthKey(&text, 16, font), || i as f32), i as f32);
        }
        for _ in 0..3 {
            for i in 0..=MEASURED {
                let text =
                    format_compact!("a long cell value over twenty-four bytes {i}");
                assert_eq!(
                    cache.measure(WidthKey(&text, 16, font), || panic!("reshaped {i}")),
                    i as f32
                );
            }
        }
        assert!(cache.current.len() <= MEASURED);
        assert!(cache.old.len() <= MEASURED);
        assert_eq!(
            cache.measure(
                WidthKey("a long cell value over twenty-four bytes 0", 17, font),
                || 42.0
            ),
            42.0
        );
    }
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
/// yet, each beside its call's answer id and whether that answer came. The
/// control shows its newest send, so input that comes before an echo builds
/// on what the user did, not on the stale value; a send answered and not
/// echoed by the time the runtime is idle was rejected.
// CR claude for claude: [structure/low] The echo protocol is four steps, and every
// control repeats them by hand: answered then ask_idle, and retain_handler, in
// handle_update; idle; and call_arg then send in on_message. That happens in checkbox
// and toggler (toggle.rs, whose two trait impls are identical outside the macro that
// was meant to share them), text_input and text_editor. A control that leaves out a
// step, such as ask_idle, never sees a rejection and shows the user's value forever.
// Give Echoes an update(gx, id, handler) -> bool (answered + ask_idle + retain_handler)
// and an on_call(msg, handler, shell, extract) -> bool, and move the shared toggle
// methods into toggle_widget!. Found by reading; not run. (gui-widgets-b-04)
pub(crate) struct Echoes<T>(std::collections::VecDeque<(ExprId, Option<T>, bool)>);

impl<T: PartialEq> Echoes<T> {
    pub(crate) fn new() -> Self {
        Self(std::collections::VecDeque::new())
    }

    pub(crate) fn send<X: GXExt>(
        &mut self,
        value: T,
        handler: &Handler<X>,
        msg: &Message,
        shell: &mut MessageShell,
    ) -> bool {
        let (Some(f), Message::Call(id, args)) = (handler.f.as_ref(), msg) else {
            return false;
        };
        if f.id() != *id {
            return false;
        }
        shell.call_handled = true;
        match f.call_answered(args.clone()) {
            Ok(()) => {
                self.0.push_back((f.answer, Some(value), false));
                true
            }
            Err(e) => {
                log::error!("failed to call: {e:?}");
                false
            }
        }
    }

    /// A value echo retires itself and earlier values; a program value
    /// retires them all.
    pub(crate) fn delivered(&mut self, v: &T) -> bool {
        let at = self.0.iter().position(|(_, p, _)| p.as_ref() == Some(v));
        let n = at.map_or(self.0.len(), |i| i + 1);
        for (_, p, _) in self.0.iter_mut().take(n) {
            *p = None;
        }
        at.is_some()
    }

    /// The answer `id` came for the oldest send it answers: one echoed
    /// already retires, one not waits for the runtime's idle to judge it,
    /// and this says so.
    pub(crate) fn answered(&mut self, id: ExprId) -> bool {
        let Some(i) = self.0.iter().position(|(a, _, done)| *a == id && !done) else {
            return false;
        };
        match self.0[i].1 {
            None => {
                self.0.remove(i);
                false
            }
            Some(_) => {
                self.0[i].2 = true;
                true
            }
        }
    }

    /// The runtime is idle: the sends answered and not echoed were
    /// rejected and retire, and this says whether there were any.
    pub(crate) fn idle(&mut self) -> bool {
        let mut rejected = false;
        self.0.retain(|(_, p, done)| {
            rejected |= *done && p.is_some();
            !done
        });
        rejected
    }

    pub(crate) fn retain_handler<X: GXExt>(&mut self, handler: &Handler<X>) -> bool {
        let answer = handler.f.as_ref().map(|f| f.answer);
        let mut changed = false;
        self.0.retain(|(id, v, _)| {
            let keep = Some(*id) == answer;
            changed |= !keep && v.is_some();
            keep
        });
        changed
    }

    /// What the control shows: its newest send, else the runtime's value.
    pub(crate) fn shown<'a>(&'a self, runtime: Option<&'a T>) -> Option<&'a T> {
        self.0.iter().rev().find_map(|(_, p, _)| p.as_ref()).or(runtime)
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
            gx.update_callable(&mut f, v).await?;
        }
        Ok(Self { r, f })
    }

    /// Mark a changed handler and return the callable it replaced, for a
    /// caller that must keep it alive until its shared callback id is updated.
    pub(crate) fn update(
        &mut self,
        rt: &tokio::runtime::Handle,
        gx: &GXHandle<X>,
        id: ExprId,
        v: &Value,
        changed: &mut bool,
    ) -> Result<Option<Callable<X>>> {
        if id != self.r.id {
            return Ok(None);
        }
        let previous = self.id();
        let retired = rt.block_on(gx.update_callable(&mut self.f, v))?;
        self.r.last = Some(v.clone());
        *changed |= previous != self.id();
        Ok(retired)
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
// CR claude for claude: [bug/medium] This compile, and the handle_update, view and
// on_message recursion over the widget tree, run with no stack guard. A container chain
// 300 deep, compiled as the event loop does it (Handle::block_on on the 8 MiB main
// thread), overflows and aborts the process; 100 deep compiles. The TUI got the fix for
// x-stack-01 (StackGuarded around its compile future, ensure_sufficient in
// handle_update and draw), but the GUI never did. Fix: move StackGuarded to one place
// both UI packages share (graphix-rt, beside reconcile), wrap this Box::pin in it, and
// run the GuiWidget recursion under ensure_sufficient. probe:
// design/review-2026-10-26/repro/x-stack-gui-deep-widget.rs (x-stack-02)
pub fn compile<X: GXExt>(gx: GXHandle<X>, source: Value) -> CompileFut<X> {
    Box::pin(async move {
        let Some((s, [v])) = rep::variant(&source) else {
            bail!("invalid gui widget {source}")
        };
        let v = v.clone();
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
            _ => bail!(
                "invalid gui widget type `{s}({})",
                graphix_compiler::typ::Naked(&v)
            ),
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

#[cfg(test)]
mod echo_tests {
    use super::*;

    #[test]
    fn an_unechoed_answer_is_judged_at_idle() {
        let answer = ExprId::new();
        let mut echoes = Echoes::new();
        echoes.0.extend([(answer, Some(1), false), (answer, Some(2), false)]);
        assert!(echoes.delivered(&1));
        assert!(!echoes.answered(answer));
        assert_eq!(echoes.shown(Some(&0)), Some(&2));
        // answered, not echoed: it shows until the runtime is idle
        assert!(echoes.answered(answer));
        assert_eq!(echoes.shown(Some(&0)), Some(&2));
        assert!(echoes.idle());
        assert_eq!(echoes.shown(Some(&0)), Some(&0));
        assert!(echoes.0.is_empty());
        // an echo after the answer and before idle is no rejection
        echoes.0.push_back((answer, Some(3), false));
        assert!(echoes.answered(answer));
        assert!(echoes.delivered(&3));
        assert!(!echoes.idle());
        assert!(echoes.0.is_empty());
        // a send not answered yet waits out the idle
        echoes.0.push_back((answer, Some(4), false));
        assert!(!echoes.idle());
        assert_eq!(echoes.shown(Some(&0)), Some(&4));
    }
}
