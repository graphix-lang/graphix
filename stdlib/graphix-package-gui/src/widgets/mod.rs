use ahash::AHashMap;
use anyhow::{Context, Result, bail};
use arcstr::ArcStr;
use compact_str::CompactString;
use graphix_compiler::{expr::ExprId, rep};
use graphix_rt::{Callable, CallableId, GXExt, GXHandle, Ref};
use netidx::{protocol::valarray::ValArray, publisher::Value};
use poolshark::local::LPooled;
use serde_derive::{Deserialize, Serialize};
use std::{future::Future, hash::Hash, pin::Pin};

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
pub(crate) fn keep_equal<K: Eq + Hash, T>(
    old: impl IntoIterator<Item = (K, T)>,
    new: &[K],
) -> LPooled<Vec<Option<T>>> {
    let mut keys: LPooled<Vec<K>> = LPooled::take();
    let mut items: LPooled<Vec<Option<T>>> = LPooled::take();
    for (key, item) in old {
        keys.push(key);
        items.push(Some(item));
    }
    let mut index: Option<LPooled<AHashMap<&K, usize>>> = None;
    let mut next: LPooled<Vec<Option<usize>>> = LPooled::take();
    new.iter()
        .enumerate()
        .map(|(i, val)| {
            if keys.get(i) == Some(val) && items[i].is_some() {
                return items[i].take();
            }
            let index = index.get_or_insert_with(|| {
                let mut index: LPooled<AHashMap<&K, usize>> = LPooled::take();
                next.resize(keys.len(), None);
                for (j, key) in keys.iter().enumerate().rev() {
                    if items[j].is_some() {
                        next[j] = index.insert(key, j);
                    }
                }
                index
            });
            while let Some(j) = index.get(val).copied() {
                match next[j] {
                    Some(k) => {
                        index.insert(&keys[j], k);
                    }
                    None => {
                        index.remove(val);
                    }
                }
                if let Some(item) = items[j].take() {
                    return Some(item);
                }
            }
            None
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
/// yet. The control shows its newest send, so input that comes before an
/// echo builds on what the user did, not on the stale value.
pub(crate) struct Echoes<T>(std::collections::VecDeque<(ExprId, Option<T>)>);

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
        match futures::executor::block_on(f.call_answered(args.clone())) {
            Ok(()) => {
                self.0.push_back((f.answer, Some(value)));
                true
            }
            Err(e) => {
                log::error!("failed to call: {e:?}");
                false
            }
        }
    }

    /// A value echo retires itself and earlier values; a program value
    /// retires them all. Answer slots stay ordered until their cycles end.
    pub(crate) fn delivered(&mut self, v: &T) -> bool {
        let at = self.0.iter().position(|(_, p)| p.as_ref() == Some(v));
        let n = at.map_or(self.0.len(), |i| i + 1);
        for (_, p) in self.0.iter_mut().take(n) {
            *p = None;
        }
        at.is_some()
    }

    pub(crate) fn answered(&mut self, id: ExprId) -> bool {
        self.0
            .iter()
            .position(|(answer, _)| *answer == id)
            .and_then(|i| self.0.remove(i))
            .is_some_and(|(_, v)| v.is_some())
    }

    pub(crate) fn retain_handler<X: GXExt>(&mut self, handler: &Handler<X>) -> bool {
        let answer = handler.f.as_ref().map(|f| f.answer);
        let mut changed = false;
        self.0.retain(|(id, v)| {
            let keep = Some(*id) == answer;
            changed |= !keep && v.is_some();
            keep
        });
        changed
    }

    /// What the control shows: its newest send, else the runtime's value.
    pub(crate) fn shown<'a>(&'a self, runtime: Option<&'a T>) -> Option<&'a T> {
        self.0.iter().rev().find_map(|(_, p)| p.as_ref()).or(runtime)
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

#[cfg(test)]
mod echo_tests {
    use super::*;

    #[test]
    fn an_echo_keeps_its_answer_slot_until_the_cycle_ends() {
        let answer = ExprId::new();
        let mut echoes = Echoes::new();
        echoes.0.extend([(answer, Some(1)), (answer, Some(2))]);
        assert!(echoes.delivered(&1));
        assert!(!echoes.answered(answer));
        assert_eq!(echoes.shown(Some(&0)), Some(&2));
        assert!(echoes.answered(answer));
        assert_eq!(echoes.shown(Some(&0)), Some(&0));
        assert!(echoes.0.is_empty());
        echoes.0.extend([(answer, Some(1)), (answer, Some(2))]);
        assert!(!echoes.delivered(&3));
        echoes.0.push_back((answer, Some(4)));
        assert!(!echoes.answered(answer));
        assert!(!echoes.answered(answer));
        assert_eq!(echoes.shown(Some(&3)), Some(&4));
        assert!(echoes.answered(answer));
        assert!(echoes.0.is_empty());
    }
}
