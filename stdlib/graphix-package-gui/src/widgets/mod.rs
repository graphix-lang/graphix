use anyhow::{Context, Result, bail};
use arcstr::ArcStr;
use compact_str::CompactString;
use graphix_compiler::expr::ExprId;
use graphix_rt::{Callable, CallableId, GXExt, GXHandle, Ref};
use netidx::{protocol::valarray::ValArray, publisher::Value};
use netidx_derive::FromValue;
use poolshark::local::LPooled;
use serde_derive::{Deserialize, Serialize};
use smallvec::SmallVec;
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

// CR claude for claude: [structure] Every gui and tui widget names each property six or
// more times: its field, the FromValue Fields struct, the destructure, the try_join! of
// compile_ref, TRef::new(..).context(..), and the .update(id, v).context(..) chain in
// handle_update. Across the two crates that is about 260 TRef constructions, 335
// compile_ref calls and 257 update calls. A property that is compiled but left out of
// handle_update never changes after startup, and nothing catches it. A declarative
// macro beside this family would make that omission impossible: it takes one `name:
// Type` list per widget and generates the decode, the joined compile_refs, the TRefs
// and the update chain. context_menu.rs:84-118 and menu_bar.rs:195-230 also repeat one
// menu-item update loop, which should be a single shared function. (x-dup-14)
// 2026-10-07 claude: Handler, Child and Children below own the callback and child
// properties, and MenuItemKind::update is the one menu-item loop. The property-list
// macro for the gui and tui widgets is still open, for the TUI batch.
/// A child widget property: its ref and the widget its value compiled
/// to.
pub struct Child<X: GXExt> {
    pub(crate) r: Ref<X>,
    pub(crate) w: GuiW<X>,
}

impl<X: GXExt> Child<X> {
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
            self.r.last = Some(v.clone());
            self.w = rt.block_on(compile(gx.clone(), v.clone()))?;
            changed = true;
        }
        Ok(self.w.handle_update(rt, id, v)? || changed)
    }
}

/// A list of child widgets: its ref, and each widget beside the value it
/// compiled from, so a new list keeps the children it still holds.
pub struct Children<X: GXExt> {
    pub(crate) r: Ref<X>,
    pub(crate) ws: Vec<GuiW<X>>,
    vals: Vec<Value>,
}

impl<X: GXExt> Children<X> {
    pub(crate) async fn compile(gx: &GXHandle<X>, r: Ref<X>) -> Result<Self> {
        let vals = match r.last.as_ref() {
            None => vec![],
            Some(v) => v.clone().cast_to::<Vec<Value>>()?,
        };
        let futs = vals.iter().map(|v| compile(gx.clone(), v.clone()));
        let ws = futures::future::try_join_all(futs).await?;
        Ok(Self { r, ws, vals })
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
            self.r.last = Some(v.clone());
            let vals = v.clone().cast_to::<Vec<Value>>()?;
            let mut kept = keep_equal(self.vals.drain(..).zip(self.ws.drain(..)), &vals);
            let fresh = vals
                .iter()
                .zip(kept.iter())
                .filter(|(_, w)| w.is_none())
                .map(|(val, _)| compile(gx.clone(), val.clone()))
                .collect::<Vec<_>>();
            let mut fresh = match fresh.is_empty() {
                true => vec![].into_iter(),
                false => rt.block_on(futures::future::try_join_all(fresh))?.into_iter(),
            };
            self.ws.extend(kept.drain(..).filter_map(|w| w.or_else(|| fresh.next())));
            self.vals = vals;
            changed = true;
        }
        for w in &mut self.ws {
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
) -> Vec<Option<T>> {
    let mut old: Vec<Option<(Value, T)>> = old.into_iter().map(Some).collect();
    new.iter()
        .enumerate()
        .map(|(i, val)| {
            let same = |o: &Option<(Value, T)>| o.as_ref().is_some_and(|(v, _)| v == val);
            let at = match old.get(i) {
                Some(o) if same(o) => Some(i),
                _ => old.iter().position(same),
            };
            at.and_then(|j| old[j].take()).map(|(_, t)| t)
        })
        .collect()
}

/// Compile `items` into those of `old` made from equal values, and the
/// rest all at once.
pub(crate) async fn reconcile<T, F: Future<Output = Result<T>>>(
    old: impl IntoIterator<Item = (Value, T)>,
    items: Vec<Value>,
    compile: impl Fn(Value) -> F,
) -> Result<Vec<(Value, T)>> {
    let kept = keep_equal(old, &items);
    let fresh = items
        .iter()
        .zip(kept.iter())
        .filter(|(_, t)| t.is_none())
        .map(|(v, _)| compile(v.clone()));
    let mut fresh = futures::future::try_join_all(fresh).await?.into_iter();
    Ok(items
        .into_iter()
        .zip(kept)
        .filter_map(|(v, t)| Some((v, t.or_else(|| fresh.next())?)))
        .collect())
}

/// The width `text` lays out to at `size` in `font`, unwrapped.
pub(crate) fn measure_text(text: &str, size: f32, font: iced_core::Font) -> f32 {
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
    pub(crate) async fn compile(gx: &GXHandle<X>, r: Ref<X>) -> Result<Self> {
        let mut f = None;
        if let Some(v) = r.last.as_ref() {
            set_callable(gx, &mut f, v).await?;
        }
        Ok(Self { r, f })
    }

    /// Take `v` when it is this handler's ref's. Returns the callable it
    /// replaced, for a caller that must outlive its id's last use.
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
    ($name:ident, $label:literal, $align_ty:ty, $align:ident, $Widget:ident, $align_set:ident,
     [$($f:ident),+]) => {
        pub(crate) struct $name<X: GXExt> {
            gx: GXHandle<X>,
            spacing: graphix_rt::TRef<X, f64>,
            padding: graphix_rt::TRef<X, PaddingV>,
            width: graphix_rt::TRef<X, LengthV>,
            height: graphix_rt::TRef<X, LengthV>,
            $align: graphix_rt::TRef<X, $align_ty>,
            children: Children<X>,
        }

        impl<X: GXExt> $name<X> {
            pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
                #[derive(FromValue)]
                struct Fields {
                    children: u64,
                    $($f: u64),+
                }
                let fields: Fields = source.cast_to().context(concat!($label, " flds"))?;
                let (children, spacing, padding, width, height, align) = tokio::try_join!(
                    gx.compile_ref(fields.children),
                    gx.compile_ref(fields.spacing),
                    gx.compile_ref(fields.padding),
                    gx.compile_ref(fields.width),
                    gx.compile_ref(fields.height),
                    gx.compile_ref(fields.$align),
                )?;
                Ok(Box::new(Self {
                    children: Children::compile(&gx, children).await
                        .context(concat!($label, " children"))?,
                    gx,
                    spacing: graphix_rt::TRef::new(spacing)
                        .context(concat!($label, " tref spacing"))?,
                    padding: graphix_rt::TRef::new(padding)
                        .context(concat!($label, " tref padding"))?,
                    width: graphix_rt::TRef::new(width)
                        .context(concat!($label, " tref width"))?,
                    height: graphix_rt::TRef::new(height)
                        .context(concat!($label, " tref height"))?,
                    $align: graphix_rt::TRef::new(align)
                        .context(concat!($label, " tref ", stringify!($align)))?,
                }))
            }
        }

        impl<X: GXExt> GuiWidget<X> for $name<X> {
            fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut GuiW<X>)) {
                self.children.ws.iter_mut().for_each(f)
            }

            fn for_each_child(&self, f: &mut dyn FnMut(&GuiW<X>)) {
                self.children.ws.iter().for_each(f)
            }

            fn handle_update(
                &mut self,
                rt: &tokio::runtime::Handle,
                id: ExprId,
                v: &Value,
            ) -> Result<bool> {
                let mut changed = false;
                changed |= self.spacing.update(id, v)
                    .context(concat!($label, " update spacing"))?.is_some();
                changed |= self.padding.update(id, v)
                    .context(concat!($label, " update padding"))?.is_some();
                changed |= self.width.update(id, v)
                    .context(concat!($label, " update width"))?.is_some();
                changed |= self.height.update(id, v)
                    .context(concat!($label, " update height"))?.is_some();
                changed |= self.$align.update(id, v)
                    .context(concat!($label, " update ", stringify!($align)))?.is_some();
                changed |= self.children.update(rt, &self.gx, id, v)
                    .context(concat!($label, " children"))?;
                Ok(changed)
            }

            fn view(&self) -> IcedElement<'_> {
                let mut w = iced_widget::$Widget::new();
                if let Some(sp) = self.spacing.t {
                    w = w.spacing(sp as f32);
                }
                if let Some(p) = self.padding.t.as_ref() {
                    w = w.padding(p.0);
                }
                if let Some(wi) = self.width.t.as_ref() {
                    w = w.width(wi.0);
                }
                if let Some(h) = self.height.t.as_ref() {
                    w = w.height(h.0);
                }
                if let Some(a) = self.$align.t.as_ref() {
                    w = w.$align_set(a.0);
                }
                for child in &self.children.ws {
                    w = w.push(child.view());
                }
                w.into()
            }
        }
    };
}

flex_widget!(
    RowW,
    "row",
    VAlignV,
    valign,
    Row,
    align_y,
    [height, padding, spacing, valign, width]
);

flex_widget!(
    ColumnW,
    "column",
    HAlignV,
    halign,
    Column,
    align_x,
    [halign, height, padding, spacing, width]
);

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
    let items = v.cast_to::<SmallVec<[Value; 8]>>()?;
    let futs: Vec<_> = items.into_iter().map(|item| compile(gx.clone(), item)).collect();
    futures::future::try_join_all(futs).await
}
