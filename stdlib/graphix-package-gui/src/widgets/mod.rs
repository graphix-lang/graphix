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

/// Compile an optional callable ref during widget construction.
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
// CR claude for claude: [structure] These four macros each pair a Ref with state derived
// from it, and the pairing is still copied by hand. menu_bar.rs:62-69 repeats
// compile_callable!, data_table/mod.rs:133-140 repeats it with null meaning no handler,
// data_table's update_cb! (mod.rs:347-355) skips update_callable's is_for check and so
// recompiles a handler on every fire, and children have no helper (flex_widget! below,
// grid.rs:40-45 and 78-87, stack.rs:34-39 and 68-74). Each of those sites also writes
// out Ref::update by hand as `if id == r.id { r.last = Some(v.clone()); .. }`. Small
// owning types (a handler: Ref + Option<Callable>, a child: Ref + GuiW, children: Ref +
// Vec<GuiW>) with compile and update methods built on Ref::update would replace the
// macros and the copies, so a fix such as a same-value check is made once.
// flex_widget!'s spacing, padding, width and height idents are the same in both
// expansions and can be fixed field names. (gui-widgets-a-09)
macro_rules! compile_callable {
    ($gx:expr, $ref:ident, $label:expr) => {{
        let mut c = None;
        if let Some(v) = $ref.last.as_ref() {
            $crate::widgets::set_callable(&$gx, &mut c, v).await.context($label)?;
        }
        c
    }};
}

/// Recompile a callable ref inside `handle_update`.
macro_rules! update_callable {
    ($self:ident, $rt:ident, $id:ident, $v:ident, $field:ident, $callable:ident, $label:expr) => {
        if $id == $self.$field.id {
            $self.$field.last = Some($v.clone());
            $crate::widgets::update_callable_blocking(
                $rt,
                &$self.gx,
                &mut $self.$callable,
                $v,
            )
            .context($label)?;
        }
    };
}

/// Compile a child widget ref during widget construction.
macro_rules! compile_child {
    ($gx:expr, $ref:ident, $label:expr) => {
        match $ref.last.as_ref() {
            None => Box::new(super::EmptyW) as GuiW<X>,
            Some(v) => compile($gx.clone(), v.clone()).await.context($label)?,
        }
    };
}

/// Recompile a child widget ref inside `handle_update`.
/// Sets `$changed = true` when the child is recompiled or updated.
macro_rules! update_child {
    ($self:ident, $rt:ident, $id:ident, $v:ident, $changed:ident, $ref:ident, $child:ident, $label:expr) => {
        if $id == $self.$ref.id {
            $self.$ref.last = Some($v.clone());
            $self.$child =
                $rt.block_on(compile($self.gx.clone(), $v.clone())).context($label)?;
            $changed = true;
        }
        $changed |= $self.$child.handle_update($rt, $id, $v)?;
    };
}

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
    ($name:ident, $label:literal,
     $spacing:ident, $padding:ident, $width:ident, $height:ident,
     $align_ty:ty, $align:ident, $Widget:ident, $align_set:ident,
     [$($f:ident),+]) => {
        pub(crate) struct $name<X: GXExt> {
            gx: GXHandle<X>,
            $spacing: graphix_rt::TRef<X, f64>,
            $padding: graphix_rt::TRef<X, PaddingV>,
            $width: graphix_rt::TRef<X, LengthV>,
            $height: graphix_rt::TRef<X, LengthV>,
            $align: graphix_rt::TRef<X, $align_ty>,
            children_ref: graphix_rt::Ref<X>,
            children: Vec<GuiW<X>>,
        }

        impl<X: GXExt> $name<X> {
            pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
                #[derive(FromValue)]
                struct Fields {
                    children: u64,
                    $($f: u64),+
                }
                let Fields { children, $($f),+ } =
                    source.cast_to().context(concat!($label, " flds"))?;
                let (children_ref, $($f),+) = tokio::try_join!(
                    gx.compile_ref(children),
                    $(gx.compile_ref($f)),+
                )?;
                let compiled_children = match children_ref.last.as_ref() {
                    None => vec![],
                    Some(v) => compile_children(gx.clone(), v.clone()).await
                        .context(concat!($label, " children"))?,
                };
                Ok(Box::new(Self {
                    gx: gx.clone(),
                    $spacing: graphix_rt::TRef::new($spacing)
                        .context(concat!($label, " tref spacing"))?,
                    $padding: graphix_rt::TRef::new($padding)
                        .context(concat!($label, " tref padding"))?,
                    $width: graphix_rt::TRef::new($width)
                        .context(concat!($label, " tref width"))?,
                    $height: graphix_rt::TRef::new($height)
                        .context(concat!($label, " tref height"))?,
                    $align: graphix_rt::TRef::new($align)
                        .context(concat!($label, " tref ", stringify!($align)))?,
                    children_ref,
                    children: compiled_children,
                }))
            }
        }

        impl<X: GXExt> GuiWidget<X> for $name<X> {
            fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut GuiW<X>)) {
                self.children.iter_mut().for_each(f)
            }

            fn for_each_child(&self, f: &mut dyn FnMut(&GuiW<X>)) {
                self.children.iter().for_each(f)
            }

            fn handle_update(
                &mut self,
                rt: &tokio::runtime::Handle,
                id: ExprId,
                v: &Value,
            ) -> Result<bool> {
                let mut changed = false;
                changed |= self.$spacing.update(id, v)
                    .context(concat!($label, " update spacing"))?.is_some();
                changed |= self.$padding.update(id, v)
                    .context(concat!($label, " update padding"))?.is_some();
                changed |= self.$width.update(id, v)
                    .context(concat!($label, " update width"))?.is_some();
                changed |= self.$height.update(id, v)
                    .context(concat!($label, " update height"))?.is_some();
                changed |= self.$align.update(id, v)
                    .context(concat!($label, " update ", stringify!($align)))?.is_some();
                // CR claude for claude: [bug] This recompiles every child whenever the
                // children ref fires, even when the delivered array equals the one
                // already compiled. A place reference re-fires its value on every write
                // to its root (graphix-compiler/src/node/bind.rs:977), so
                // `column(&[text_editor(#on_edit: .., &doc.text), ..])` rebuilds the
                // editor on each write to `doc`. The new Content puts the cursor at (0,
                // 0), so typing "abc" leaves "cba". update_child! (:49), stack, grid,
                // table, menu_bar, context_menu and window.rs:147/201 rebuild the same
                // way (a `window(#title: &doc.title, ..)` rebuilds the whole window).
                // Skip the recompile when the ref's `last` equals the delivered value.
                // probe: design/review-2026-10-05/repro/gui-widgets-b-01.rs (copy it
                // under stdlib/graphix-package-gui/tests/ and run cargo test -p
                // graphix-package-gui --test review_gui_widgets_b_01).
                // (gui-widgets-b-01)
                // CR claude for claude: [bug] This recompiles every child whenever the
                // children ref fires, even when the new array equals
                // `children_ref.last` or only a sibling changed. `select page` re-emits
                // the identical array when Home is clicked on Home, and appending one
                // row to N rows rebuilds all N. The rebuild throws away state held in
                // the widget structs (a text_editor's cursor and selection restart at
                // 0). It also gives every handler in the subtree a fresh call site,
                // which `update_callable` exists to prevent. `update_child!` (line 49),
                // grid.rs, stack.rs and window.rs's window_ref and content_ref arms
                // have the same shape. probe:
                // design/review-2026-10-05/repro/gui-widgets-a-04.rs (typing Y after
                // the re-fire gives "Yabchello", expected "abcYhello"; 0 of 20 old
                // editors survive an append). (gui-widgets-a-04)
                if id == self.children_ref.id {
                    self.children_ref.last = Some(v.clone());
                    self.children = rt.block_on(
                        compile_children(self.gx.clone(), v.clone())
                    ).context(concat!($label, " children recompile"))?;
                    changed = true;
                }
                for child in &mut self.children {
                    changed |= child.handle_update(rt, id, v)?;
                }
                Ok(changed)
            }

            fn view(&self) -> IcedElement<'_> {
                let mut w = iced_widget::$Widget::new();
                if let Some(sp) = self.$spacing.t {
                    w = w.spacing(sp as f32);
                }
                if let Some(p) = self.$padding.t.as_ref() {
                    w = w.padding(p.0);
                }
                if let Some(wi) = self.$width.t.as_ref() {
                    w = w.width(wi.0);
                }
                if let Some(h) = self.$height.t.as_ref() {
                    w = w.height(h.0);
                }
                if let Some(a) = self.$align.t.as_ref() {
                    w = w.$align_set(a.0);
                }
                for child in &self.children {
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
    spacing,
    padding,
    width,
    height,
    VAlignV,
    valign,
    Row,
    align_y,
    [height, padding, spacing, valign, width]
);

flex_widget!(
    ColumnW,
    "column",
    spacing,
    padding,
    width,
    height,
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
