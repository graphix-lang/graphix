use anyhow::{bail, Context, Result};
use arcstr::ArcStr;
use graphix_compiler::expr::ExprId;
use graphix_rt::{CallableId, GXExt, GXHandle};
use netidx::{protocol::valarray::ValArray, publisher::Value};
use smallvec::SmallVec;
use std::{future::Future, pin::Pin};

use crate::types::{HAlignV, LengthV, PaddingV, VAlignV};

/// Compile an optional callable ref during widget construction.
macro_rules! compile_callable {
    ($gx:expr, $ref:ident, $label:expr) => {
        match $ref.last.as_ref() {
            Some(v) => Some($gx.compile_callable(v.clone()).await.context($label)?),
            None => None,
        }
    };
}

/// Recompile a callable ref inside `handle_update`.
macro_rules! update_callable {
    ($self:ident, $rt:ident, $id:ident, $v:ident, $field:ident, $callable:ident, $label:expr) => {
        if $id == $self.$field.id {
            $self.$field.last = Some($v.clone());
            $self.$callable = Some(
                $rt.block_on($self.gx.compile_callable($v.clone()))
                    .context($label)?,
            );
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
            $self.$child = $rt
                .block_on(compile($self.gx.clone(), $v.clone()))
                .context($label)?;
            $changed = true;
        }
        $changed |= $self.$child.handle_update($rt, $id, $v)?;
    };
}

pub(crate) mod button;
pub(crate) mod canvas;
pub(crate) mod chart;
pub(crate) mod combo_box;
pub(crate) mod container;
pub(crate) mod grid;
pub(crate) mod iced_keyboard_area;
pub(crate) mod image;
pub(crate) mod keyboard_area;
pub(crate) mod markdown;
pub(crate) mod menu_bar;
pub(crate) mod menu_bar_widget;
pub(crate) mod mouse_area;
pub(crate) mod pick_list;
pub(crate) mod progress_bar;
pub(crate) mod qr_code;
pub(crate) mod radio;
pub(crate) mod rule;
pub(crate) mod scrollable;
pub(crate) mod slider;
pub(crate) mod space;
pub(crate) mod stack;
pub(crate) mod svg;
pub(crate) mod table;
pub(crate) mod text;
pub(crate) mod text_editor;
pub(crate) mod text_input;
pub(crate) mod toggle;
pub(crate) mod tooltip;

/// Concrete iced renderer type used throughout the GUI package.
/// Must match iced_widget's default Renderer parameter.
pub(crate) type Renderer = iced_renderer::Renderer;

/// Concrete iced Element type with our Message/Theme/Renderer.
pub(crate) type IcedElement<'a> =
    iced_core::Element<'a, Message, crate::theme::GraphixTheme, Renderer>;

/// Message type for iced widget interactions.
#[derive(Debug, Clone)]
pub(crate) enum Message {
    Nop,
    Call(CallableId, ValArray),
    EditorAction(ExprId, iced_widget::text_editor::Action),
}

/// Trait for GUI widgets. Unlike TUI widgets, GUI widgets are not
/// async — handle_update is synchronous, and the view method builds
/// an iced Element tree.
pub(crate) trait GuiWidget<X: GXExt>: Send + 'static {
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

    /// Route a text editor action to the widget that owns the given
    /// content ref. Returns `Some((callable_id, value))` if the action
    /// was an edit and the result should be called back to graphix.
    fn editor_action(
        &mut self,
        id: ExprId,
        action: &iced_widget::text_editor::Action,
    ) -> Option<(CallableId, Value)> {
        let _ = (id, action);
        None
    }
}

pub(crate) type GuiW<X> = Box<dyn GuiWidget<X>>;

/// Future type for widget compilation (avoids infinite-size async fn).
pub(crate) type CompileFut<X> =
    Pin<Box<dyn Future<Output = Result<GuiW<X>>> + Send + 'static>>;

/// Empty widget placeholder.
pub(crate) struct EmptyW;

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

/// Generate a flex layout widget (Row or Column). All parameters use
/// call-site tokens to satisfy macro hygiene for local variable names.
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
                let [(_, children), $((_, $f)),+] =
                    source.cast_to::<[(ArcStr, u64); 6]>()
                        .context(concat!($label, " flds"))?;
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

            fn editor_action(
                &mut self,
                id: ExprId,
                action: &iced_widget::text_editor::Action,
            ) -> Option<(CallableId, Value)> {
                for child in &mut self.children {
                    if let some @ Some(_) = child.editor_action(id, action) {
                        return some;
                    }
                }
                None
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
pub(crate) fn compile<X: GXExt>(gx: GXHandle<X>, source: Value) -> CompileFut<X> {
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
            "Svg" => svg::SvgW::compile(gx, v).await,
            "Canvas" => canvas::CanvasW::compile(gx, v).await,
            "Chart" => chart::ChartW::compile(gx, v).await,
            "Markdown" => markdown::MarkdownW::compile(gx, v).await,
            "MenuBar" => menu_bar::MenuBarW::compile(gx, v).await,
            "QrCode" => qr_code::QrCodeW::compile(gx, v).await,
            "Table" => table::TableW::compile(gx, v).await,
            _ => bail!("invalid gui widget type `{s}({v})"),
        }
    })
}

/// Compile an array of widget values into a Vec of GuiW.
pub(crate) async fn compile_children<X: GXExt>(
    gx: GXHandle<X>,
    v: Value,
) -> Result<Vec<GuiW<X>>> {
    let items = v.cast_to::<SmallVec<[Value; 8]>>()?;
    let futs: Vec<_> = items.into_iter().map(|item| compile(gx.clone(), item)).collect();
    futures::future::try_join_all(futs).await
}
