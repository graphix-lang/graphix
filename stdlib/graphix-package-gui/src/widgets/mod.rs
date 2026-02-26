use anyhow::{bail, Context, Result};
use arcstr::ArcStr;
use graphix_compiler::{expr::ExprId, BindId};
use graphix_rt::{CallableId, GXExt, GXHandle};
use netidx::publisher::Value;
use smallvec::SmallVec;
use std::{future::Future, pin::Pin};

use crate::types::{HAlignV, LengthV, PaddingV, VAlignV};

pub(crate) mod button;
pub(crate) mod canvas;
pub(crate) mod chart;
pub(crate) mod checkbox;
pub(crate) mod combo_box;
pub(crate) mod container;
pub(crate) mod image;
pub(crate) mod mouse_area;
pub(crate) mod pick_list;
pub(crate) mod plotters_backend;
pub(crate) mod progress_bar;
pub(crate) mod radio;
pub(crate) mod rule;
pub(crate) mod scrollable;
pub(crate) mod slider;
pub(crate) mod space;
pub(crate) mod stack;
pub(crate) mod svg;
pub(crate) mod text;
pub(crate) mod text_editor;
pub(crate) mod text_input;
pub(crate) mod toggler;
pub(crate) mod tooltip;
pub(crate) mod vertical_slider;

/// Concrete iced renderer type used throughout the GUI package.
/// Must match iced_widget's default Renderer parameter.
pub(crate) type Renderer = iced_renderer::Renderer;

/// Concrete iced Element type with our Message/Theme/Renderer.
pub(crate) type IcedElement<'a> =
    iced_core::Element<'a, Message, iced_core::Theme, Renderer>;

/// Message type for iced widget interactions.
#[derive(Debug, Clone)]
pub(crate) enum Message {
    Nop,
    Set(BindId, Value),
    Call(CallableId),
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
    /// content ref. Returns `Some((bid, value))` if the action was an
    /// edit and the result should be pushed to graphix.
    fn editor_action(
        &mut self,
        id: ExprId,
        action: &iced_widget::text_editor::Action,
    ) -> Option<(BindId, Value)> {
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
            ) -> Option<(BindId, Value)> {
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
        match source.cast_to::<(ArcStr, Value)>()? {
            (s, v) if &s == "Text" => text::TextW::compile(gx, v).await,
            (s, v) if &s == "Column" => ColumnW::compile(gx, v).await,
            (s, v) if &s == "Row" => RowW::compile(gx, v).await,
            (s, v) if &s == "Container" => container::ContainerW::compile(gx, v).await,
            (s, v) if &s == "Button" => button::ButtonW::compile(gx, v).await,
            (s, v) if &s == "Space" => space::SpaceW::compile(gx, v).await,
            (s, v) if &s == "TextInput" => text_input::TextInputW::compile(gx, v).await,
            (s, v) if &s == "Checkbox" => checkbox::CheckboxW::compile(gx, v).await,
            (s, v) if &s == "Toggler" => toggler::TogglerW::compile(gx, v).await,
            (s, v) if &s == "Slider" => slider::SliderW::compile(gx, v).await,
            (s, v) if &s == "ProgressBar" => {
                progress_bar::ProgressBarW::compile(gx, v).await
            }
            (s, v) if &s == "Scrollable" => {
                scrollable::ScrollableW::compile(gx, v).await
            }
            (s, v) if &s == "HorizontalRule" => {
                rule::HorizontalRuleW::compile(gx, v).await
            }
            (s, v) if &s == "VerticalRule" => rule::VerticalRuleW::compile(gx, v).await,
            (s, v) if &s == "Tooltip" => tooltip::TooltipW::compile(gx, v).await,
            (s, v) if &s == "PickList" => pick_list::PickListW::compile(gx, v).await,
            (s, v) if &s == "Stack" => stack::StackW::compile(gx, v).await,
            (s, v) if &s == "Radio" => radio::RadioW::compile(gx, v).await,
            (s, v) if &s == "VerticalSlider" => {
                vertical_slider::VerticalSliderW::compile(gx, v).await
            }
            (s, v) if &s == "ComboBox" => combo_box::ComboBoxW::compile(gx, v).await,
            (s, v) if &s == "TextEditor" => {
                text_editor::TextEditorW::compile(gx, v).await
            }
            (s, v) if &s == "MouseArea" => mouse_area::MouseAreaW::compile(gx, v).await,
            (s, v) if &s == "Image" => image::ImageW::compile(gx, v).await,
            (s, v) if &s == "Svg" => svg::SvgW::compile(gx, v).await,
            (s, v) if &s == "Canvas" => canvas::CanvasW::compile(gx, v).await,
            (s, v) if &s == "Chart" => chart::ChartW::compile(gx, v).await,
            (s, v) => bail!("invalid gui widget type `{s}({v})"),
        }
    })
}

/// Compile an array of widget values into a Vec of GuiW.
pub(crate) async fn compile_children<X: GXExt>(
    gx: GXHandle<X>,
    v: Value,
) -> Result<Vec<GuiW<X>>> {
    let items = v.cast_to::<SmallVec<[Value; 8]>>()?;
    let futs: Vec<_> =
        items.into_iter().map(|item| compile(gx.clone(), item)).collect();
    futures::future::try_join_all(futs).await
}
