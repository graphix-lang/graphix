use super::{GuiW, IcedElement, Message};
use crate::types::LengthV;
use anyhow::{Context, Result};
use arcstr::ArcStr;
use graphix_compiler::expr::ExprId;
use graphix_rt::{Callable, GXExt, GXHandle, Ref, TRef};
use iced_widget as widget;
use netidx::{protocol::valarray::ValArray, publisher::Value};
use tokio::try_join;

pub(crate) struct MarkdownW<X: GXExt> {
    gx: GXHandle<X>,
    content: TRef<X, String>,
    on_link: Ref<X>,
    on_link_callable: Option<Callable<X>>,
    spacing: TRef<X, Option<f64>>,
    text_size: TRef<X, Option<f64>>,
    width: TRef<X, LengthV>,
    items: Vec<widget::markdown::Item>,
}

impl<X: GXExt> MarkdownW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let [(_, content), (_, on_link), (_, spacing), (_, text_size), (_, width)] =
            source.cast_to::<[(ArcStr, u64); 5]>().context("markdown flds")?;
        let (content_ref, on_link, spacing, text_size, width) = try_join! {
            gx.compile_ref(content),
            gx.compile_ref(on_link),
            gx.compile_ref(spacing),
            gx.compile_ref(text_size),
            gx.compile_ref(width),
        }?;
        let callable = compile_callable!(gx, on_link, "markdown on_link");
        let content = TRef::new(content_ref).context("markdown tref content")?;
        let items = match content.t.as_deref() {
            Some(s) => widget::markdown::parse(s).collect(),
            None => vec![],
        };
        Ok(Box::new(Self {
            gx: gx.clone(),
            content,
            on_link,
            on_link_callable: callable,
            spacing: TRef::new(spacing).context("markdown tref spacing")?,
            text_size: TRef::new(text_size).context("markdown tref text_size")?,
            width: TRef::new(width).context("markdown tref width")?,
            items,
        }))
    }
}

impl<X: GXExt> super::GuiWidget<X> for MarkdownW<X> {
    fn handle_update(
        &mut self,
        rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let mut changed = false;
        if let Some(_) = self.content.update(id, v).context("markdown update content")? {
            self.items = match self.content.t.as_deref() {
                Some(s) => widget::markdown::parse(s).collect(),
                None => vec![],
            };
            changed = true;
        }
        changed |=
            self.spacing.update(id, v).context("markdown update spacing")?.is_some();
        changed |= self
            .text_size
            .update(id, v)
            .context("markdown update text_size")?
            .is_some();
        changed |= self.width.update(id, v).context("markdown update width")?.is_some();
        update_callable!(
            self, rt, id, v, on_link, on_link_callable, "markdown on_link recompile"
        );
        Ok(changed)
    }

    fn view(&self) -> IcedElement<'_> {
        let text_size = self.text_size.t.flatten().unwrap_or(16.0) as f32;
        let settings =
            widget::markdown::Settings::with_text_size(text_size, iced_core::Theme::Dark);
        let on_link_id = self.on_link_callable.as_ref().map(|c| c.id());
        let md: iced_core::Element<'_, widget::markdown::Uri, _, _> =
            widget::markdown::view(&self.items, settings);
        let element = md.map(move |uri| match on_link_id {
            Some(id) => {
                Message::Call(id, ValArray::from_iter([Value::String(uri.into())]))
            }
            None => Message::Nop,
        });
        let mut container = iced_widget::Container::new(element);
        if let Some(w) = self.width.t.as_ref() {
            container = container.width(w.0);
        }
        container.into()
    }
}
