use super::{GuiW, Handler, IcedElement, Message};
use crate::types::{LengthV, TextSizeV};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle, TRef};
use iced_widget as widget;
use netidx::{protocol::valarray::ValArray, publisher::Value};
use netidx_derive::FromValue;
use tokio::try_join;

// TODO: link hover underlines flicker between adjacent link spans;
// upstream bug in `iced_widget::text::rich::update` (no redraw when
// the hovered link index changes).

pub(crate) struct MarkdownW<X: GXExt> {
    gx: GXHandle<X>,
    content: TRef<X, ArcStr>,
    on_link: Handler<X>,
    spacing: TRef<X, Option<f64>>,
    text_size: TRef<X, Option<TextSizeV>>,
    width: TRef<X, LengthV>,
    items: Vec<widget::markdown::Item>,
}

impl<X: GXExt> MarkdownW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        #[derive(FromValue)]
        struct Fields {
            content: u64,
            on_link: u64,
            spacing: u64,
            text_size: u64,
            width: u64,
        }
        let Fields { content, on_link, spacing, text_size, width } =
            source.cast_to().context("markdown flds")?;
        let (content_ref, on_link, spacing, text_size, width) = try_join! {
            gx.compile_ref(content),
            gx.compile_ref(on_link),
            gx.compile_ref(spacing),
            gx.compile_ref(text_size),
            gx.compile_ref(width),
        }?;
        let on_link = Handler::compile(&gx, on_link).await.context("markdown on_link")?;
        let content = TRef::new(content_ref).context("markdown tref content")?;
        let items = match content.t.as_deref() {
            Some(s) => widget::markdown::parse(s).collect(),
            None => vec![],
        };
        Ok(Box::new(Self {
            gx: gx.clone(),
            content,
            on_link,
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
        changed |=
            self.text_size.update(id, v).context("markdown update text_size")?.is_some();
        changed |= self.width.update(id, v).context("markdown update width")?.is_some();
        self.on_link.update(rt, &self.gx, id, v).context("markdown on_link recompile")?;
        Ok(changed)
    }

    fn view(&self) -> IcedElement<'_> {
        let text_size = self.text_size.t.flatten().map_or(16.0, |s| s.0);
        let palette = crate::theme::view_theme().palette();
        let mut settings = widget::markdown::Settings::with_text_size(
            text_size,
            widget::markdown::Style::from_palette(palette),
        );
        if let Some(Some(sp)) = self.spacing.t.filter(|s| s.is_some_and(f64::is_finite)) {
            settings.spacing = (sp as f32).into();
        }
        let on_link_id = self.on_link.id();
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
