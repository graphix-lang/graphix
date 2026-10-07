use super::{GuiW, Handler, IcedElement, Message};
use crate::types::{LengthV, TextSizeV};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use futures::try_join;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle};
use iced_widget as widget;
use netidx::{protocol::valarray::ValArray, publisher::Value};

// TODO: link hover underlines flicker between adjacent link spans;
// upstream bug in `iced_widget::text::rich::update` (no redraw when
// the hovered link index changes).

graphix_rt::props! {
    struct Props {
        content: ArcStr,
        spacing: Option<f64>,
        text_size: Option<TextSizeV>,
        width: LengthV,
    }
}

pub(crate) struct MarkdownW<X: GXExt> {
    gx: GXHandle<X>,
    p: Props<X>,
    on_link: Handler<X>,
    items: Vec<widget::markdown::Item>,
}

impl<X: GXExt> MarkdownW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let (p, on_link) = try_join!(
            Props::compile(&gx, &source),
            Handler::field(&gx, &source, "on_link"),
        )
        .context("markdown")?;
        let items = Self::parse(&p);
        Ok(Box::new(Self { gx, p, on_link, items }))
    }
}

impl<X: GXExt> MarkdownW<X> {
    fn parse(p: &Props<X>) -> Vec<widget::markdown::Item> {
        match p.content.t.as_deref() {
            Some(s) => widget::markdown::parse(s).collect(),
            None => vec![],
        }
    }
}

impl<X: GXExt> super::GuiWidget<X> for MarkdownW<X> {
    fn handle_update(
        &mut self,
        rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let changed = self.p.update(id, v).context("markdown")?;
        if id == self.p.content.r.id {
            self.items = Self::parse(&self.p);
        }
        self.on_link.update(rt, &self.gx, id, v).context("markdown on_link recompile")?;
        Ok(changed)
    }

    fn view(&self) -> IcedElement<'_> {
        let text_size = self.p.text_size.t.flatten().map_or(16.0, |s| s.0);
        let palette = crate::theme::view_theme().palette();
        let mut settings = widget::markdown::Settings::with_text_size(
            text_size,
            widget::markdown::Style::from_palette(palette),
        );
        if let Some(Some(sp)) = self.p.spacing.t.filter(|s| s.is_some_and(f64::is_finite))
        {
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
        if let Some(w) = self.p.width.t.as_ref() {
            container = container.width(w.0);
        }
        container.into()
    }
}
