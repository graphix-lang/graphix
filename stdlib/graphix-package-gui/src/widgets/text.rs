use super::{GuiW, IcedElement};
use crate::types::{ColorV, FontV, HAlignV, LengthV, TextSizeV, VAlignV};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle};
use iced_widget as widget;
use netidx::publisher::Value;

graphix_rt::props! {
    struct Props {
        color: Option<ColorV>,
        content: ArcStr,
        font: Option<FontV>,
        halign: HAlignV,
        height: LengthV,
        size: Option<TextSizeV>,
        valign: VAlignV,
        width: LengthV,
    }
}

pub(crate) struct TextW<X: GXExt>(Props<X>);

impl<X: GXExt> TextW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        Ok(Box::new(Self(Props::compile(&gx, &source).await.context("text")?)))
    }
}

impl<X: GXExt> super::GuiWidget<X> for TextW<X> {
    fn handle_update(
        &mut self,
        _rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        self.0.update(id, v).context("text")
    }

    fn view(&self) -> IcedElement<'_> {
        let content = self.0.content.t.as_deref().unwrap_or("");
        let mut t = widget::Text::new(content);
        if let Some(Some(sz)) = self.0.size.t {
            t = t.size(sz.0);
        }
        if let Some(Some(c)) = self.0.color.t.as_ref() {
            t = t.color(c.0);
        }
        if let Some(Some(f)) = self.0.font.t.as_ref() {
            t = t.font(f.0);
        }
        if let Some(w) = self.0.width.t.as_ref() {
            t = t.width(w.0);
        }
        if let Some(h) = self.0.height.t.as_ref() {
            t = t.height(h.0);
        }
        if let Some(a) = self.0.halign.t.as_ref() {
            t = t.align_x(a.0);
        }
        if let Some(a) = self.0.valign.t.as_ref() {
            t = t.align_y(a.0);
        }
        t.into()
    }
}
