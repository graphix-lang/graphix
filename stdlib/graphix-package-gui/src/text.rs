use crate::{
    types::{ColorV, FontV, HAlignV, LengthV, VAlignV},
    GuiW, IcedElement,
};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle, TRef};
use iced_widget as widget;
use netidx::publisher::Value;
use tokio::try_join;

pub(crate) struct TextW<X: GXExt> {
    content: TRef<X, String>,
    size: TRef<X, Option<f64>>,
    color: TRef<X, Option<ColorV>>,
    font: TRef<X, Option<FontV>>,
    width: TRef<X, LengthV>,
    height: TRef<X, LengthV>,
    halign: TRef<X, HAlignV>,
    valign: TRef<X, VAlignV>,
}

impl<X: GXExt> TextW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let [
            (_, color), (_, content), (_, font), (_, halign),
            (_, height), (_, size), (_, valign), (_, width),
        ] = source.cast_to::<[(ArcStr, u64); 8]>().context("text flds")?;
        let (color, content, font, halign, height, size, valign, width) = try_join! {
            gx.compile_ref(color),
            gx.compile_ref(content),
            gx.compile_ref(font),
            gx.compile_ref(halign),
            gx.compile_ref(height),
            gx.compile_ref(size),
            gx.compile_ref(valign),
            gx.compile_ref(width),
        }?;
        Ok(Box::new(Self {
            content: TRef::new(content).context("text tref content")?,
            size: TRef::new(size).context("text tref size")?,
            color: TRef::new(color).context("text tref color")?,
            font: TRef::new(font).context("text tref font")?,
            width: TRef::new(width).context("text tref width")?,
            height: TRef::new(height).context("text tref height")?,
            halign: TRef::new(halign).context("text tref halign")?,
            valign: TRef::new(valign).context("text tref valign")?,
        }))
    }
}

impl<X: GXExt> crate::GuiWidget<X> for TextW<X> {
    fn handle_update(&mut self, id: ExprId, v: &Value) -> Result<()> {
        self.content.update(id, v).context("text update content")?;
        self.size.update(id, v).context("text update size")?;
        self.color.update(id, v).context("text update color")?;
        self.font.update(id, v).context("text update font")?;
        self.width.update(id, v).context("text update width")?;
        self.height.update(id, v).context("text update height")?;
        self.halign.update(id, v).context("text update halign")?;
        self.valign.update(id, v).context("text update valign")?;
        Ok(())
    }

    fn view(&self) -> IcedElement<'_> {
        let content = self.content.t.as_deref().unwrap_or("");
        let mut t = widget::Text::new(content);
        if let Some(Some(sz)) = self.size.t {
            t = t.size(sz as f32);
        }
        if let Some(Some(c)) = self.color.t.as_ref() {
            t = t.color(c.0);
        }
        if let Some(Some(f)) = self.font.t.as_ref() {
            t = t.font(f.0);
        }
        if let Some(w) = self.width.t.as_ref() {
            t = t.width(w.0);
        }
        if let Some(h) = self.height.t.as_ref() {
            t = t.height(h.0);
        }
        if let Some(a) = self.halign.t.as_ref() {
            t = t.align_x(a.0);
        }
        if let Some(a) = self.valign.t.as_ref() {
            t = t.align_y(a.0);
        }
        t.into()
    }
}
