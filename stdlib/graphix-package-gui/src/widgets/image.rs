use super::{GuiW, GuiWidget, IcedElement};
use crate::types::{ContentFitV, ImageSourceV, LengthV};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle, TRef};
use iced_widget as widget;
use netidx::publisher::Value;
use tokio::try_join;

pub(crate) struct ImageW<X: GXExt> {
    source: TRef<X, ImageSourceV>,
    handle: Option<iced_core::image::Handle>,
    width: TRef<X, LengthV>,
    height: TRef<X, LengthV>,
    content_fit: TRef<X, ContentFitV>,
}

impl<X: GXExt> ImageW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let [(_, content_fit), (_, height), (_, src), (_, width)] =
            source.cast_to::<[(ArcStr, u64); 4]>().context("image flds")?;
        let (content_fit, height, src, width) = try_join! {
            gx.compile_ref(content_fit),
            gx.compile_ref(height),
            gx.compile_ref(src),
            gx.compile_ref(width),
        }?;
        let source = TRef::new(src).context("image tref source")?;
        let handle = source.t.as_ref().map(|s: &ImageSourceV| s.to_handle());
        Ok(Box::new(Self {
            source,
            handle,
            width: TRef::new(width).context("image tref width")?,
            height: TRef::new(height).context("image tref height")?,
            content_fit: TRef::new(content_fit).context("image tref content_fit")?,
        }))
    }
}

impl<X: GXExt> GuiWidget<X> for ImageW<X> {
    fn handle_update(
        &mut self,
        _rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let mut changed = false;
        if self.source.update(id, v).context("image update source")?.is_some() {
            self.handle = self.source.t.as_ref().map(|s| s.to_handle());
            changed = true;
        }
        changed |= self.width.update(id, v).context("image update width")?.is_some();
        changed |= self.height.update(id, v).context("image update height")?.is_some();
        changed |=
            self.content_fit.update(id, v).context("image update content_fit")?.is_some();
        Ok(changed)
    }

    fn view(&self) -> IcedElement<'_> {
        let handle = match &self.handle {
            Some(h) => h.clone(),
            None => iced_core::image::Handle::from_path(""),
        };
        let mut img = widget::Image::new(handle);
        if let Some(w) = self.width.t.as_ref() {
            img = img.width(w.0);
        }
        if let Some(h) = self.height.t.as_ref() {
            img = img.height(h.0);
        }
        if let Some(cf) = self.content_fit.t.as_ref() {
            img = img.content_fit(cf.0);
        }
        img.into()
    }
}
