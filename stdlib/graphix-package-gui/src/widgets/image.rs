use super::{GuiW, GuiWidget, IcedElement};
use crate::types::{ContentFitV, ImageSourceV, LengthV};
use anyhow::{Context, Result};
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle, TRef};
use iced_core::{image, svg};
use iced_widget as widget;
use netidx::publisher::Value;
use netidx_derive::FromValue;
use tokio::try_join;

/// What iced draws a source with.
enum ImageHandle {
    Raster(image::Handle),
    Svg(svg::Handle),
}

impl ImageHandle {
    fn of(source: &ImageSourceV) -> Self {
        match source {
            ImageSourceV::Path(p) if source.is_svg() => {
                Self::Svg(svg::Handle::from_path(p))
            }
            ImageSourceV::Path(p) => Self::Raster(image::Handle::from_path(p)),
            ImageSourceV::Svg(s) => {
                Self::Svg(svg::Handle::from_memory(s.as_bytes().to_vec()))
            }
            ImageSourceV::Bytes(b) => Self::Raster(image::Handle::from_bytes(b.clone())),
            ImageSourceV::Rgba { width, height, pixels } => {
                Self::Raster(image::Handle::from_rgba(*width, *height, pixels.clone()))
            }
        }
    }
}

pub(crate) struct ImageW<X: GXExt> {
    source: TRef<X, ImageSourceV>,
    handle: Option<ImageHandle>,
    width: TRef<X, LengthV>,
    height: TRef<X, LengthV>,
    content_fit: TRef<X, ContentFitV>,
}

impl<X: GXExt> ImageW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        #[derive(FromValue)]
        struct Fields {
            content_fit: u64,
            height: u64,
            source: u64,
            width: u64,
        }
        let Fields { content_fit, height, source: src, width } =
            source.cast_to().context("image flds")?;
        let (content_fit, height, src, width) = try_join! {
            gx.compile_ref(content_fit),
            gx.compile_ref(height),
            gx.compile_ref(src),
            gx.compile_ref(width),
        }?;
        let source = TRef::new(src).context("image tref source")?;
        let handle = source.t.as_ref().map(ImageHandle::of);
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
        let old = (id == self.source.r.id).then(|| self.source.t.clone()).flatten();
        if let Some(new) = self.source.update(id, v).context("image update source")?
            && !old.is_some_and(|o| o.same_as(new))
        {
            self.handle = Some(ImageHandle::of(new));
            changed = true;
        }
        changed |= self.width.update(id, v).context("image update width")?.is_some();
        changed |= self.height.update(id, v).context("image update height")?.is_some();
        changed |=
            self.content_fit.update(id, v).context("image update content_fit")?.is_some();
        Ok(changed)
    }

    fn view(&self) -> IcedElement<'_> {
        let width = self.width.t.as_ref().map(|w| w.0);
        let height = self.height.t.as_ref().map(|h| h.0);
        let content_fit = self.content_fit.t.as_ref().map(|cf| cf.0);
        match &self.handle {
            Some(ImageHandle::Svg(h)) => {
                let mut s = widget::Svg::new(h.clone());
                if let Some(w) = width {
                    s = s.width(w);
                }
                if let Some(h) = height {
                    s = s.height(h);
                }
                if let Some(cf) = content_fit {
                    s = s.content_fit(cf);
                }
                s.into()
            }
            None => widget::Space::new().into(),
            Some(ImageHandle::Raster(h)) => {
                let mut img = widget::Image::new(h.clone());
                if let Some(w) = width {
                    img = img.width(w);
                }
                if let Some(h) = height {
                    img = img.height(h);
                }
                if let Some(cf) = content_fit {
                    img = img.content_fit(cf);
                }
                img.into()
            }
        }
    }
}
