use super::{GuiW, GuiWidget, IcedElement};
use crate::types::{ContentFitV, ImageSourceV, LengthV};
use anyhow::{Context, Result};
use futures::try_join;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle, TRef};
use iced_core::{image, svg};
use iced_widget as widget;
use netidx::publisher::Value;

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

graphix_rt::props! {
    struct Props {
        content_fit: ContentFitV,
        height: LengthV,
        width: LengthV,
    }
}

pub(crate) struct ImageW<X: GXExt> {
    p: Props<X>,
    source: TRef<X, ImageSourceV>,
    handle: Option<ImageHandle>,
}

impl<X: GXExt> ImageW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let (p, src) =
            try_join!(Props::compile(&gx, &source), gx.compile_field(&source, "source"),)
                .context("image")?;
        let source = TRef::new(src).context("image source")?;
        let handle = source.t.as_ref().map(ImageHandle::of);
        Ok(Box::new(Self { p, source, handle }))
    }
}

impl<X: GXExt> GuiWidget<X> for ImageW<X> {
    fn handle_update(
        &mut self,
        _rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let mut changed = self.p.update(id, v).context("image")?;
        let old = (id == self.source.r.id).then(|| self.source.t.clone()).flatten();
        if let Some(new) = self.source.update(id, v).context("image source")?
            && !old.is_some_and(|o| o.same_as(new))
        {
            self.handle = Some(ImageHandle::of(new));
            changed = true;
        }
        Ok(changed)
    }

    fn view(&self) -> IcedElement<'_> {
        let width = self.p.width.t.as_ref().map(|w| w.0);
        let height = self.p.height.t.as_ref().map(|h| h.0);
        let content_fit = self.p.content_fit.t.as_ref().map(|cf| cf.0);
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
