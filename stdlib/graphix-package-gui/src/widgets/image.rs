use super::{GuiW, GuiWidget, IcedElement};
use crate::types::{ContentFitV, ImageSourceV, LengthV};
use anyhow::{Context, Result};
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle, TRef};
use iced_widget as widget;
use netidx::publisher::Value;
use netidx_derive::FromValue;
use tokio::try_join;

// CR claude for eric: [structure] make_handle picks a branch with is_svg() and then
// calls to_handle or to_svg_handle (types.rs:355-382). This dispatch never reaches
// to_handle's Svg arm or to_svg_handle's Bytes and Rgba arms, and each of those builds
// an empty Handle::from_path(""). view() also builds a from_path("") image on every
// frame while there is no source. One exhaustive match on ImageSourceV here can replace
// the three functions, and view() can render Space when handle is None.
// book/src/ui/gui/image.md still documents a separate svg widget that does not exist,
// and its ImageSource omits the Svg(string) variant. (gui-widgets-a-16)
fn make_handle(source: &ImageSourceV) -> ImageHandle {
    if source.is_svg() {
        ImageHandle::Svg(source.to_svg_handle())
    } else {
        ImageHandle::Raster(source.to_handle())
    }
}

enum ImageHandle {
    Raster(iced_core::image::Handle),
    Svg(iced_core::svg::Handle),
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
        let handle = source.t.as_ref().map(make_handle);
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
        // CR claude for eric: [bug] Every delivery of the source rebuilds the handle,
        // and iced gives each Bytes or Rgba handle a fresh unique id, so its id-keyed
        // raster cache misses even when the value is unchanged:
        // ``image(&`Bytes(state.logo))`` delivers the same bytes again on every change
        // to another field of `state`, and so does a source variable re-set to equal
        // bytes. For Bytes, iced_wgpu then lays the new handle out at 0x0 under
        // `Shrink` and draws nothing until its worker thread has decoded it again. The
        // worker reports completion to the `Shell::headless()` the renderer is built
        // with (render.rs:58), so nothing redraws: the image stays blank until an
        // unrelated event redraws the window, and it never shows while the source keeps
        // re-firing. An Rgba source under 2 MB is re-uploaded on the GUI thread on
        // every delivery instead. Keep the handle, and report no change, when the new
        // source equals the one it was made from (Bytes by pointer and length first,
        // then by content). probe:
        // design/review-2026-10-05/repro/gui-widgets-a.r2-16.gx (opens a window;
        // typechecked, not run). (gui-widgets-a.r2-16)
        if self.source.update(id, v).context("image update source")?.is_some() {
            self.handle = self.source.t.as_ref().map(make_handle);
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
            handle => {
                let h = match handle {
                    Some(ImageHandle::Raster(h)) => h.clone(),
                    _ => iced_core::image::Handle::from_path(""),
                };
                let mut img = widget::Image::new(h);
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
