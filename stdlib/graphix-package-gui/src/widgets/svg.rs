use super::{GuiW, GuiWidget, IcedElement};
use crate::types::{ContentFitV, LengthV};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle, TRef};
use iced_core::svg::Handle;
use iced_widget as widget;
use netidx::publisher::Value;
use tokio::try_join;

fn make_handle(source: &str) -> Handle {
    if source.trim_start().starts_with('<') {
        Handle::from_memory(source.as_bytes().to_vec())
    } else {
        Handle::from_path(source)
    }
}

pub(crate) struct SvgW<X: GXExt> {
    source: TRef<X, String>,
    handle: Option<Handle>,
    width: TRef<X, LengthV>,
    height: TRef<X, LengthV>,
    content_fit: TRef<X, ContentFitV>,
}

impl<X: GXExt> SvgW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let [(_, content_fit), (_, height), (_, src), (_, width)] =
            source.cast_to::<[(ArcStr, u64); 4]>().context("svg flds")?;
        let (content_fit, height, src, width) = try_join! {
            gx.compile_ref(content_fit),
            gx.compile_ref(height),
            gx.compile_ref(src),
            gx.compile_ref(width),
        }?;
        let source = TRef::new(src).context("svg tref source")?;
        let handle = source.t.as_deref().map(make_handle);
        Ok(Box::new(Self {
            source,
            handle,
            width: TRef::new(width).context("svg tref width")?,
            height: TRef::new(height).context("svg tref height")?,
            content_fit: TRef::new(content_fit).context("svg tref content_fit")?,
        }))
    }
}

impl<X: GXExt> GuiWidget<X> for SvgW<X> {
    fn handle_update(
        &mut self,
        _rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let mut changed = false;
        if self.source.update(id, v).context("svg update source")?.is_some() {
            self.handle = self.source.t.as_deref().map(make_handle);
            changed = true;
        }
        changed |= self.width.update(id, v).context("svg update width")?.is_some();
        changed |= self.height.update(id, v).context("svg update height")?.is_some();
        changed |=
            self.content_fit.update(id, v).context("svg update content_fit")?.is_some();
        Ok(changed)
    }

    fn view(&self) -> IcedElement<'_> {
        let handle = match &self.handle {
            Some(h) => h.clone(),
            None => Handle::from_path(""),
        };
        let mut s = widget::Svg::new(handle);
        if let Some(w) = self.width.t.as_ref() {
            s = s.width(w.0);
        }
        if let Some(h) = self.height.t.as_ref() {
            s = s.height(h.0);
        }
        if let Some(cf) = self.content_fit.t.as_ref() {
            s = s.content_fit(cf.0);
        }
        s.into()
    }
}
