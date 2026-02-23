use crate::{types::LengthV, GuiW, GuiWidget, IcedElement};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle, TRef};
use iced_widget as widget;
use netidx::publisher::Value;
use tokio::try_join;

pub(crate) struct SpaceW<X: GXExt> {
    width: TRef<X, LengthV>,
    height: TRef<X, LengthV>,
}

impl<X: GXExt> SpaceW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let [(_, height), (_, width)] =
            source.cast_to::<[(ArcStr, u64); 2]>().context("space flds")?;
        let (height, width) = try_join! {
            gx.compile_ref(height),
            gx.compile_ref(width),
        }?;
        Ok(Box::new(Self {
            width: TRef::new(width).context("space tref width")?,
            height: TRef::new(height).context("space tref height")?,
        }))
    }
}

impl<X: GXExt> GuiWidget<X> for SpaceW<X> {
    fn handle_update(
        &mut self,
        _rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let mut changed = false;
        changed |= self.width.update(id, v).context("space update width")?.is_some();
        changed |= self.height.update(id, v).context("space update height")?.is_some();
        Ok(changed)
    }

    fn view(&self) -> IcedElement<'_> {
        let mut s = widget::Space::new();
        if let Some(w) = self.width.t.as_ref() {
            s = s.width(w.0);
        }
        if let Some(h) = self.height.t.as_ref() {
            s = s.height(h.0);
        }
        s.into()
    }
}
