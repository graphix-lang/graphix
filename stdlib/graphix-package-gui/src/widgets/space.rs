use super::{GuiW, GuiWidget, IcedElement};
use crate::types::LengthV;
use anyhow::{Context, Result};
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle};
use iced_widget as widget;
use netidx::publisher::Value;

graphix_rt::props! {
    struct Props {
        height: LengthV,
        width: LengthV,
    }
}

pub(crate) struct SpaceW<X: GXExt>(Props<X>);

impl<X: GXExt> SpaceW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        Ok(Box::new(Self(Props::compile(&gx, &source).await.context("space")?)))
    }
}

impl<X: GXExt> GuiWidget<X> for SpaceW<X> {
    fn handle_update(
        &mut self,
        _rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        self.0.update(id, v).context("space")
    }

    fn view(&self) -> IcedElement<'_> {
        let mut s = widget::Space::new();
        if let Some(w) = self.0.width.t.as_ref() {
            s = s.width(w.0);
        }
        if let Some(h) = self.0.height.t.as_ref() {
            s = s.height(h.0);
        }
        s.into()
    }
}
