use super::{GuiW, IcedElement};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle};
use iced_widget as widget;
use log::error;
use netidx::publisher::Value;

graphix_rt::props! {
    struct Props {
        cell_size: Option<f64>,
        data: ArcStr,
    }
}

pub(crate) struct QrCodeW<X: GXExt> {
    p: Props<X>,
    qr_data: Option<widget::qr_code::Data>,
}

impl<X: GXExt> QrCodeW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let p = Props::compile(&gx, &source).await.context("qr_code")?;
        let qr_data = Self::encode(&p);
        Ok(Box::new(Self { p, qr_data }))
    }

    fn encode(p: &Props<X>) -> Option<widget::qr_code::Data> {
        p.data.t.as_deref().and_then(|s| match widget::qr_code::Data::new(s) {
            Ok(d) => Some(d),
            Err(e) => {
                error!("qr_code: failed to encode data: {e}");
                None
            }
        })
    }
}

impl<X: GXExt> super::GuiWidget<X> for QrCodeW<X> {
    fn handle_update(
        &mut self,
        _rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let changed = self.p.update(id, v).context("qr_code")?;
        if id == self.p.data.r.id {
            self.qr_data = Self::encode(&self.p);
        }
        Ok(changed)
    }

    fn view(&self) -> IcedElement<'_> {
        match &self.qr_data {
            Some(data) => {
                let mut qr = widget::QRCode::new(data);
                if let Some(Some(sz)) = self.p.cell_size.t {
                    qr = qr.cell_size(sz as f32);
                }
                qr.into()
            }
            None => iced_widget::Space::new().into(),
        }
    }
}
