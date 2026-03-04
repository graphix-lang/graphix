use super::{GuiW, IcedElement};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle, TRef};
use iced_widget as widget;
use log::error;
use netidx::publisher::Value;
use tokio::try_join;

pub(crate) struct QrCodeW<X: GXExt> {
    data: TRef<X, String>,
    cell_size: TRef<X, Option<f64>>,
    qr_data: Option<widget::qr_code::Data>,
}

impl<X: GXExt> QrCodeW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let [(_, cell_size), (_, data)] =
            source.cast_to::<[(ArcStr, u64); 2]>().context("qr_code flds")?;
        let (cell_size, data) = try_join! {
            gx.compile_ref(cell_size),
            gx.compile_ref(data),
        }?;
        let data = TRef::new(data).context("qr_code tref data")?;
        let qr_data = data.t.as_deref().and_then(|s| match widget::qr_code::Data::new(s) {
            Ok(d) => Some(d),
            Err(e) => {
                error!("qr_code: failed to encode data: {e}");
                None
            }
        });
        Ok(Box::new(Self {
            data,
            cell_size: TRef::new(cell_size).context("qr_code tref cell_size")?,
            qr_data,
        }))
    }
}

impl<X: GXExt> super::GuiWidget<X> for QrCodeW<X> {
    fn handle_update(
        &mut self,
        _rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let mut changed = false;
        changed |=
            self.cell_size.update(id, v).context("qr_code update cell_size")?.is_some();
        if let Some(_) = self.data.update(id, v).context("qr_code update data")? {
            self.qr_data = self.data.t.as_deref().and_then(
                |s| match widget::qr_code::Data::new(s) {
                    Ok(d) => Some(d),
                    Err(e) => {
                        error!("qr_code: failed to encode data: {e}");
                        None
                    }
                },
            );
            changed = true;
        }
        Ok(changed)
    }

    fn view(&self) -> IcedElement<'_> {
        match &self.qr_data {
            Some(data) => {
                let mut qr = widget::QRCode::new(data);
                if let Some(Some(sz)) = self.cell_size.t {
                    qr = qr.cell_size(sz as f32);
                }
                qr.into()
            }
            None => iced_widget::Space::new().into(),
        }
    }
}
