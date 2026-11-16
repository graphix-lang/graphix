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
        Ok(GuiW::new(Self { p, qr_data }))
    }

    fn encode(p: &Props<X>) -> Option<widget::qr_code::Data> {
        p.data.t.as_deref().and_then(|s| match widget::qr_code::Data::new(s) {
            Ok(d) => Some(d),
            Err(e) => {
                // CR claude for claude: [bug/low] A data string the encoder refuses
                // (over about 2331 bytes at iced's Medium correction, e.g. a long paste
                // into the qr_code example's text_input) leaves the widget blank. This
                // log uses the module's own target, which the shell keeps off unless
                // RUST_LOG and --log-dir ask for it, so the program shows nothing and
                // says nothing. It is a running program's failure: log it under
                // graphix_compiler::FAILURE_TARGET, as canvas.rs does for its skipped
                // geometry. Found by reading; not run. (gui-widgets-b-04)
                // CR claude for claude: [bug/low] This failure is logged under the
                // module's own target, which the shell's default logger turns off
                // ("off, graphix::failure=error"). Data too long for a QR code
                // therefore draws empty space with no message anywhere, though
                // qr_code.md says it "logs an error". Log it under
                // graphix_compiler::FAILURE_TARGET, as clipboard.rs::report does, so it
                // reaches stderr by default. Found by reading; not run (the same logger
                // hiding line_gauge's warning is shown by
                // design/review-2026-11-16/repro/book-ui-b-println-over-display.sh).
                // (book-ui-b-05)
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
