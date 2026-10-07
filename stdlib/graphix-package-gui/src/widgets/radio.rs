use super::{GuiW, GuiWidget, Handler, IcedElement, Message};
use crate::types::{LengthV, TextSizeV};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use futures::try_join;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle};
use iced_widget as widget;
use netidx::{protocol::valarray::ValArray, publisher::Value};

graphix_rt::props! {
    struct Props {
        disabled: bool,
        label: ArcStr,
        selected: Value,
        size: Option<TextSizeV>,
        spacing: Option<f64>,
        value: Value,
        width: LengthV,
    }
}

pub(crate) struct RadioW<X: GXExt> {
    gx: GXHandle<X>,
    p: Props<X>,
    on_select: Handler<X>,
}

impl<X: GXExt> RadioW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let (p, on_select) = try_join!(
            Props::compile(&gx, &source),
            Handler::field(&gx, &source, "on_select"),
        )
        .context("radio")?;
        Ok(Box::new(Self { gx, p, on_select }))
    }
}

impl<X: GXExt> GuiWidget<X> for RadioW<X> {
    fn handle_update(
        &mut self,
        rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let changed = self.p.update(id, v).context("radio")?;
        self.on_select
            .update(rt, &self.gx, id, v)
            .context("radio on_select recompile")?;
        Ok(changed)
    }

    fn view(&self) -> IcedElement<'_> {
        let label = self.p.label.t.as_deref().unwrap_or("");
        let is_selected = self.p.value.t.is_some() && self.p.value.t == self.p.selected.t;
        // a radio without its value has nothing to select
        let on_select = match (&self.p.value.t, self.p.disabled.t.unwrap_or(false)) {
            (Some(v), false) => self.on_select.id().map(|id| (id, v.clone())),
            _ => None,
        };
        // iced's Radio needs a Copy + Eq value type; selection is computed here.
        let mut r =
            widget::Radio::new(label, true, is_selected.then_some(true), move |_| {
                match &on_select {
                    Some((id, v)) => Message::Call(*id, ValArray::from_iter([v.clone()])),
                    None => Message::Nop,
                }
            });
        if let Some(w) = self.p.width.t.as_ref() {
            r = r.width(w.0);
        }
        if let Some(Some(TextSizeV(sz))) = self.p.size.t {
            r = r.size(sz);
        }
        if let Some(Some(sp)) = self.p.spacing.t {
            r = r.spacing(sp as f32);
        }
        r.into()
    }
}
