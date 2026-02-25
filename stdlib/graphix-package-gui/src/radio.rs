use crate::{
    types::LengthV,
    GuiW, GuiWidget, IcedElement, Message,
};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use graphix_compiler::{expr::ExprId, BindId};
use graphix_rt::{GXExt, GXHandle, TRef};
use iced_widget as widget;
use netidx::publisher::Value;
use tokio::try_join;

pub(crate) struct RadioW<X: GXExt> {
    value: TRef<X, String>,
    label: TRef<X, String>,
    selected: TRef<X, Option<String>>,
    target_bid: Option<BindId>,
    width: TRef<X, LengthV>,
    size: TRef<X, Option<f64>>,
    spacing: TRef<X, Option<f64>>,
}

impl<X: GXExt> RadioW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let [(_, label), (_, selected), (_, size), (_, spacing), (_, value), (_, width)] =
            source.cast_to::<[(ArcStr, u64); 6]>().context("radio flds")?;
        let (label, selected, size, spacing, value, width) = try_join! {
            gx.compile_ref(label),
            gx.compile_ref(selected),
            gx.compile_ref(size),
            gx.compile_ref(spacing),
            gx.compile_ref(value),
            gx.compile_ref(width),
        }?;
        let target_bid = selected.target_bid;
        Ok(Box::new(Self {
            value: TRef::new(value).context("radio tref value")?,
            label: TRef::new(label).context("radio tref label")?,
            selected: TRef::new(selected).context("radio tref selected")?,
            target_bid,
            width: TRef::new(width).context("radio tref width")?,
            size: TRef::new(size).context("radio tref size")?,
            spacing: TRef::new(spacing).context("radio tref spacing")?,
        }))
    }
}

impl<X: GXExt> GuiWidget<X> for RadioW<X> {
    fn handle_update(
        &mut self,
        _rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let mut changed = false;
        changed |= self.value.update(id, v).context("radio update value")?.is_some();
        changed |= self.label.update(id, v).context("radio update label")?.is_some();
        changed |= self.selected.update(id, v).context("radio update selected")?.is_some();
        changed |= self.width.update(id, v).context("radio update width")?.is_some();
        changed |= self.size.update(id, v).context("radio update size")?.is_some();
        changed |= self.spacing.update(id, v).context("radio update spacing")?.is_some();
        Ok(changed)
    }

    fn view(&self) -> IcedElement<'_> {
        let label = self.label.t.as_deref().unwrap_or("");
        let value = self.value.t.as_deref().unwrap_or("");
        let selected = self.selected.t.as_ref().and_then(|o| o.as_deref());
        let is_selected = selected == Some(value);
        let mut r = widget::Radio::new(label, value, is_selected.then_some(value), |v| {
            match self.target_bid {
                Some(bid) => Message::Set(bid, Value::String(v.to_string().into())),
                None => Message::Nop,
            }
        });
        if let Some(w) = self.width.t.as_ref() {
            r = r.width(w.0);
        }
        if let Some(Some(sz)) = self.size.t {
            r = r.size(sz as f32);
        }
        if let Some(Some(sp)) = self.spacing.t {
            r = r.spacing(sp as f32);
        }
        r.into()
    }
}
