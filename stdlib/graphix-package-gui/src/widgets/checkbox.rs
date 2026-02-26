use super::{GuiW, GuiWidget, IcedElement, Message};
use crate::types::LengthV;
use anyhow::{Context, Result};
use arcstr::ArcStr;
use graphix_compiler::{expr::ExprId, BindId};
use graphix_rt::{GXExt, GXHandle, TRef};
use iced_widget as widget;
use netidx::publisher::Value;
use tokio::try_join;

pub(crate) struct CheckboxW<X: GXExt> {
    is_checked: TRef<X, bool>,
    target_bid: Option<BindId>,
    label: TRef<X, String>,
    width: TRef<X, LengthV>,
    size: TRef<X, Option<f64>>,
    spacing: TRef<X, Option<f64>>,
}

impl<X: GXExt> CheckboxW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let [(_, is_checked), (_, label), (_, size), (_, spacing), (_, width)] =
            source.cast_to::<[(ArcStr, u64); 5]>().context("checkbox flds")?;
        let (is_checked, label, size, spacing, width) = try_join! {
            gx.compile_ref(is_checked),
            gx.compile_ref(label),
            gx.compile_ref(size),
            gx.compile_ref(spacing),
            gx.compile_ref(width),
        }?;
        let target_bid = is_checked.target_bid;
        Ok(Box::new(Self {
            is_checked: TRef::new(is_checked).context("checkbox tref is_checked")?,
            target_bid,
            label: TRef::new(label).context("checkbox tref label")?,
            width: TRef::new(width).context("checkbox tref width")?,
            size: TRef::new(size).context("checkbox tref size")?,
            spacing: TRef::new(spacing).context("checkbox tref spacing")?,
        }))
    }
}

impl<X: GXExt> GuiWidget<X> for CheckboxW<X> {
    fn handle_update(
        &mut self,
        _rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let mut changed = false;
        changed |= self.is_checked.update(id, v).context("checkbox update is_checked")?.is_some();
        changed |= self.label.update(id, v).context("checkbox update label")?.is_some();
        changed |= self.width.update(id, v).context("checkbox update width")?.is_some();
        changed |= self.size.update(id, v).context("checkbox update size")?.is_some();
        changed |= self.spacing.update(id, v).context("checkbox update spacing")?.is_some();
        Ok(changed)
    }

    fn view(&self) -> IcedElement<'_> {
        let label = self.label.t.as_deref().unwrap_or("");
        let checked = self.is_checked.t.unwrap_or(false);
        let mut cb = widget::Checkbox::new(checked)
            .label(label);
        if let Some(bid) = self.target_bid {
            cb = cb.on_toggle(move |b| Message::Set(bid, Value::from(b)));
        }
        if let Some(w) = self.width.t.as_ref() {
            cb = cb.width(w.0);
        }
        if let Some(Some(sz)) = self.size.t {
            cb = cb.size(sz as f32);
        }
        if let Some(Some(sp)) = self.spacing.t {
            cb = cb.spacing(sp as f32);
        }
        cb.into()
    }
}
