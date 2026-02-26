use super::{GuiW, GuiWidget, IcedElement, Message};
use crate::types::LengthV;
use anyhow::{Context, Result};
use arcstr::ArcStr;
use graphix_compiler::{expr::ExprId, BindId};
use graphix_rt::{GXExt, GXHandle, TRef};
use iced_widget as widget;
use netidx::publisher::Value;
use tokio::try_join;

pub(crate) struct TogglerW<X: GXExt> {
    is_toggled: TRef<X, bool>,
    target_bid: Option<BindId>,
    label: TRef<X, String>,
    width: TRef<X, LengthV>,
    size: TRef<X, Option<f64>>,
    spacing: TRef<X, Option<f64>>,
}

impl<X: GXExt> TogglerW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let [(_, is_toggled), (_, label), (_, size), (_, spacing), (_, width)] =
            source.cast_to::<[(ArcStr, u64); 5]>().context("toggler flds")?;
        let (is_toggled, label, size, spacing, width) = try_join! {
            gx.compile_ref(is_toggled),
            gx.compile_ref(label),
            gx.compile_ref(size),
            gx.compile_ref(spacing),
            gx.compile_ref(width),
        }?;
        let target_bid = is_toggled.target_bid;
        Ok(Box::new(Self {
            is_toggled: TRef::new(is_toggled).context("toggler tref is_toggled")?,
            target_bid,
            label: TRef::new(label).context("toggler tref label")?,
            width: TRef::new(width).context("toggler tref width")?,
            size: TRef::new(size).context("toggler tref size")?,
            spacing: TRef::new(spacing).context("toggler tref spacing")?,
        }))
    }
}

impl<X: GXExt> GuiWidget<X> for TogglerW<X> {
    fn handle_update(
        &mut self,
        _rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let mut changed = false;
        changed |= self.is_toggled.update(id, v).context("toggler update is_toggled")?.is_some();
        changed |= self.label.update(id, v).context("toggler update label")?.is_some();
        changed |= self.width.update(id, v).context("toggler update width")?.is_some();
        changed |= self.size.update(id, v).context("toggler update size")?.is_some();
        changed |= self.spacing.update(id, v).context("toggler update spacing")?.is_some();
        Ok(changed)
    }

    fn view(&self) -> IcedElement<'_> {
        let label = self.label.t.as_deref().unwrap_or("");
        let toggled = self.is_toggled.t.unwrap_or(false);
        let mut tg = widget::Toggler::new(toggled);
        if !label.is_empty() {
            tg = tg.label(label);
        }
        if let Some(bid) = self.target_bid {
            tg = tg.on_toggle(move |b| Message::Set(bid, Value::from(b)));
        }
        if let Some(w) = self.width.t.as_ref() {
            tg = tg.width(w.0);
        }
        if let Some(Some(sz)) = self.size.t {
            tg = tg.size(sz as f32);
        }
        if let Some(Some(sp)) = self.spacing.t {
            tg = tg.spacing(sp as f32);
        }
        tg.into()
    }
}
