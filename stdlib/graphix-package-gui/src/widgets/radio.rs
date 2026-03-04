use super::{GuiW, GuiWidget, IcedElement, Message};
use crate::types::LengthV;
use anyhow::{Context, Result};
use arcstr::ArcStr;
use graphix_compiler::expr::ExprId;
use graphix_rt::{Callable, GXExt, GXHandle, Ref, TRef};
use iced_widget as widget;
use netidx::{protocol::valarray::ValArray, publisher::Value};
use tokio::try_join;

pub(crate) struct RadioW<X: GXExt> {
    gx: GXHandle<X>,
    disabled: TRef<X, bool>,
    value: Ref<X>,
    label: TRef<X, String>,
    selected: Ref<X>,
    on_select: Ref<X>,
    on_select_callable: Option<Callable<X>>,
    width: TRef<X, LengthV>,
    size: TRef<X, Option<f64>>,
    spacing: TRef<X, Option<f64>>,
}

impl<X: GXExt> RadioW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let [(_, disabled), (_, label), (_, on_select), (_, selected), (_, size), (_, spacing), (_, value), (_, width)] =
            source.cast_to::<[(ArcStr, u64); 8]>().context("radio flds")?;
        let (disabled, label, on_select, selected, size, spacing, value, width) = try_join! {
            gx.compile_ref(disabled),
            gx.compile_ref(label),
            gx.compile_ref(on_select),
            gx.compile_ref(selected),
            gx.compile_ref(size),
            gx.compile_ref(spacing),
            gx.compile_ref(value),
            gx.compile_ref(width),
        }?;
        let callable =
            compile_callable!(gx, on_select, "radio on_select");
        Ok(Box::new(Self {
            gx: gx.clone(),
            disabled: TRef::new(disabled).context("radio tref disabled")?,
            value,
            label: TRef::new(label).context("radio tref label")?,
            selected,
            on_select,
            on_select_callable: callable,
            width: TRef::new(width).context("radio tref width")?,
            size: TRef::new(size).context("radio tref size")?,
            spacing: TRef::new(spacing).context("radio tref spacing")?,
        }))
    }
}

impl<X: GXExt> GuiWidget<X> for RadioW<X> {
    fn handle_update(
        &mut self,
        rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let mut changed = false;
        changed |=
            self.disabled.update(id, v).context("radio update disabled")?.is_some();
        if id == self.value.id {
            self.value.last = Some(v.clone());
            changed = true;
        }
        changed |= self.label.update(id, v).context("radio update label")?.is_some();
        if id == self.selected.id {
            self.selected.last = Some(v.clone());
            changed = true;
        }
        changed |= self.width.update(id, v).context("radio update width")?.is_some();
        changed |= self.size.update(id, v).context("radio update size")?.is_some();
        changed |= self.spacing.update(id, v).context("radio update spacing")?.is_some();
        update_callable!(self, rt, id, v, on_select, on_select_callable, "radio on_select recompile");
        Ok(changed)
    }

    fn view(&self) -> IcedElement<'_> {
        let label = self.label.t.as_deref().unwrap_or("");
        let is_selected =
            self.value.last.is_some() && self.value.last == self.selected.last;
        let on_select_id = if self.disabled.t.unwrap_or(false) {
            None
        } else {
            self.on_select_callable.as_ref().map(|c| c.id())
        };
        let value_for_callback = self.value.last.clone().unwrap_or(Value::Null);
        // Use bool as the dummy value type for iced's Radio (needs Copy + Eq).
        // Selection state is computed by us via value/selected comparison.
        let mut r =
            widget::Radio::new(label, true, is_selected.then_some(true), move |_| {
                match on_select_id {
                    Some(id) => Message::Call(
                        id,
                        ValArray::from_iter([value_for_callback.clone()]),
                    ),
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
