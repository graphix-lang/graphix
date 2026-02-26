use super::{GuiW, GuiWidget, IcedElement, Message};
use crate::types::LengthV;
use anyhow::{Context, Result};
use arcstr::ArcStr;
use graphix_compiler::expr::ExprId;
use graphix_rt::{Callable, GXExt, GXHandle, Ref, TRef};
use iced_widget as widget;
use netidx::{protocol::valarray::ValArray, publisher::Value};
use tokio::try_join;

pub(crate) struct CheckboxW<X: GXExt> {
    gx: GXHandle<X>,
    is_checked: TRef<X, bool>,
    label: TRef<X, String>,
    on_toggle: Ref<X>,
    on_toggle_callable: Option<Callable<X>>,
    width: TRef<X, LengthV>,
    size: TRef<X, Option<f64>>,
    spacing: TRef<X, Option<f64>>,
}

impl<X: GXExt> CheckboxW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let [(_, is_checked), (_, label), (_, on_toggle), (_, size), (_, spacing), (_, width)] =
            source.cast_to::<[(ArcStr, u64); 6]>().context("checkbox flds")?;
        let (is_checked, label, on_toggle, size, spacing, width) = try_join! {
            gx.compile_ref(is_checked),
            gx.compile_ref(label),
            gx.compile_ref(on_toggle),
            gx.compile_ref(size),
            gx.compile_ref(spacing),
            gx.compile_ref(width),
        }?;
        let callable = match on_toggle.last.as_ref() {
            Some(v) => Some(
                gx.compile_callable(v.clone())
                    .await
                    .context("checkbox on_toggle callable")?,
            ),
            None => None,
        };
        Ok(Box::new(Self {
            gx: gx.clone(),
            is_checked: TRef::new(is_checked).context("checkbox tref is_checked")?,
            label: TRef::new(label).context("checkbox tref label")?,
            on_toggle,
            on_toggle_callable: callable,
            width: TRef::new(width).context("checkbox tref width")?,
            size: TRef::new(size).context("checkbox tref size")?,
            spacing: TRef::new(spacing).context("checkbox tref spacing")?,
        }))
    }
}

impl<X: GXExt> GuiWidget<X> for CheckboxW<X> {
    fn handle_update(
        &mut self,
        rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let mut changed = false;
        changed |= self
            .is_checked
            .update(id, v)
            .context("checkbox update is_checked")?
            .is_some();
        changed |= self.label.update(id, v).context("checkbox update label")?.is_some();
        changed |= self.width.update(id, v).context("checkbox update width")?.is_some();
        changed |= self.size.update(id, v).context("checkbox update size")?.is_some();
        changed |=
            self.spacing.update(id, v).context("checkbox update spacing")?.is_some();
        if id == self.on_toggle.id {
            self.on_toggle.last = Some(v.clone());
            self.on_toggle_callable = Some(
                rt.block_on(self.gx.compile_callable(v.clone()))
                    .context("checkbox on_toggle callable recompile")?,
            );
        }
        Ok(changed)
    }

    fn view(&self) -> IcedElement<'_> {
        let label = self.label.t.as_deref().unwrap_or("");
        let checked = self.is_checked.t.unwrap_or(false);
        let mut cb = widget::Checkbox::new(checked).label(label);
        if let Some(callable) = &self.on_toggle_callable {
            let id = callable.id();
            cb = cb.on_toggle(move |b| {
                Message::Call(id, ValArray::from_iter([Value::from(b)]))
            });
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
