use super::{GuiW, GuiWidget, IcedElement, Message};
use crate::types::LengthV;
use anyhow::{Context, Result};
use arcstr::ArcStr;
use graphix_compiler::expr::ExprId;
use graphix_rt::{Callable, GXExt, GXHandle, Ref, TRef};
use iced_widget as widget;
use netidx::{protocol::valarray::ValArray, publisher::Value};
use tokio::try_join;

pub(crate) struct TogglerW<X: GXExt> {
    gx: GXHandle<X>,
    is_toggled: TRef<X, bool>,
    label: TRef<X, String>,
    on_toggle: Ref<X>,
    on_toggle_callable: Option<Callable<X>>,
    width: TRef<X, LengthV>,
    size: TRef<X, Option<f64>>,
    spacing: TRef<X, Option<f64>>,
}

impl<X: GXExt> TogglerW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let [(_, is_toggled), (_, label), (_, on_toggle), (_, size), (_, spacing), (_, width)] =
            source.cast_to::<[(ArcStr, u64); 6]>().context("toggler flds")?;
        let (is_toggled, label, on_toggle, size, spacing, width) = try_join! {
            gx.compile_ref(is_toggled),
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
                    .context("toggler on_toggle callable")?,
            ),
            None => None,
        };
        Ok(Box::new(Self {
            gx: gx.clone(),
            is_toggled: TRef::new(is_toggled).context("toggler tref is_toggled")?,
            label: TRef::new(label).context("toggler tref label")?,
            on_toggle,
            on_toggle_callable: callable,
            width: TRef::new(width).context("toggler tref width")?,
            size: TRef::new(size).context("toggler tref size")?,
            spacing: TRef::new(spacing).context("toggler tref spacing")?,
        }))
    }
}

impl<X: GXExt> GuiWidget<X> for TogglerW<X> {
    fn handle_update(
        &mut self,
        rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let mut changed = false;
        changed |=
            self.is_toggled.update(id, v).context("toggler update is_toggled")?.is_some();
        changed |= self.label.update(id, v).context("toggler update label")?.is_some();
        changed |= self.width.update(id, v).context("toggler update width")?.is_some();
        changed |= self.size.update(id, v).context("toggler update size")?.is_some();
        changed |=
            self.spacing.update(id, v).context("toggler update spacing")?.is_some();
        if id == self.on_toggle.id {
            self.on_toggle.last = Some(v.clone());
            self.on_toggle_callable = Some(
                rt.block_on(self.gx.compile_callable(v.clone()))
                    .context("toggler on_toggle callable recompile")?,
            );
        }
        Ok(changed)
    }

    fn view(&self) -> IcedElement<'_> {
        let label = self.label.t.as_deref().unwrap_or("");
        let toggled = self.is_toggled.t.unwrap_or(false);
        let mut tg = widget::Toggler::new(toggled);
        if !label.is_empty() {
            tg = tg.label(label);
        }
        if let Some(callable) = &self.on_toggle_callable {
            let id = callable.id();
            tg = tg.on_toggle(move |b| {
                Message::Call(id, ValArray::from_iter([Value::from(b)]))
            });
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
