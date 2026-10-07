use super::{GuiW, GuiWidget, Handler, IcedElement, Message};
use crate::types::{LengthV, TextSizeV};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle, Ref, TRef};
use iced_widget as widget;
use netidx::{protocol::valarray::ValArray, publisher::Value};
use netidx_derive::FromValue;
use tokio::try_join;

pub(crate) struct RadioW<X: GXExt> {
    gx: GXHandle<X>,
    disabled: TRef<X, bool>,
    value: Ref<X>,
    label: TRef<X, ArcStr>,
    selected: Ref<X>,
    on_select: Handler<X>,
    width: TRef<X, LengthV>,
    size: TRef<X, Option<TextSizeV>>,
    spacing: TRef<X, Option<f64>>,
}

impl<X: GXExt> RadioW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        #[derive(FromValue)]
        struct Fields {
            disabled: u64,
            label: u64,
            on_select: u64,
            selected: u64,
            size: u64,
            spacing: u64,
            value: u64,
            width: u64,
        }
        let Fields { disabled, label, on_select, selected, size, spacing, value, width } =
            source.cast_to().context("radio flds")?;
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
        let on_select =
            Handler::compile(&gx, on_select).await.context("radio on_select")?;
        Ok(Box::new(Self {
            gx: gx.clone(),
            disabled: TRef::new(disabled).context("radio tref disabled")?,
            value,
            label: TRef::new(label).context("radio tref label")?,
            selected,
            on_select,
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
        self.on_select
            .update(rt, &self.gx, id, v)
            .context("radio on_select recompile")?;
        Ok(changed)
    }

    fn view(&self) -> IcedElement<'_> {
        let label = self.label.t.as_deref().unwrap_or("");
        let is_selected =
            self.value.last.is_some() && self.value.last == self.selected.last;
        // a radio without its value has nothing to select
        let on_select = match (&self.value.last, self.disabled.t.unwrap_or(false)) {
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
        if let Some(w) = self.width.t.as_ref() {
            r = r.width(w.0);
        }
        if let Some(Some(TextSizeV(sz))) = self.size.t {
            r = r.size(sz);
        }
        if let Some(Some(sp)) = self.spacing.t {
            r = r.spacing(sp as f32);
        }
        r.into()
    }
}
