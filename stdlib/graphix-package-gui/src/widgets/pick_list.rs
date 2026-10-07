use super::{GuiW, GuiWidget, Handler, IcedElement, Message, disabled_choice};
use crate::types::{LengthV, PaddingV, StringVec};
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
        options: StringVec,
        padding: PaddingV,
        placeholder: ArcStr,
        selected: Option<String>,
        width: LengthV,
    }
}

pub(crate) struct PickListW<X: GXExt> {
    gx: GXHandle<X>,
    p: Props<X>,
    on_select: Handler<X>,
}

impl<X: GXExt> PickListW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let (p, on_select) = try_join!(
            Props::compile(&gx, &source),
            Handler::field(&gx, &source, "on_select"),
        )
        .context("pick_list")?;
        Ok(Box::new(Self { gx, p, on_select }))
    }
}

impl<X: GXExt> GuiWidget<X> for PickListW<X> {
    fn handle_update(
        &mut self,
        rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let changed = self.p.update(id, v).context("pick_list")?;
        self.on_select
            .update(rt, &self.gx, id, v)
            .context("pick_list on_select recompile")?;
        Ok(changed)
    }

    fn view(&self) -> IcedElement<'_> {
        let options = self.p.options.t.as_ref().map(|v| v.0.as_slice()).unwrap_or(&[]);
        let selected = self.p.selected.t.as_ref().and_then(|o| o.clone());
        if self.p.disabled.t.unwrap_or(false) {
            let placeholder = self.p.placeholder.t.as_deref().unwrap_or("");
            let shown =
                self.p.selected.t.as_ref().and_then(|o| o.as_deref()).unwrap_or("");
            return disabled_choice(placeholder, shown, self.p.width.t.as_ref());
        }
        let on_select_id = self.on_select.id();
        let mut pl =
            widget::PickList::new(
                options,
                selected,
                move |s: String| match on_select_id {
                    Some(id) => {
                        Message::Call(id, ValArray::from_iter([Value::String(s.into())]))
                    }
                    None => Message::Nop,
                },
            );
        let placeholder = self.p.placeholder.t.as_deref().unwrap_or("");
        if !placeholder.is_empty() {
            pl = pl.placeholder(placeholder);
        }
        if let Some(w) = self.p.width.t.as_ref() {
            pl = pl.width(w.0);
        }
        if let Some(p) = self.p.padding.t.as_ref() {
            pl = pl.padding(p.0);
        }
        pl.into()
    }
}
