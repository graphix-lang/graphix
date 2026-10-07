use super::{GuiW, GuiWidget, Handler, IcedElement, Message, disabled_choice};
use crate::types::{LengthV, StringVec};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use futures::try_join;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle};
use iced_widget::{self as widget, combo_box};
use netidx::{protocol::valarray::ValArray, publisher::Value};

graphix_rt::props! {
    struct Props {
        disabled: bool,
        options: StringVec,
        placeholder: ArcStr,
        selected: Option<String>,
        width: LengthV,
    }
}

pub(crate) struct ComboBoxW<X: GXExt> {
    gx: GXHandle<X>,
    p: Props<X>,
    on_select: Handler<X>,
    state: combo_box::State<String>,
}

impl<X: GXExt> ComboBoxW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let (p, on_select) = try_join!(
            Props::compile(&gx, &source),
            Handler::field(&gx, &source, "on_select"),
        )
        .context("combo_box")?;
        let state = Self::state(&p);
        Ok(Box::new(Self { gx, p, on_select, state }))
    }
}

impl<X: GXExt> ComboBoxW<X> {
    fn state(p: &Props<X>) -> combo_box::State<String> {
        combo_box::State::new(
            p.options.t.as_ref().map(|v| v.0.clone()).unwrap_or_default(),
        )
    }
}

impl<X: GXExt> GuiWidget<X> for ComboBoxW<X> {
    fn handle_update(
        &mut self,
        rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let changed = self.p.update(id, v).context("combo_box")?;
        if id == self.p.options.r.id
            && let Some(opts) = &self.p.options.t
            && self.state.options() != opts.0.as_slice()
        {
            self.state = Self::state(&self.p);
        }
        self.on_select
            .update(rt, &self.gx, id, v)
            .context("combo_box on_select recompile")?;
        Ok(changed)
    }

    fn view(&self) -> IcedElement<'_> {
        let selected = self.p.selected.t.as_ref().and_then(|o| o.as_ref());
        let placeholder = self.p.placeholder.t.as_deref().unwrap_or("");
        if self.p.disabled.t.unwrap_or(false) {
            return disabled_choice(
                placeholder,
                selected.map_or("", |s| s.as_str()),
                self.p.width.t.as_ref(),
            );
        }
        let on_select_id = self.on_select.id();
        let mut cb = widget::ComboBox::new(
            &self.state,
            placeholder,
            selected,
            move |s: String| match on_select_id {
                Some(id) => {
                    Message::Call(id, ValArray::from_iter([Value::String(s.into())]))
                }
                None => Message::Nop,
            },
        );
        if let Some(w) = self.p.width.t.as_ref() {
            cb = cb.width(w.0);
        }
        cb.into()
    }
}
