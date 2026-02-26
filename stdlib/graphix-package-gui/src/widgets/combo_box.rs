use super::{GuiW, GuiWidget, IcedElement, Message};
use crate::types::{LengthV, StringVec};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use graphix_compiler::{expr::ExprId, BindId};
use graphix_rt::{GXExt, GXHandle, TRef};
use iced_widget::{self as widget, combo_box};
use netidx::publisher::Value;
use tokio::try_join;

pub(crate) struct ComboBoxW<X: GXExt> {
    options: TRef<X, StringVec>,
    state: combo_box::State<String>,
    selected: TRef<X, Option<String>>,
    target_bid: Option<BindId>,
    placeholder: TRef<X, String>,
    width: TRef<X, LengthV>,
}

impl<X: GXExt> ComboBoxW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let [(_, options), (_, placeholder), (_, selected), (_, width)] =
            source.cast_to::<[(ArcStr, u64); 4]>().context("combo_box flds")?;
        let (options, placeholder, selected, width) = try_join! {
            gx.compile_ref(options),
            gx.compile_ref(placeholder),
            gx.compile_ref(selected),
            gx.compile_ref(width),
        }?;
        let target_bid = selected.target_bid;
        let options_tref: TRef<X, StringVec> =
            TRef::new(options).context("combo_box tref options")?;
        let state = combo_box::State::new(
            options_tref.t.as_ref().map(|v| v.0.clone()).unwrap_or_default(),
        );
        Ok(Box::new(Self {
            options: options_tref,
            state,
            selected: TRef::new(selected).context("combo_box tref selected")?,
            target_bid,
            placeholder: TRef::new(placeholder).context("combo_box tref placeholder")?,
            width: TRef::new(width).context("combo_box tref width")?,
        }))
    }
}

impl<X: GXExt> GuiWidget<X> for ComboBoxW<X> {
    fn handle_update(
        &mut self,
        _rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let mut changed = false;
        if let Some(opts) = self.options.update(id, v).context("combo_box update options")? {
            self.state = combo_box::State::new(opts.0.clone());
            changed = true;
        }
        changed |= self.selected.update(id, v).context("combo_box update selected")?.is_some();
        changed |= self.placeholder.update(id, v).context("combo_box update placeholder")?.is_some();
        changed |= self.width.update(id, v).context("combo_box update width")?.is_some();
        Ok(changed)
    }

    fn view(&self) -> IcedElement<'_> {
        let selected = self.selected.t.as_ref().and_then(|o| o.as_ref());
        let placeholder = self.placeholder.t.as_deref().unwrap_or("");
        let target_bid = self.target_bid;
        let mut cb = widget::ComboBox::new(
            &self.state,
            placeholder,
            selected,
            move |s: String| match target_bid {
                Some(bid) => Message::Set(bid, Value::String(s.into())),
                None => Message::Nop,
            },
        );
        if let Some(w) = self.width.t.as_ref() {
            cb = cb.width(w.0);
        }
        cb.into()
    }
}
