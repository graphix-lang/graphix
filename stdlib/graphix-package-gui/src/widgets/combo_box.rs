use super::{GuiW, GuiWidget, IcedElement, Message};
use crate::types::{LengthV, StringVec};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use graphix_compiler::expr::ExprId;
use graphix_rt::{Callable, GXExt, GXHandle, Ref, TRef};
use iced_widget::{self as widget, combo_box};
use netidx::{protocol::valarray::ValArray, publisher::Value};
use tokio::try_join;

pub(crate) struct ComboBoxW<X: GXExt> {
    gx: GXHandle<X>,
    options: TRef<X, StringVec>,
    state: combo_box::State<String>,
    selected: TRef<X, Option<String>>,
    on_select: Ref<X>,
    on_select_callable: Option<Callable<X>>,
    placeholder: TRef<X, String>,
    width: TRef<X, LengthV>,
}

impl<X: GXExt> ComboBoxW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let [(_, on_select), (_, options), (_, placeholder), (_, selected), (_, width)] =
            source.cast_to::<[(ArcStr, u64); 5]>().context("combo_box flds")?;
        let (on_select, options, placeholder, selected, width) = try_join! {
            gx.compile_ref(on_select),
            gx.compile_ref(options),
            gx.compile_ref(placeholder),
            gx.compile_ref(selected),
            gx.compile_ref(width),
        }?;
        let callable = match on_select.last.as_ref() {
            Some(v) => Some(
                gx.compile_callable(v.clone())
                    .await
                    .context("combo_box on_select callable")?,
            ),
            None => None,
        };
        let options_tref: TRef<X, StringVec> =
            TRef::new(options).context("combo_box tref options")?;
        let state = combo_box::State::new(
            options_tref.t.as_ref().map(|v| v.0.clone()).unwrap_or_default(),
        );
        Ok(Box::new(Self {
            gx: gx.clone(),
            options: options_tref,
            state,
            selected: TRef::new(selected).context("combo_box tref selected")?,
            on_select,
            on_select_callable: callable,
            placeholder: TRef::new(placeholder).context("combo_box tref placeholder")?,
            width: TRef::new(width).context("combo_box tref width")?,
        }))
    }
}

impl<X: GXExt> GuiWidget<X> for ComboBoxW<X> {
    fn handle_update(
        &mut self,
        rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let mut changed = false;
        if let Some(opts) =
            self.options.update(id, v).context("combo_box update options")?
        {
            self.state = combo_box::State::new(opts.0.clone());
            changed = true;
        }
        changed |=
            self.selected.update(id, v).context("combo_box update selected")?.is_some();
        changed |= self
            .placeholder
            .update(id, v)
            .context("combo_box update placeholder")?
            .is_some();
        changed |= self.width.update(id, v).context("combo_box update width")?.is_some();
        if id == self.on_select.id {
            self.on_select.last = Some(v.clone());
            self.on_select_callable = Some(
                rt.block_on(self.gx.compile_callable(v.clone()))
                    .context("combo_box on_select callable recompile")?,
            );
        }
        Ok(changed)
    }

    fn view(&self) -> IcedElement<'_> {
        let selected = self.selected.t.as_ref().and_then(|o| o.as_ref());
        let placeholder = self.placeholder.t.as_deref().unwrap_or("");
        let on_select_id = self.on_select_callable.as_ref().map(|c| c.id());
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
        if let Some(w) = self.width.t.as_ref() {
            cb = cb.width(w.0);
        }
        cb.into()
    }
}
