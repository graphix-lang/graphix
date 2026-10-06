use super::{GuiW, GuiWidget, IcedElement, Message};
use crate::types::{LengthV, StringVec};
use anyhow::{Context, Result};
use graphix_compiler::expr::ExprId;
use graphix_rt::{Callable, GXExt, GXHandle, Ref, TRef};
use iced_widget::{self as widget, combo_box};
use netidx::{protocol::valarray::ValArray, publisher::Value};
use netidx_derive::FromValue;
use tokio::try_join;

pub(crate) struct ComboBoxW<X: GXExt> {
    gx: GXHandle<X>,
    disabled: TRef<X, bool>,
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
        #[derive(FromValue)]
        struct Fields {
            disabled: u64,
            on_select: u64,
            options: u64,
            placeholder: u64,
            selected: u64,
            width: u64,
        }
        let Fields { disabled, on_select, options, placeholder, selected, width } =
            source.cast_to().context("combo_box flds")?;
        let (disabled, on_select, options, placeholder, selected, width) = try_join! {
            gx.compile_ref(disabled),
            gx.compile_ref(on_select),
            gx.compile_ref(options),
            gx.compile_ref(placeholder),
            gx.compile_ref(selected),
            gx.compile_ref(width),
        }?;
        let callable = compile_callable!(gx, on_select, "combo_box on_select");
        let options_tref: TRef<X, StringVec> =
            TRef::new(options).context("combo_box tref options")?;
        let state = combo_box::State::new(
            options_tref.t.as_ref().map(|v| v.0.clone()).unwrap_or_default(),
        );
        Ok(Box::new(Self {
            gx: gx.clone(),
            disabled: TRef::new(disabled).context("combo_box tref disabled")?,
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
        changed |=
            self.disabled.update(id, v).context("combo_box update disabled")?.is_some();
        if let Some(opts) =
            self.options.update(id, v).context("combo_box update options")?
        {
            // CR claude for claude: [bug] Every fire of `options` replaces the iced
            // State, which holds the text being typed and the filtered list. A re-fire
            // with unchanged contents (from a timer, a recomputed array or a struct
            // field that re-fires) therefore erases what the user is typing and resets
            // the dropdown. Rebuild only when the new options differ from
            // self.state.options(). The TRef's copy of the Vec is then never read
            // (view() uses only the State), so a plain Ref will do. probe:
            // design/review-2026-10-05/repro/gui-widgets-a-11.rs (after typing "ban"
            // and three same-value fires of options, Enter selects "apple" instead of
            // "banana"). (gui-widgets-a-11)
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
        update_callable!(
            self,
            rt,
            id,
            v,
            on_select,
            on_select_callable,
            "combo_box on_select recompile"
        );
        Ok(changed)
    }

    fn view(&self) -> IcedElement<'_> {
        let selected = self.selected.t.as_ref().and_then(|o| o.as_ref());
        let placeholder = self.placeholder.t.as_deref().unwrap_or("");
        // CR claude for claude: [doc-drift] combo_box.md says a disabled combo box cannot
        // be interacted with, but this code only turns the selection into Message::Nop.
        // iced's ComboBox always wires on_input, so a disabled box still takes focus
        // and typing, opens its list and silently drops the pick, with no disabled
        // styling. When disabled, either render a text_input without on_input (iced
        // draws it as disabled) showing the selection, or correct the book.
        // pick_list.rs:105 has the same pattern, against pick_list.md's "the dropdown
        // cannot be opened". probe: design/review-2026-10-05/repro/gui-widgets-a-11.rs
        // (disabled case: all 3 keys captured, Enter publishes Nop). (gui-widgets-a-13)
        let on_select_id = if self.disabled.t.unwrap_or(false) {
            None
        } else {
            self.on_select_callable.as_ref().map(|c| c.id())
        };
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
