use super::{
    GuiW, GuiWidget, Handler, IcedElement, Message, MessageShell, call_arg,
    disabled_choice,
};
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
    /// What the user has typed since the last choice, which new options
    /// must not wipe.
    typed: String,
}

impl<X: GXExt> ComboBoxW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let (p, on_select) = try_join!(
            Props::compile(&gx, &source),
            Handler::field(&gx, &source, "on_select"),
        )
        .context("combo_box")?;
        let typed = String::new();
        let state = Self::state(&p, &typed);
        Ok(Box::new(Self { gx, p, on_select, state, typed }))
    }
}

impl<X: GXExt> ComboBoxW<X> {
    fn state(p: &Props<X>, typed: &String) -> combo_box::State<String> {
        combo_box::State::with_selection(
            p.options.t.as_ref().map(|v| v.0.clone()).unwrap_or_default(),
            Some(typed),
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
        let mut changed = self.p.update(id, v).context("combo_box")?;
        if id == self.p.options.r.id
            && let Some(opts) = &self.p.options.t
            && self.state.options() != opts.0.as_slice()
        {
            // XCR claude for claude: [bug/medium] New options rebuild the iced State
            // from the options alone. That resets the text the user is typing (iced
            // keeps it in State's private value), so the input empties and Enter picks
            // the first unfiltered option: after typing "ban" with [apple, banana,
            // cherry], adding "date" makes Enter choose "apple". Track the typed text
            // through ComboBox::on_input as a message to this widget and rebuild with
            // State::with_selection(options, Some(&typed)). probe:
            // design/review-2026-10-19/repro/gui-widgets-a-combo-box-typing-lost.rs
            // (gui-widgets-a-01)
            // 2026-10-19 claude: The combo box now sends Message::ComboText(options
            // ref id, text) from iced's on_input, keeps the text in ComboBoxW::typed
            // (cleared at a choice: its on_select call, or a ComboText with empty text
            // when there is no handler), and rebuilds with
            // State::with_selection(options, Some(&typed)). Pin:
            // interaction_test::combo_box_keeps_typing_across_new_options.
            self.state = Self::state(&self.p, &self.typed);
        }
        self.on_select
            .update(rt, &self.gx, id, v, &mut changed)
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
        let key = self.p.options.r.id;
        let mut cb = widget::ComboBox::new(
            &self.state,
            placeholder,
            selected,
            move |s: String| match on_select_id {
                Some(id) => {
                    Message::Call(id, ValArray::from_iter([Value::String(s.into())]))
                }
                None => Message::ComboText(key, String::new()),
            },
        )
        .on_input(move |s| Message::ComboText(key, s));
        if let Some(w) = self.p.width.t.as_ref() {
            cb = cb.width(w.0);
        }
        cb.into()
    }

    fn on_message(&mut self, msg: &Message, _shell: &mut MessageShell) -> bool {
        match msg {
            Message::ComboText(key, s) if *key == self.p.options.r.id => {
                self.typed.clone_from(s)
            }
            _ if call_arg(msg, self.on_select.id()).is_some() => self.typed.clear(),
            _ => (),
        }
        false
    }
}
