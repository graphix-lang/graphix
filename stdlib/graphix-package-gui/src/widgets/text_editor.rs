use super::{GuiW, GuiWidget, Handler, IcedElement, Message, MessageShell};
use crate::types::{FontV, PaddingV, TextSizeV};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use futures::try_join;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle};
use iced_widget::{self as widget, text_editor};
use netidx::{protocol::valarray::ValArray, publisher::Value};
use std::collections::VecDeque;

graphix_rt::props! {
    struct Props {
        content: ArcStr,
        disabled: bool,
        font: Option<FontV>,
        height: Option<f64>,
        padding: PaddingV,
        placeholder: ArcStr,
        size: Option<TextSizeV>,
        width: Option<f64>,
    }
}

/// Multi-line text editor widget, editable when it has an on_edit and is not
/// disabled; otherwise it still scrolls, selects and copies.
pub(crate) struct TextEditorW<X: GXExt> {
    gx: GXHandle<X>,
    p: Props<X>,
    on_edit: Handler<X>,
    editor: text_editor::Content,
    /// The texts sent through on_edit whose echoes have not come back, in
    /// order: an echo is the editor's own text and rebuilds nothing.
    pending: VecDeque<ArcStr>,
}

impl<X: GXExt> TextEditorW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let (p, on_edit) = try_join!(
            Props::compile(&gx, &source),
            Handler::field(&gx, &source, "on_edit"),
        )
        .context("text_editor")?;
        let editor =
            text_editor::Content::with_text(p.content.t.as_deref().unwrap_or(""));
        Ok(Box::new(Self { gx, p, on_edit, editor, pending: VecDeque::new() }))
    }
}

impl<X: GXExt> GuiWidget<X> for TextEditorW<X> {
    fn handle_update(
        &mut self,
        rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let changed = self.p.update(id, v).context("text_editor")?;
        if id == self.p.content.r.id
            && let Some(text) = &self.p.content.t
        {
            match self.pending.iter().position(|p| p == text) {
                // an echo: what it answered and every earlier send are done
                Some(i) => drop(self.pending.drain(..=i)),
                None if *text == *self.editor.text() => (),
                None => {
                    let cursor = self.editor.cursor();
                    self.editor = text_editor::Content::with_text(text);
                    self.editor.move_to(cursor);
                    self.pending.clear();
                }
            }
        }
        self.on_edit
            .update(rt, &self.gx, id, v)
            .context("text_editor on_edit recompile")?;
        Ok(changed)
    }

    fn view(&self) -> IcedElement<'_> {
        let mut te = widget::TextEditor::new(&self.editor);
        let content_id = self.p.content.r.id;
        te = te.on_action(move |a| Message::EditorAction(content_id, a));
        let placeholder = self.p.placeholder.t.as_deref().unwrap_or("");
        if !placeholder.is_empty() {
            te = te.placeholder(placeholder);
        }
        if let Some(Some(w)) = self.p.width.t {
            te = te.width(w as f32);
        }
        if let Some(Some(h)) = self.p.height.t {
            te = te.height(h as f32);
        }
        if let Some(p) = self.p.padding.t.as_ref() {
            te = te.padding(p.0);
        }
        if let Some(Some(f)) = self.p.font.t.as_ref() {
            te = te.font(f.0);
        }
        if let Some(Some(TextSizeV(sz))) = self.p.size.t {
            te = te.size(sz);
        }
        te.into()
    }

    fn on_message(&mut self, msg: &Message, shell: &mut MessageShell) -> bool {
        match msg {
            Message::EditorAction(id, action) => {
                if *id != self.p.content.r.id {
                    return false;
                }
                if !action.is_edit() {
                    self.editor.perform(action.clone());
                    return true;
                }
                let editable = !self.p.disabled.t.unwrap_or(false);
                match self.on_edit.id().filter(|_| editable) {
                    None => false,
                    Some(cid) => {
                        self.editor.perform(action.clone());
                        let text = ArcStr::from(self.editor.text());
                        self.pending.push_back(text.clone());
                        shell.publish(Message::Call(
                            cid,
                            ValArray::from_iter([Value::String(text)]),
                        ));
                        true
                    }
                }
            }
            Message::Nop | Message::Call(..) | Message::Table(..) => false,
        }
    }
}
