use super::{
    Echoes, GuiW, GuiWidget, Handler, IcedElement, Message, MessageShell, call_arg,
};
use crate::types::{FontV, PaddingV, TextSizeV};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use futures::try_join;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle};
use iced_core::text::editor::{Cursor, Position};
use iced_widget::{self as widget, text_editor};
use netidx::{protocol::valarray::ValArray, publisher::Value};

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
    echoes: Echoes<ArcStr>,
    sync_content: bool,
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
        Ok(Box::new(Self {
            gx,
            p,
            on_edit,
            editor,
            echoes: Echoes::new(),
            sync_content: false,
        }))
    }
}

impl<X: GXExt> GuiWidget<X> for TextEditorW<X> {
    fn handle_update(
        &mut self,
        rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let mut changed = self.p.update(id, v).context("text_editor")?;
        let mut sync = self.echoes.answered(id);
        if id == self.p.content.r.id
            && let Some(text) = &self.p.content.t
        {
            sync |= !self.echoes.delivered(text);
        }
        self.on_edit
            .update(rt, &self.gx, id, v, &mut changed)
            .context("text_editor on_edit recompile")?;
        sync |= self.echoes.retain_handler(&self.on_edit);
        self.sync_content |= sync;
        Ok(changed || sync)
    }

    fn before_view(&mut self) -> bool {
        if !std::mem::take(&mut self.sync_content) {
            return false;
        }
        let text =
            self.echoes.shown(self.p.content.t.as_ref()).map_or("", |v| v.as_str());
        if text == self.editor.text() {
            return false;
        }
        let cursor = self.editor.cursor();
        self.editor = text_editor::Content::with_text(text);
        self.editor.move_to(clamp_cursor(&self.editor, cursor));
        true
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
                        shell.publish(Message::Call(
                            cid,
                            ValArray::from_iter([Value::String(text)]),
                        ));
                        true
                    }
                }
            }
            Message::Call(..) => match call_arg(msg, self.on_edit.id()) {
                Some(Value::String(text)) => {
                    self.echoes.send(text.clone(), &self.on_edit, msg, shell)
                }
                _ => false,
            },
            Message::Nop | Message::Table(..) => false,
        }
    }
}

fn clamp_cursor(editor: &text_editor::Content, mut cursor: Cursor) -> Cursor {
    let clamp = |position: Position| {
        let line = position.line.min(editor.line_count().saturating_sub(1));
        let text = editor.line(line).unwrap_or_default().text;
        let mut column = position.column.min(text.len());
        while !text.is_char_boundary(column) {
            column -= 1;
        }
        Position { line, column }
    };
    let position = clamp(cursor.position);
    cursor.selection =
        cursor.selection.filter(|s| position == cursor.position && clamp(*s) == *s);
    cursor.position = position;
    cursor
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rewritten_editor_clamps_utf8_and_selection() {
        let editor = text_editor::Content::with_text("a\né");
        let pos = |line, column| Position { line, column };
        for (cursor, expected) in [
            (
                Cursor { position: pos(5, 8), selection: Some(pos(0, 0)) },
                Cursor { position: pos(1, 2), selection: None },
            ),
            (
                Cursor { position: pos(1, 1), selection: None },
                Cursor { position: pos(1, 0), selection: None },
            ),
            (
                Cursor { position: pos(0, 1), selection: Some(pos(1, 1)) },
                Cursor { position: pos(0, 1), selection: None },
            ),
            (
                Cursor { position: pos(0, 1), selection: Some(pos(1, 2)) },
                Cursor { position: pos(0, 1), selection: Some(pos(1, 2)) },
            ),
        ] {
            assert_eq!(clamp_cursor(&editor, cursor), expected);
        }
        let editor = text_editor::Content::new();
        assert_eq!(
            clamp_cursor(&editor, Cursor { position: pos(2, 3), selection: None })
                .position,
            pos(0, 0)
        );
    }
}
