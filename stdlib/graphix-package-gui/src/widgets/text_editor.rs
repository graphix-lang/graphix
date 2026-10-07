use super::{GuiW, GuiWidget, Handler, IcedElement, Message, MessageShell};
use crate::types::{FontV, PaddingV, TextSizeV};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle, TRef};
use iced_widget::{self as widget, text_editor};
use netidx::{protocol::valarray::ValArray, publisher::Value};
use netidx_derive::FromValue;
use std::collections::VecDeque;
use tokio::try_join;

/// Multi-line text editor widget, editable when it has an on_edit and is not
/// disabled; otherwise it still scrolls, selects and copies.
pub(crate) struct TextEditorW<X: GXExt> {
    gx: GXHandle<X>,
    disabled: TRef<X, bool>,
    content: text_editor::Content,
    content_ref: TRef<X, ArcStr>,
    on_edit: Handler<X>,
    /// The texts sent through on_edit whose echoes have not come back, in
    /// order: an echo is the editor's own text and rebuilds nothing.
    pending: VecDeque<ArcStr>,
    placeholder: TRef<X, ArcStr>,
    width: TRef<X, Option<f64>>,
    height: TRef<X, Option<f64>>,
    padding: TRef<X, PaddingV>,
    font: TRef<X, Option<FontV>>,
    size: TRef<X, Option<TextSizeV>>,
}

impl<X: GXExt> TextEditorW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        #[derive(FromValue)]
        struct Fields {
            content: u64,
            disabled: u64,
            font: u64,
            height: u64,
            on_edit: u64,
            padding: u64,
            placeholder: u64,
            size: u64,
            width: u64,
        }
        let Fields {
            content,
            disabled,
            font,
            height,
            on_edit,
            padding,
            placeholder,
            size,
            width,
        } = source.cast_to().context("text_editor flds")?;
        let (content, disabled, font, height, on_edit, padding, placeholder, size, width) =
            try_join! {
                gx.compile_ref(content),
                gx.compile_ref(disabled),
                gx.compile_ref(font),
                gx.compile_ref(height),
                gx.compile_ref(on_edit),
                gx.compile_ref(padding),
                gx.compile_ref(placeholder),
                gx.compile_ref(size),
                gx.compile_ref(width),
            }?;
        let on_edit =
            Handler::compile(&gx, on_edit).await.context("text_editor on_edit")?;
        let content_tref: TRef<X, ArcStr> =
            TRef::new(content).context("text_editor tref content")?;
        let initial_text = content_tref.t.as_deref().unwrap_or("");
        let editor_content = text_editor::Content::with_text(initial_text);
        Ok(Box::new(Self {
            gx: gx.clone(),
            disabled: TRef::new(disabled).context("text_editor tref disabled")?,
            content: editor_content,
            content_ref: content_tref,
            on_edit,
            pending: VecDeque::new(),
            placeholder: TRef::new(placeholder)
                .context("text_editor tref placeholder")?,
            width: TRef::new(width).context("text_editor tref width")?,
            height: TRef::new(height).context("text_editor tref height")?,
            padding: TRef::new(padding).context("text_editor tref padding")?,
            font: TRef::new(font).context("text_editor tref font")?,
            size: TRef::new(size).context("text_editor tref size")?,
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
        let mut changed = false;
        changed |=
            self.disabled.update(id, v).context("text_editor update disabled")?.is_some();
        if let Some(text) =
            self.content_ref.update(id, v).context("text_editor update content")?
        {
            match self.pending.iter().position(|p| p == text) {
                // an echo: what it answered and every earlier send are done
                Some(i) => drop(self.pending.drain(..=i)),
                None if *text == *self.content.text() => (),
                None => {
                    let cursor = self.content.cursor();
                    self.content = text_editor::Content::with_text(text);
                    self.content.move_to(cursor);
                    self.pending.clear();
                    changed = true;
                }
            }
        }
        changed |= self
            .placeholder
            .update(id, v)
            .context("text_editor update placeholder")?
            .is_some();
        changed |=
            self.width.update(id, v).context("text_editor update width")?.is_some();
        changed |=
            self.height.update(id, v).context("text_editor update height")?.is_some();
        changed |=
            self.padding.update(id, v).context("text_editor update padding")?.is_some();
        changed |= self.font.update(id, v).context("text_editor update font")?.is_some();
        changed |= self.size.update(id, v).context("text_editor update size")?.is_some();
        self.on_edit
            .update(rt, &self.gx, id, v)
            .context("text_editor on_edit recompile")?;
        Ok(changed)
    }

    fn view(&self) -> IcedElement<'_> {
        let mut te = widget::TextEditor::new(&self.content);
        let content_id = self.content_ref.r.id;
        te = te.on_action(move |a| Message::EditorAction(content_id, a));
        let placeholder = self.placeholder.t.as_deref().unwrap_or("");
        if !placeholder.is_empty() {
            te = te.placeholder(placeholder);
        }
        if let Some(Some(w)) = self.width.t {
            te = te.width(w as f32);
        }
        if let Some(Some(h)) = self.height.t {
            te = te.height(h as f32);
        }
        if let Some(p) = self.padding.t.as_ref() {
            te = te.padding(p.0);
        }
        if let Some(Some(f)) = self.font.t.as_ref() {
            te = te.font(f.0);
        }
        if let Some(Some(TextSizeV(sz))) = self.size.t {
            te = te.size(sz);
        }
        te.into()
    }

    fn on_message(&mut self, msg: &Message, shell: &mut MessageShell) -> bool {
        match msg {
            Message::EditorAction(id, action) => {
                if *id != self.content_ref.r.id {
                    return false;
                }
                if !action.is_edit() {
                    self.content.perform(action.clone());
                    return true;
                }
                let editable = !self.disabled.t.unwrap_or(false);
                match self.on_edit.id().filter(|_| editable) {
                    None => false,
                    Some(cid) => {
                        self.content.perform(action.clone());
                        let text = ArcStr::from(self.content.text());
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
