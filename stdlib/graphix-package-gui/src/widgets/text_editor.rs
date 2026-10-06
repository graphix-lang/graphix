use super::{GuiW, GuiWidget, IcedElement, Message, MessageShell};
use crate::types::{FontV, PaddingV};
use anyhow::{Context, Result};
use graphix_compiler::expr::ExprId;
use graphix_rt::{Callable, GXExt, GXHandle, Ref, TRef};
use iced_widget::{self as widget, text_editor};
use netidx::{protocol::valarray::ValArray, publisher::Value};
use netidx_derive::FromValue;
use tokio::try_join;

/// Multi-line text editor widget. Editable when on_edit callback is provided.
pub(crate) struct TextEditorW<X: GXExt> {
    gx: GXHandle<X>,
    disabled: TRef<X, bool>,
    content: text_editor::Content,
    content_ref: TRef<X, String>,
    on_edit: Ref<X>,
    on_edit_callable: Option<Callable<X>>,
    /// Last text pushed via callback; its echo must not rebuild `Content`.
    last_set_text: Option<String>,
    placeholder: TRef<X, String>,
    width: TRef<X, Option<f64>>,
    height: TRef<X, Option<f64>>,
    padding: TRef<X, PaddingV>,
    font: TRef<X, Option<FontV>>,
    size: TRef<X, Option<f64>>,
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
        let on_edit_callable = compile_callable!(gx, on_edit, "text_editor on_edit");
        let content_tref: TRef<X, String> =
            TRef::new(content).context("text_editor tref content")?;
        let initial_text = content_tref.t.as_deref().unwrap_or("");
        let editor_content = text_editor::Content::with_text(initial_text);
        Ok(Box::new(Self {
            gx: gx.clone(),
            disabled: TRef::new(disabled).context("text_editor tref disabled")?,
            content: editor_content,
            content_ref: content_tref,
            on_edit,
            on_edit_callable,
            last_set_text: None,
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
        if let Some(new_text) =
            self.content_ref.update(id, v).context("text_editor update content")?
        {
            // CR claude for eric: [bug] `last_set_text` holds only the last text pushed
            // through on_edit, and the next content update of any kind takes it, so
            // every other update rebuilds `Content` with the cursor at (0,0). With
            // `#on_edit: |s| c <- s`, two keys handled before the first echo arrives
            // (one event-loop batch, or an echo slower than the next key) make that
            // echo rebuild the editor: typing "ab" then "c" gives "cab", and a "c"
            // typed between the two echoes gives "ca", losing the "b". Delivering the
            // unchanged text again also rebuilds: `&doc.text` re-fires on every write
            // to `doc`, so writing another field of `doc` sends the cursor to the
            // start. An on_edit that rewrites the text (`str::to_upper`) types
            // backwards: "abc" becomes "CBA". An update equal to `self.content.text()`,
            // or to a push not yet echoed, should not rebuild, and a real change can
            // keep the cursor with `move_to`. probe:
            // design/review-2026-10-05/repro/gui-widgets-b-04.rs (copy into
            // stdlib/graphix-package-gui/tests/ and run `cargo test -p
            // graphix-package-gui --test review_gui_widgets_b_04`). (gui-widgets-b-04)
            if self.last_set_text.take().as_ref() != Some(new_text) {
                self.content = text_editor::Content::with_text(new_text.as_str());
                changed = true;
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
        update_callable!(
            self,
            rt,
            id,
            v,
            on_edit,
            on_edit_callable,
            "text_editor on_edit recompile"
        );
        Ok(changed)
    }

    fn view(&self) -> IcedElement<'_> {
        let mut te = widget::TextEditor::new(&self.content);
        // CR claude for eric: [bug] `on_edit_callable` is always Some because
        // text_editor.gx defaults #on_edit to `|_| null`, so every enabled editor is
        // editable, against the doc on line 11. Without #on_edit, what the user types
        // changes the local Content but never `content`, and the next `content` update
        // throws it away. A disabled editor gets no on_action, and iced's
        // TextEditor::update returns at once without one (iced_widget 0.14.2
        // text_editor.rs:689). So it cannot scroll, select or copy: in
        // `text_editor(#disabled: &true, #height: &200.0, &long_log)` the text below
        // 200 px is unreachable. A scrollable read-only editor needs on_action
        // installed, with `action.is_edit()` actions dropped in on_message; iced then
        // no longer draws it as disabled. (gui-widgets-b-11)
        if !self.disabled.t.unwrap_or(false) && self.on_edit_callable.is_some() {
            let content_id = self.content_ref.r.id;
            te = te.on_action(move |a| Message::EditorAction(content_id, a));
        }
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
        if let Some(Some(sz)) = self.size.t {
            te = te.size(sz as f32);
        }
        te.into()
    }

    fn on_message(&mut self, msg: &Message, shell: &mut MessageShell) -> bool {
        match msg {
            Message::EditorAction(id, action) => {
                if *id != self.content_ref.r.id {
                    return false;
                }
                self.content.perform(action.clone());
                if action.is_edit() {
                    if let Some(callable) = &self.on_edit_callable {
                        let text = self.content.text();
                        self.last_set_text = Some(text.clone());
                        shell.publish(Message::Call(
                            callable.id(),
                            ValArray::from_iter([Value::String(text.into())]),
                        ));
                    }
                }
                true
            }
            Message::Nop
            | Message::Call(..)
            | Message::Scroll(..)
            | Message::CellClick(..)
            | Message::CellEdit(..)
            | Message::CellEditInput(..)
            | Message::CellEditSubmit
            | Message::CellEditCancel
            | Message::ColumnResizeStart(..)
            | Message::ColumnResizeMove(..)
            | Message::ColumnResizeEnd
            | Message::TableKey(..) => false,
        }
    }
}
