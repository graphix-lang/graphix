use super::{GuiW, GuiWidget, IcedElement, Message};
use crate::types::{FontV, PaddingV};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use graphix_compiler::expr::ExprId;
use graphix_rt::{Callable, CallableId, GXExt, GXHandle, Ref, TRef};
use iced_widget::{self as widget, text_editor};
use netidx::publisher::Value;
use tokio::try_join;

/// Multi-line text editor widget. Editable when on_edit callback is provided.
pub(crate) struct TextEditorW<X: GXExt> {
    gx: GXHandle<X>,
    disabled: TRef<X, bool>,
    content: text_editor::Content,
    content_ref: TRef<X, String>,
    on_edit: Ref<X>,
    on_edit_callable: Option<Callable<X>>,
    /// Last text we pushed via callback, used to suppress the echo
    /// in handle_update so we don't destroy cursor/selection/undo state.
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
        let [(_, content), (_, disabled), (_, font), (_, height), (_, on_edit), (_, padding), (_, placeholder), (_, size), (_, width)] =
            source.cast_to::<[(ArcStr, u64); 9]>().context("text_editor flds")?;
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
        let on_edit_callable = match on_edit.last.as_ref() {
            Some(v) => Some(
                gx.compile_callable(v.clone())
                    .await
                    .context("text_editor on_edit callable")?,
            ),
            None => None,
        };
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
            // If this is the echo of text we just pushed, skip the
            // destructive Content rebuild to preserve cursor/selection/undo.
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
        if id == self.on_edit.id {
            self.on_edit.last = Some(v.clone());
            self.on_edit_callable = Some(
                rt.block_on(self.gx.compile_callable(v.clone()))
                    .context("text_editor on_edit callable recompile")?,
            );
        }
        Ok(changed)
    }

    fn view(&self) -> IcedElement<'_> {
        let mut te = widget::TextEditor::new(&self.content);
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

    fn editor_action(
        &mut self,
        id: ExprId,
        action: &text_editor::Action,
    ) -> Option<(CallableId, Value)> {
        if id != self.content_ref.r.id {
            return None;
        }
        self.content.perform(action.clone());
        if action.is_edit() {
            if let Some(callable) = &self.on_edit_callable {
                let text = self.content.text();
                self.last_set_text = Some(text.clone());
                return Some((callable.id(), Value::String(text.into())));
            }
        }
        None
    }
}
