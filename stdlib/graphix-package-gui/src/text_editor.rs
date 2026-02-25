use crate::{
    types::{FontV, PaddingV},
    GuiW, GuiWidget, IcedElement, Message,
};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use graphix_compiler::{expr::ExprId, BindId};
use graphix_rt::{GXExt, GXHandle, TRef};
use iced_widget::{self as widget, text_editor};
use netidx::publisher::Value;
use tokio::try_join;

/// Multi-line text editor widget. Editable when the content ref
/// points to a mutable `&string` variable.
pub(crate) struct TextEditorW<X: GXExt> {
    content: text_editor::Content,
    content_ref: TRef<X, String>,
    /// BindId of the target variable when content is `&string`.
    target_bid: Option<BindId>,
    /// Last text we pushed to graphix, used to suppress the echo
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
        let [(_, content), (_, font), (_, height), (_, padding), (_, placeholder), (_, size), (_, width)] =
            source.cast_to::<[(ArcStr, u64); 7]>().context("text_editor flds")?;
        let (content, font, height, padding, placeholder, size, width) = try_join! {
            gx.compile_ref(content),
            gx.compile_ref(font),
            gx.compile_ref(height),
            gx.compile_ref(padding),
            gx.compile_ref(placeholder),
            gx.compile_ref(size),
            gx.compile_ref(width),
        }?;
        let target_bid = content.target_bid;
        let content_tref: TRef<X, String> =
            TRef::new(content).context("text_editor tref content")?;
        let initial_text = content_tref.t.as_deref().unwrap_or("");
        let editor_content = text_editor::Content::with_text(initial_text);
        Ok(Box::new(Self {
            content: editor_content,
            content_ref: content_tref,
            target_bid,
            last_set_text: None,
            placeholder: TRef::new(placeholder).context("text_editor tref placeholder")?,
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
        _rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let mut changed = false;
        if let Some(new_text) = self.content_ref.update(id, v)
            .context("text_editor update content")?
        {
            // If this is the echo of text we just pushed, skip the
            // destructive Content rebuild to preserve cursor/selection/undo.
            if self.last_set_text.take().as_ref() != Some(new_text) {
                self.content = text_editor::Content::with_text(new_text.as_str());
                changed = true;
            }
        }
        changed |= self.placeholder.update(id, v).context("text_editor update placeholder")?.is_some();
        changed |= self.width.update(id, v).context("text_editor update width")?.is_some();
        changed |= self.height.update(id, v).context("text_editor update height")?.is_some();
        changed |= self.padding.update(id, v).context("text_editor update padding")?.is_some();
        changed |= self.font.update(id, v).context("text_editor update font")?.is_some();
        changed |= self.size.update(id, v).context("text_editor update size")?.is_some();
        Ok(changed)
    }

    fn view(&self) -> IcedElement<'_> {
        let mut te = widget::TextEditor::new(&self.content);
        if self.target_bid.is_some() {
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
    ) -> Option<(BindId, Value)> {
        if id != self.content_ref.r.id {
            return None;
        }
        self.content.perform(action.clone());
        if action.is_edit() {
            if let Some(bid) = self.target_bid {
                let text = self.content.text();
                self.last_set_text = Some(text.clone());
                return Some((bid, Value::String(text.into())));
            }
        }
        None
    }
}
