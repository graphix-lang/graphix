use super::{GuiW, GuiWidget, IcedElement, Message};
use crate::types::{FontV, LengthV, PaddingV};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use graphix_compiler::expr::ExprId;
use graphix_rt::{Callable, GXExt, GXHandle, Ref, TRef};
use iced_widget as widget;
use netidx::{protocol::valarray::ValArray, publisher::Value};
use tokio::try_join;

pub(crate) struct TextInputW<X: GXExt> {
    gx: GXHandle<X>,
    disabled: TRef<X, bool>,
    value: TRef<X, String>,
    placeholder: TRef<X, String>,
    on_input: Ref<X>,
    on_input_callable: Option<Callable<X>>,
    on_submit: Ref<X>,
    on_submit_callable: Option<Callable<X>>,
    is_secure: TRef<X, bool>,
    width: TRef<X, LengthV>,
    padding: TRef<X, PaddingV>,
    size: TRef<X, Option<f64>>,
    font: TRef<X, Option<FontV>>,
}

impl<X: GXExt> TextInputW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let [(_, disabled), (_, font), (_, is_secure), (_, on_input), (_, on_submit), (_, padding), (_, placeholder), (_, size), (_, value), (_, width)] =
            source.cast_to::<[(ArcStr, u64); 10]>().context("text_input flds")?;
        let (
            disabled,
            font,
            is_secure,
            on_input,
            on_submit,
            padding,
            placeholder,
            size,
            value,
            width,
        ) = try_join! {
            gx.compile_ref(disabled),
            gx.compile_ref(font),
            gx.compile_ref(is_secure),
            gx.compile_ref(on_input),
            gx.compile_ref(on_submit),
            gx.compile_ref(padding),
            gx.compile_ref(placeholder),
            gx.compile_ref(size),
            gx.compile_ref(value),
            gx.compile_ref(width),
        }?;
        let on_input_callable =
            compile_callable!(gx, on_input, "text_input on_input");
        let on_submit_callable =
            compile_callable!(gx, on_submit, "text_input on_submit");
        Ok(Box::new(Self {
            gx: gx.clone(),
            disabled: TRef::new(disabled).context("text_input tref disabled")?,
            value: TRef::new(value).context("text_input tref value")?,
            placeholder: TRef::new(placeholder).context("text_input tref placeholder")?,
            on_input,
            on_input_callable,
            on_submit,
            on_submit_callable,
            is_secure: TRef::new(is_secure).context("text_input tref is_secure")?,
            width: TRef::new(width).context("text_input tref width")?,
            padding: TRef::new(padding).context("text_input tref padding")?,
            size: TRef::new(size).context("text_input tref size")?,
            font: TRef::new(font).context("text_input tref font")?,
        }))
    }
}

impl<X: GXExt> GuiWidget<X> for TextInputW<X> {
    fn handle_update(
        &mut self,
        rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let mut changed = false;
        changed |=
            self.disabled.update(id, v).context("text_input update disabled")?.is_some();
        changed |= self.value.update(id, v).context("text_input update value")?.is_some();
        changed |= self
            .placeholder
            .update(id, v)
            .context("text_input update placeholder")?
            .is_some();
        changed |= self
            .is_secure
            .update(id, v)
            .context("text_input update is_secure")?
            .is_some();
        changed |= self.width.update(id, v).context("text_input update width")?.is_some();
        changed |=
            self.padding.update(id, v).context("text_input update padding")?.is_some();
        changed |= self.size.update(id, v).context("text_input update size")?.is_some();
        changed |= self.font.update(id, v).context("text_input update font")?.is_some();
        update_callable!(self, rt, id, v, on_input, on_input_callable, "text_input on_input recompile");
        update_callable!(self, rt, id, v, on_submit, on_submit_callable, "text_input on_submit recompile");
        Ok(changed)
    }

    fn view(&self) -> IcedElement<'_> {
        let val = self.value.t.as_deref().unwrap_or("");
        let placeholder = self.placeholder.t.as_deref().unwrap_or("");
        let mut ti = widget::TextInput::new(placeholder, val);
        if !self.disabled.t.unwrap_or(false) {
            if let Some(callable) = &self.on_input_callable {
                let id = callable.id();
                ti = ti.on_input(move |s| {
                    Message::Call(id, ValArray::from_iter([Value::String(s.into())]))
                });
            }
            if let Some(callable) = &self.on_submit_callable {
                ti = ti.on_submit(Message::Call(
                    callable.id(),
                    ValArray::from_iter([Value::Null]),
                ));
            }
        }
        if self.is_secure.t == Some(true) {
            ti = ti.secure(true);
        }
        if let Some(w) = self.width.t.as_ref() {
            ti = ti.width(w.0);
        }
        if let Some(p) = self.padding.t.as_ref() {
            ti = ti.padding(p.0);
        }
        if let Some(Some(sz)) = self.size.t {
            ti = ti.size(sz as f32);
        }
        if let Some(Some(f)) = self.font.t.as_ref() {
            ti = ti.font(f.0);
        }
        ti.into()
    }
}
